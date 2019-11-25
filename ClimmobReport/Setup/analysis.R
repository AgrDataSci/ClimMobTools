####Analysis####


#Table 1 - Variety Summary
vartable<-data.frame(table(c(dataset$variety_a,dataset$variety_b,dataset$variety_c)))
colnames(vartable)<-c("Variety","Frequency")

if(length(gender)>0){
  dataset$gender<-dataset[,gender]
  d1<-dataset %>% gather(a,b,variety_a:variety_c) 
  
  vartable<-vartable %>%
    mutate("% of Respondents"=percent(Frequency/nrow(dataset),1),"x"=table(d1$b,d1$gender)[,1],
           "x2"=table(d1$b,d1$gender)[,2])
  
  colnames(vartable)[4:5]<-paste0(colnames(table(d1$b,d1$gender))[1:2]," (n=",table(dataset$gender),")")
}

#Map 1
geodata = F
if(geodata==TRUE){
map1<- map.f(data = dt.fr,lon = "lon",lat="lat")
mapshot(map1, file = "map1.png")
}

#Replace this with the updated function

#Favourability Analysis Table
df<-as.data.frame(dataset)
fav1<-favourability(a=df[,vars[1]],
                    b=df[,vars[2]],
                    c=df[,vars[3]],
                    best=df[,overall[1]],
                    worst=df[,overall[2]])

fav2<-fav1

fav2$best_per<-formattable::percent(fav2$best_per/100,1)
fav2$worst_per<-formattable::percent(fav2$worst_per/100,1)
fav2$wins<-formattable::percent(fav2$wins,1)
fav2$Fav_Score<-round(fav2$Fav_Score,1)
colnames(fav2)<-c("Variety","N","% Top Ranked","% Bottom Ranked","% Contests Won","Net Favourability Score")
fav2<-fav2[nrow(fav2):1,]

#Contest Plots
cont1<-contests(a=df[,vars[1]],
                b=df[,vars[2]],
                c=df[,vars[3]],
                best=df[,overall[1]],
                worst=df[,overall[2]])

#Trait agreement

agreement_traits<-concordance(overall_best = df[,metadat$num[which(metadat$type=="overall"&metadat$bw=="best")]],
                              overall_worst = df[,metadat$num[which(metadat$type=="overall"&metadat$bw=="worst")]],
                              bests =   df[,metadat$num[which(metadat$type!="overall"&metadat$bw=="best")]],
                              worsts = df[,metadat$num[which(metadat$type!="overall"&metadat$bw=="worst")]],
                              names = trait_short)

agreement_table<-agreement_traits[[2]] %>% select(trait,overall,best,worst) %>%
  rename("Agreement with Overall Best"=best,"Agreement with Overall Worst"=worst,
         "Complete Ranking Agreement"=overall)


strongest_link<-
  agreement_traits[[1]]$data %>% filter(type=="% Agreement with\nOverall Ranking") %>% filter(agreement==max(agreement))

weakest_link<-
  agreement_traits[[1]]$data %>% filter(type=="% Agreement with\nOverall Ranking") %>% filter(agreement==min(agreement))

#PL Model
#overall model

dt_comp<-na.omit(dt.fr[,c(vars,overall)])

R_overall <- to_rankings(dt_comp,
                         items = 1:3,
                         rankings = 4:5,
                         type = "tricot")

mod_overall <- PlackettLuce(R_overall,  npseudo = 0, maxit = 20)

model_summaries<-multcompPL(mod_overall,adjust = ci.adjust)
fullanova<-anova.PL(mod_overall)

worthscaled<-rev(sort(exp(coef(mod_overall))/sum(exp(coef(mod_overall)))))
worthscaled<-data.frame(Variety=factor(names(worthscaled),
                                       (names(worthscaled))),worth=worthscaled,
                        "% Probability"=percent(worthscaled),check.names = FALSE)


mods<-list()
summaries<-list()
worths<-list()
anovas<-list()
contests_t<-list()
for (i in 1:ntrait){
  
  
  best<-subset(metadat,trait==trait_names[i]&bw=="best")$num
  worst<-subset(metadat,trait==trait_names[i]&bw=="worst")$num
  
  contests_t[[i]]<-contests(a=df[,vars[1]],
                            b=df[,vars[2]],
                            c=df[,vars[3]],
                            best=df[,best],
                            worst=df[,worst])
  
  df_t<-na.omit(data.frame(a=df[,vars[1]],
                           b=df[,vars[2]],
                           c=df[,vars[3]],
                           best=df[,best],
                           worst=df[,worst]))
  
  n<-(2*i)-1
  R_t <- to_rankings(df_t,
                     items = 1:3,
                     rankings = 4:5,
                     type = "tricot")
  
  mod_t <-tryCatch( PlackettLuce(R_t,  npseudo = 0, maxit =20,trace=FALSE),
                   error=function(cond) {
                    message("Here's the original error message:")
                     message(cond)
                     # Choose a return value in case of error
                     return(NA)
                   })
  mods[[i]]<-mod_t
  if(class(mod_t)=="PlackettLuce"){
  summaries[[i]]<-multcompPL(mod_t,adjust = ci.adjust)
  
  anovas[[i]]<-anova.PL(mod_t)
  
  worths[[i]]<-rev(sort(exp(coef(mod_t))/sum(exp(coef(mod_t)))))
  
  worths[[i]]<-data.frame(Variety=factor(names(worths[[i]]),
                                         (names(worths[[i]]))),worth=worths[[i]],
                          "Probability"=percent(worths[[i]]))
  }
  if(class(mod_t)!="PlackettLuce"){
    anovas[[i]]<-NA
    worths[[i]]<-NA
  }  
  
}


#PLS Analysis combining traits together


coefs<-qvcalc(mod_overall)[[2]]$estimate
for(i in 1:ntrait){
  if(class(mods[[i]])=="PlackettLuce"){
  coefs<-cbind(coefs,scale(qvcalc(mods[[i]])[[2]]$estimate))
  }
}


rownames(coefs)<-rownames(qvcalc(mod_overall)[[2]])
colnames(coefs)<-c("Overall",trait_short[unlist(lapply(mods,class))=="PlackettLuce"])

coefs<-data.frame(coefs)

library(pls)

m2 <- plsr(as.formula(paste("Overall~",paste(trait_short[unlist(lapply(mods,class))=="PlackettLuce"],collapse="+"))),
           data=coefs, validation = "LOO", jackknife = TRUE)
arrows<-NULL
scores<-NULL
if(ncol(m2$projection)>1){
arrows<-data.frame((m2$projection)[,1:2],trait=trait_short,x0=0,y0=0)
scores<-data.frame((m2$scores)[,1:2],var=rownames(m2$scores))
}


yve <- drop(R2(m2, estimate = "train",
               intercept = FALSE)$val)

adjCV<-m2$validation$adj
nc<-which(adjCV==min(adjCV))

#Analysis with covariates

dt.fr2<-dt.fr

covarlist2=c(covars,coords)

lev1<-unique(c(dt.fr2$variety_a,dt.fr2$variety_b,dt.fr2$variety_c))
dt.fr2$variety_a<-factor(dt.fr2$variety_a,levels=lev1)
dt.fr2$variety_b<-factor(dt.fr2$variety_b,levels=lev1)
dt.fr2$variety_c<-factor(dt.fr2$variety_c,levels=lev1)

#first format the covariates and exclude those likely to cause problems
pout<-NULL
stoplist<-NULL
for(i in covarlist2){
  stop<-0
  
  if(class(dt.fr2[,i])=="character"){
    dt.fr2[,i]<-factor(dt.fr2[,i])
  }
  if(class(dt.fr2[,i])=="factor"){
   # dt.fr2[,i]<-as.factor(replace_na(as.character(dt.fr2[,i]),"missing"))
    
    t1<-table(dt.fr2[,i],dt.fr2$variety_a)
    t2<-table(dt.fr2[,i],dt.fr2$variety_b)
    t3<-table(dt.fr2[,i],dt.fr2$variety_c)
    
    tt<-t1+t2+t3
    if(any(tt==0)){
      stop<-1
    }
    
  }
  if(length(unique(dt.fr2[,i]))<2){
    stop<-2
  }
  
  if(class(dt.fr2[,i])!="factor"&class(dt.fr2[,i])!="character"&class(dt.fr2[,i])!="numeric"
     &class(dt.fr2[,i])!="integer"&class(dt.fr2[,i])!="Date"){
    stop<-3
  }
  if(mean(is.na(dt.fr2[,i]))>missper){
    stop<-4
  }
  
  
  stoplist<-c(stoplist,stop)
}

covarlist3<-covarlist2[stoplist==0]


R_overall <- to_rankings(dt.fr2,
                         items = vars,
                         rankings = overall,
                         type = "tricot")
dt.fr2$G<-grouped_rankings(R_overall, index = seq_len(nrow(R_overall)))


dt.fr_t<-na.omit(dt.fr2[,c("G",colnames(dt.fr2)[covarlist3])])

fullmodel<-as.formula(paste("G~",paste(colnames(dt.fr)[covarlist3],collapse="+")))




tree_f <- pltree(formula=fullmodel,
                 data = dt.fr_t, minsize = 50, alpha = 0.05)


if(length(tree_f)>1){
  
  coefs_t<-map_df(nodeids(tree_f,terminal = TRUE),
                  function(x)data.frame(node=x,
                                        rule=partykit:::.list.rules.party(tree_f, x),
                                        multcompPL(tree_f[[ x ]]$node$info$object)))
  
  
  ns<-map_df(nodeids(tree_f,terminal = TRUE),
             function(x)data.frame(node=x,
                                   rule=partykit:::.list.rules.party(tree_f, x),
                                   n=tree_f[[ x ]]$node$info$nobs))
  
  coefs_t<-inner_join(coefs_t,ns)
  
  coefs_t$Label<-paste("Node",coefs_t$node,":",coefs_t$rule,"\n","n=",coefs_t$n)
  
  
  coefs_t<-coefs_t %>% mutate(term=reorder(term,estimate,mean)) %>%
    group_by(node) %>% mutate(m=mean(estimate),ctd=estimate-m) %>%data.frame()
  
  rules=unique(coefs_t$rule)
  best_tree<-NULL
  
  for(i in 1:length(rules)){
    tmp<-subset(coefs_t,rule==rules[i])
    best_tree<-rbind(best_tree,c(tmp$n[1],paste(tmp$term[grep("a",tmp$.group)],collapse=", "),
                                 paste(rev(tmp$term[grep(tmp$.group[nrow(tmp)],tmp$.group)]),collapse=", ")))
    
    
  }
  node_summary<-data.frame(rules,best_tree)
  colnames(node_summary)<-c("Subgroup","Number of Respondents","Best Ranked Varieties","Worst Ranked Varieties")
  
}    


outtabs<-NULL
for(j in 1:length(tree_f)){
  xxx<-nodeapply(tree_f,j,function(n) info_node(n)$test)[[1]]
  if(length(xxx)>0){
    outtabs[[j]]<-data.frame(Node=j,t(nodeapply(tree_f,j,function(n) info_node(n)$test)[[1]]))
    outtabs[[j]]$p<-format.pval(outtabs[[j]]$p.value)
  }
  else{
    outtabs[[j]]<-data.frame(Node=j,Message="No further splits possible",p.value=NA)
  }
}


##Build headline summaries

siglist<-NULL
for(i in 1:length(outtabs)){
  if(ncol(outtabs[[i]])>3){
    siglist<-c(siglist,rownames(outtabs[[i]])[outtabs[[i]]$p.value<0.05])
  }
}
siglist<-unique(siglist)

ps<-fullanova[2,5]
if(ps<0.05){
bests<-paste(model_summaries$term[grep("a",model_summaries$.group)],collapse=", ")
worsts<-paste(rev(model_summaries$term[grep(model_summaries$.group[nrow(model_summaries)],
                                            model_summaries$.group)]),collapse=", ")
}else{
  bests<-worsts<-"No significant differences"
}

for(i in 1:length(anovas)){
  if(class(mods[[i]])=="PlackettLuce"){
  ps<-c(ps,anovas[[i]][2,5])
  if(ps[i]<0.05){
    bests<-c(bests,paste(summaries[[i]]$term[grep("a",summaries[[i]]$.group)],collapse=", "))
    worsts<-c(worsts,paste(rev(summaries[[i]]$term[grep(summaries[[i]]$.group[nrow(summaries[[i]])],
                                                        summaries[[i]]$.group)]),collapse=", "))
  }
  
  else{
    bests<-c(bests,"No significant differences")  
    worsts<-c(worsts,"No significant differences")  
  }
  }
  else{
    ps<-c(ps,NA)
    bests<-c(bests,NA)
    worsts<-c(worsts,NA)
  }
}
ptab<-data.frame(Ranking=c("Overall",trait_short),
                 p.value=ps,"Best Ranked"=bests,
                 "Worst Ranked"=worsts,check.names = FALSE)

ptab$p.value<-paste(format.pval(ptab$p.value),stars.pval(ptab$p.value))


outtabs[[1]]$p.value<-as.numeric(outtabs[[1]]$p.value)

uni_sum<-outtabs[[1]]
uni_sum$Variable<-rownames(outtabs[[1]])
uni_sum$p<-paste(format.pval(outtabs[[1]]$p.value),stars.pval(outtabs[[1]]$p.value))
rownames(uni_sum)<-NULL
