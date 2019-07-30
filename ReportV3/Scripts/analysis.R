####Analysis####


#Table 1 - Variety Summary
vartable<-data.frame(table(c(dataset$variety_a,dataset$variety_b,dataset$variety_c)))
colnames(vartable)<-c("Variety","Frequency")

if(length(gender)>0){
d1<-dataset %>% gather(a,b,variety_a:variety_c) 

vartable<-vartable %>%
  mutate("% of Respondents"=percent(Frequency/nrow(dataset),1),"Female (n="=table(d1$b,d1$gender)[,1],
         "Male (n="=table(d1$b,d1$gender)[,2])

colnames(vartable)[4:5]<-paste0(colnames(vartable)[4:5],table(dataset$gender),")")
}

#Map 1

map1<- map.f(data = dt.fr,lon = "lon",lat="lat")

mapshot(map1, file = "map1.png")

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

agreement_traits<-concordance(overall_best = df[,metadat$num[which(metadat$trait=="overall"&metadat$bw=="best")]],
               overall_worst = df[,metadat$num[which(metadat$trait=="overall"&metadat$bw=="worst")]],
               bests =   df[,metadat$num[which(metadat$trait!="overall"&metadat$bw=="best")]],
               worsts = df[,metadat$num[which(metadat$trait!="overall"&metadat$bw=="worst")]],
               names = trait_names)

agreement_table<-agreement_traits[[2]] %>% select(trait,overall,best,worst) %>%
  rename("Agreement with Overall Best"=best,"Agreement with Overall Worst"=worst,
         "Complete Ranking Agreement"=overall)


strongest_link<-
  agreement_traits[[1]]$data %>% filter(type=="% Agreement with\nOverall Ranking") %>% filter(agreement==max(agreement))

weakest_link<-
  agreement_traits[[1]]$data %>% filter(type=="% Agreement with\nOverall Ranking") %>% filter(agreement==min(agreement))

#PL Model
#overall model

R_overall <- to_rankings(dt.fr,
                         items = vars,
                         rankings = overall,
                         type = "tricot")

mod_overall <- PlackettLuce(R_overall,  npseudo = 0, maxit = 7)

model_summaries<-multcompPL(mod_overall,adjust = "BH")
fullanova<-anova.PL(mod_overall)

worthscaled<-rev(sort(exp(coef(mod_overall))/sum(exp(coef(mod_overall)))))
worthscaled<-data.frame(Variety=factor(names(worthscaled),
                                       (names(worthscaled))),worth=worthscaled,
                        "% Probability"=percent(worthscaled))


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
  
  
n<-(2*i)-1
R_t <- to_rankings(df,
                   items = vars,
                   rankings = traits[n:(n+1)],
                   type = "tricot")

mod_t <- PlackettLuce(R_t,  npseudo = 0, maxit = 7)
mods[[i]]<-mod_t

summaries[[i]]<-multcompPL(mod_t,adjust = "BH")

anovas[[i]]<-anova.PL(mod_t)

worths[[i]]<-rev(sort(exp(coef(mod_t))/sum(exp(coef(mod_t)))))

worths[[i]]<-data.frame(Variety=factor(names(worths[[i]]),
                                       (names(worths[[i]]))),worth=worths[[i]],
                        "Probability"=percent(worths[[i]]))
}


#PLS Analysis combining traits together

coefs<-qvcalc(mod_overall)[[2]]$estimate
for(i in 1:ntrait){
  coefs<-cbind(coefs,scale(qvcalc(mods[[i]])[[2]]$estimate))
}


rownames(coefs)<-rownames(qvcalc(mod_overall)[[2]])
colnames(coefs)<-c("Overall",trait_names)

coefs<-data.frame(coefs)

library(pls)

m2 <- plsr(as.formula(paste("Overall~",paste(trait_names,collapse="+"))),data=coefs, validation = "LOO", jackknife = TRUE)

arrows<-data.frame((m2$projection)[,1:2],trait=trait_names,x0=0,y0=0)
scores<-data.frame((m2$scores)[,1:2],var=rownames(m2$scores))
yve <- drop(R2(m2, estimate = "train",
               intercept = FALSE)$val)

adjCV<-m2$validation$adj
nc<-which(adjCV==min(adjCV))

#Analysis with covariates
dt.fr$G<-grouped_rankings(R_overall, index = seq_len(nrow(R_overall)))

covarlist2=c(covars,coords)

colnames(dt.fr[,covarlist2])

#first format the covariates and exclude those likely to cause problems
pout<-NULL
stoplist<-NULL
for(i in covarlist2){
  stop<-0
  
  if(class(dt.fr[,i])=="character"){
    dt.fr[,i]<-factor(dt.fr[,i])
  }
  if(class(dt.fr[,i])=="factor"){
    dt.fr[,i]<-as.factor(replace_na(as.character(dt.fr[,i]),"missing"))
    
    t1<-table(dt.fr[,i],dt.fr$variety_a)
    t2<-table(dt.fr[,i],dt.fr$variety_b)
    t3<-table(dt.fr[,i],dt.fr$variety_c)
    
    tt<-t1+t2+t3
    if(any(tt==0)){
      stop<-1
    }
    
  }
  if(length(unique(dt.fr[,i]))<2){
    stop<-2
  }
  
  if(class(dt.fr[,i])!="factor"&class(dt.fr[,i])!="character"&class(dt.fr[,i])!="numeric"
     &class(dt.fr[,i])!="integer"&class(dt.fr[,i])!="Date"){
    stop<-3
  }
stoplist<-c(stoplist,stop)
}

covarlist3<-covarlist2[stoplist==0]

fullmodel<-as.formula(paste("G~",paste(colnames(dt.fr)[covarlist3],collapse="+")))

R_overall <- to_rankings(dt.fr,
                         items = vars,
                         rankings = overall,
                         type = "tricot")
dt.fr$G<-grouped_rankings(R_overall, index = seq_len(nrow(R_overall)))


tree_f <- pltree(formula=fullmodel,
                data = dt.fr, minsize = 50, alpha = 0.05)


if(length(tree_f)>1){
  
coefs<-map_df(nodeids(tree_f,terminal = TRUE),
                function(x)data.frame(node=x,
                                      rule=partykit:::.list.rules.party(tree_f, x),
                                      multcompPL(tree_f[[ x ]]$node$info$object)))
  
  
ns<-map_df(nodeids(tree_f,terminal = TRUE),
             function(x)data.frame(node=x,
                                   rule=partykit:::.list.rules.party(tree_f, x),
                                   n=tree_f[[ x ]]$node$info$nobs))
  
  coefs<-inner_join(coefs,ns)
  
  coefs$Label<-paste("Node",coefs$node,":",coefs$rule,"\n","n=",coefs$n)
  
  
  coefs<-coefs %>% mutate(term=reorder(term,estimate,mean)) %>%
    group_by(node) %>% mutate(m=mean(estimate),ctd=estimate-m) %>%data.frame()
}    


outtabs<-NULL
for(j in 1:length(tree_f)){
  xxx<-nodeapply(tree_f,j,function(n) info_node(n)$test)[[1]]
  if(length(xxx)>0){
    outtabs[[j]]<-data.frame(Node=j,t(nodeapply(tree_f,j,function(n) info_node(n)$test)[[1]]))
    outtabs[[j]]$p<-format.pval(outtabs[[j]]$p.value)
  }
  else{
    outtabs[[j]]<-data.frame(Node=j,Message="No further splits possible")
  }
}


##Build headline summaries

siglist<-NULL
for(i in 1:length(outtabs)){
  if(ncol(outtabs[[i]])>2){
    siglist<-c(siglist,rownames(outtabs[[i]])[outtabs[[i]]$p.value<0.05])
  }
}
siglist<-unique(siglist)

ps<-fullanova[2,5]
bests<-paste(model_summaries$term[grep("a",model_summaries$.group)],collapse=", ")
worsts<-paste(rev(model_summaries$term[grep(model_summaries$.group[nrow(model_summaries)],
                                            model_summaries$.group)]),collapse=", ")

for(i in 1:length(anovas)){
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
ptab<-data.frame(Ranking=c("Overall",trait_names),
                 p.value=ps,"Best Ranked"=bests,
                 "Worst Ranked"=worsts,check.names = FALSE)

ptab$p.value<-paste(format.pval(ptab$p.value),stars.pval(ptab$p.value))


rules=unique(coefs$rule)
best_tree<-NULL

for(i in 1:length(rules)){
  tmp<-subset(coefs,rule==rules[i])
  best_tree<-rbind(best_tree,c(tmp$n[1],paste(tmp$term[grep("a",tmp$.group)],collapse=", "),
                               paste(rev(tmp$term[grep(tmp$.group[nrow(tmp)],tmp$.group)]),collapse=", ")))
  
  
}
node_summary<-data.frame(rules,best_tree)
colnames(node_summary)<-c("Subgroup","Number of Respondents","Best Ranked Varieties","Worst Ranked Varieties")


uni_sum<-outtabs[[1]]
uni_sum$Variable<-rownames(outtabs[[1]])
uni_sum$p<-paste(format.pval(outtabs[[1]]$p.value),stars.pval(outtabs[[1]]$p.value))
rownames(uni_sum)<-NULL
