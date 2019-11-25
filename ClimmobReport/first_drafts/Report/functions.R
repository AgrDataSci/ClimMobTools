
###Functions for Climmob Reporting Analysis

##List of functions
##Updated 01 May 2019
##
##byfac() : produces PlackettLuce estimates and errorbars to compare between different levels of factor
##
##favourability() : calculates total wins, losses and favourability scores
##
##favourability_plot() : plot of favourability scores
##
##win_plot() : plot of win %
##
##multcompPL() : does mean seperation from PlackettLuce object with CLD grouping
##
##plot.multcompPL() : plots errorbars, confidence intervals and CLD letters
##
##concordance() : determines level of agreement between list of traits with the overall trait
##
##contests() : produce pairwise contest matrix between all varieties


byfac<-function(model,split){
  
  split<-as.factor(split)
  out<-NULL
  for(i in 1:nlevels(split)){
    mod_t<-update(mod1,rankings=R[split==levels(split)[i],])
    
    tmp<-data.frame(var=rownames(qvcalc(mod_t)$qvframe),split=levels(split)[i],qvcalc(mod_t)$qvframe)
    tmp$estimate_adj<-tmp$estimate-mean(tmp$estimate)
    
    
    out<-rbind(out,tmp)
  }
  out
  
  ggplot(data=out,aes(y=estimate_adj,x=var,ymax=estimate_adj+qnorm(0.92)*quasiSE,
                      ymin=estimate_adj-qnorm(0.92)*quasiSE,col=split))+
    geom_errorbar(width=0.2,position = position_dodge(width=0.25))+
    geom_point(position = position_dodge(width=0.25))
}

favourability<-function(a,b,c,best,worst,format="ABC",reorder=TRUE,out="summary"){
  a<-as.character(a)
  b<-as.character(b)
  c<-as.character(c)
  best<-as.character(best)
  worst<-as.character(worst)
  
  if(format=="abc"){
    best<-toupper(best)
    worst<-toupper(worst)
  }
  
  best1<-ifelse(best=="A",a,
                ifelse(best=="B",b,
                       ifelse(best=="C",c,NA)))
  worst1<-ifelse(worst=="A",a,
                 ifelse(worst=="B",b,
                        ifelse(worst=="C",c,NA)))
  if(out=="summary"){
  vars<-sort(unique(c(a,b,c)))
  
  inrow<-NULL
  wins<-NULL
  losses<-NULL
  
  for(i in 1:length(vars)){
    inrow<-cbind(inrow,ifelse(a==vars[i]|b==vars[i]|c==vars[i],1,0))
    wins<-cbind(wins,ifelse(best1==vars[i],1,0))
    losses<-cbind(losses,ifelse(worst1==vars[i],1,0))
  }
  colnames(inrow)<-paste("n",1:(length(vars)),sep="")
  colnames(wins)<-paste("b",1:(length(vars)),sep="")
  colnames(losses)<-paste("w",1:(length(vars)),sep="")
  
  
  

    sumstats<-data.frame(var=vars,N=colSums(inrow),
                         best_per=100*colSums(wins)/colSums(inrow),
                         worst_per=100*colSums(losses)/colSums(inrow),
                         wins=((2*colSums(wins))+colSums(inrow-wins-losses))/(2*colSums(inrow)))
    
    sumstats$Fav_Score=sumstats$best_per-sumstats$worst_per
    
    if(reorder==TRUE){
      sumstats$var<-reorder(sumstats$var,sumstats$Fav_Score,mean)
      sumstats<-sumstats[order(sumstats$Fav_Score),]
    }
    return(sumstats)
  }
  else{
   return( data.frame(best1,worst1))
  }
}

favourability_plot<-function(x){
  p1<- ggplot(data=x,aes(y=Fav_Score,fill=Fav_Score,x=var))+
    geom_hline(yintercept = 0)+
    geom_bar(stat="identity",col="black")+
    coord_flip()+
    scale_y_continuous(breaks=seq(-100,100,by=20))+
    scale_fill_gradient2(low="red",mid="white",high="forestgreen",limits=c(-100,100))
  
  return(p1)
}

win_plot<-function(x){
  p1<- ggplot(data=x,aes(y=wins,fill=wins,x=var))+
    geom_bar(stat="identity",col="black")+
    coord_flip()+
    scale_y_continuous(breaks=seq(0,1,by=0.1),labels=scales::percent)+
    scale_fill_gradient2(low="red",mid="white",high="forestgreen",limits=c(0,1),midpoint=0.5)
  
  return(p1)
}

multcompPL<-function(mod,terms=NULL,threshold=0.05,Letters=letters,adjust="none"){
  
  require(qvcalc)
  require(multcompView)
  
  #get estimates with quasi-SEs
  qv1<-qvcalc(mod)$qvframe
  
  #reduce frame to only selected terms if not all comparisons are desired
  if(is.null(terms)==FALSE){
    qv1<-subset(qv1,rownames(qv1)%in%terms)
    #give error if less than 2 terms can be identified
    if(nrow(qv1)<3){
      stop("Less than 2 terms selected")
    }
  }
  
  #set up matrices for all differences and pooled errors
  diffs<-mat.or.vec(nrow(qv1),nrow(qv1))
  ses<-mat.or.vec(nrow(qv1),nrow(qv1))
  
  for(i in 1:nrow(qv1)){
    for(j in 1:nrow(qv1)){
      
      #get differences and pooled ses
      diffs[i,j]<-qv1$estimate[i]-qv1$estimate[j]
      ses[i,j]<-sqrt(qv1$quasiVar[i]+qv1$quasiVar[j])
    }
  }
  
  #calculate z scores
  z<-diffs/ses
  #TO DO: What DF to use to use here? Is it just the resid DF?
  p<-2*(1-pt(abs(z),mod$df.residual))
  
  #adjust p-value if you want to adjust. make sure to only take each p once for adjustment
  p[upper.tri(p)]<-p.adjust(p[upper.tri(p)],method = adjust)
  
  #make sure lower triangular is mirror of upper
  p[lower.tri(p)] = t(p)[lower.tri(p)]
  
  #set rownames
  rownames(p)<-colnames(p)<-rownames(qv1)
  
  #re-order qv output to ensure letters are produced in a sensible order
  qv1$term<-reorder(factor(rownames(qv1)),qv1$estimate,mean)
  qv1<-qv1[order(qv1$estimate,decreasing = TRUE),]
  
  #get mean seperation letter groupings
  qv1$.group<-multcompLetters2(estimate ~ term, p, qv1,
                               compare="<",
                               threshold=threshold,
                               Letters=Letters,
                               reversed = FALSE)$`Letters`
  return(qv1)
  
}

#simple ggplot function to plot output from multcompPL with error bars
plot.multcompPL<-function(x,level=0.95,xlab="",ylab=""){
  require(ggplot2)
  p1<- ggplot(data=x,aes(y=estimate,x=term,label=.group,ymax=estimate+qnorm(1-(1-level)/2)*quasiSE,
                         ymin=estimate-qnorm(1-(1-level)/2)*quasiSE))+
    geom_point()+
    geom_errorbar(width=0.1)+
    coord_flip()+
    geom_text(vjust=1.2)+
    xlab(xlab)+ylab(ylab)
  return(p1)
}

concordance<-function(overall_best,overall_worst,bests,worsts,names){
  require(ggplot2)
  require(dplyr)
  require(formattable)
  require(scales)
  require(tidyr)
  
  
  b1<-bests
  colnames(b1)<-paste(names,"b",sep="")
  
  w1<-worsts
  colnames(w1)<-paste(names,"w",sep="")
  
  dt1<-data.frame(overall_b=overall_best,overall_w=overall_worst,b1,w1,stringsAsFactors = FALSE)
  
  for(i in 3:(2+ncol(bests))){
    dt1[,i]<-dt1[,i]==dt1[,1]
  }
  for(i in (3+ncol(bests)):ncol(dt1)){
    dt1[,i]<-dt1[,i]==dt1[,2]
  }
  
  
  Overall<-dt1[,3:(2+ncol(bests))]==TRUE & dt1[,(3+ncol(bests)):ncol(dt1)]==TRUE
  
  plot_data<-data.frame(agreement=c(colMeans(dt1[,-(1:2)]),colMeans(Overall)),
                        type=rep(c("best","worst","overall"),each=length(names)),
                        trait=rep(names,3))
  
  plot_data$trait<-reorder(factor(plot_data$trait),plot_data$agreement,mean)
  
  out_data<-plot_data %>% mutate(agreement=percent(agreement,1)) %>% spread(type,agreement)
  
  
  plot_data$type<-factor(plot_data$type,levels=rev(c("worst","best","overall")),
                         labels=rev(c("% Agreement with\nOverall Worst",
                                      "% Agreement with\nOverall Best",
                                      "% Agreement with\nOverall Ranking")))
  
  
  p1<-ggplot(plot_data,aes(y=agreement,x=trait,alpha=agreement,fill=type))+
    geom_bar(stat="identity",position="dodge",col=alpha("gray50",1),show.legend = FALSE)+
    facet_wrap(~type)+coord_flip()+
    geom_hline(yintercept=0:1,size=1)+
    scale_fill_manual(values=c("purple","forestgreen","red"))+
    geom_text(aes(y=agreement/2,label=formattable::percent(agreement,1)),fontface=2,size=8,alpha=1)+
    scale_y_continuous(labels=scales::percent,breaks=seq(0,1,by=0.2),limits=c(0,1))+ylab("")+xlab("")+
    theme(axis.text.y = element_text(size=15,face=2),strip.text =element_text(size=15,face=2) )
  
  
  return(list(p1,out_data))
}


contests<-function(a,b,c,best,worst,format="ABC",type="all",player=NULL){
  
 bw<- data.frame(favourability(a,b,c,best,worst,
                format=format,out="raw"),variety.a=a,variety.b=b,variety.c=c)
  
  require(tidyr)
  require(dplyr)
  require(scales)
  require(ggplot2)
  
  if(type=="all"){
    vars<-sort(unique(c(a,b,c)))
    nvars<-length(vars)
    
    out<-NULL
    for(i in 1:nvars){
      tmp<-ifelse(bw$best1==vars[i],1,
                  ifelse(bw$worst1==vars[i],3,
                         ifelse(bw[,"variety.a"]==vars[i]|
                                  bw[,"variety.b"]==vars[i]|bw[,"variety.c"]==vars[i],2,NA)))
      out<-cbind(out,tmp)
    }
    
    colnames(out)<-vars
    
    pref<-matrix(NA,nvars,nvars)
    for(i in 1:nvars){
      for(j in 1:nvars){
        if(i!=j){
          n<-sum(is.na(out[,i])==F&is.na(out[,j])==F)
          pij<-sum(out[,i]>out[,j],na.rm=T)
          pji<-sum(out[,i]<out[,j],na.rm=T)
          pref[i,j]<-pij/n
          pref[j,i]<-pji/n
        }
      }
    }
    
    rownames(pref)<-vars
    colnames(pref)<-vars
    
    pref2<-100*round(pref[(order(rowMeans(pref,na.rm=T))),(order(rowMeans(pref,na.rm=T)))],3)
    pref2
    pref2[lower.tri(pref2)]<-NA
    
 p1<-   pref2 %>%
      data.frame(check.names = FALSE) %>%
      mutate(var1=rownames(pref2)) %>%
      gather(key="var2",value="val",-var1) %>%
      mutate(txt=ifelse(val==50,"50:50",ifelse(val<50,paste("A","\n",100-val,"%",sep=""),
                                               paste("B","\n",val,"%",sep=""))),
             var1=factor(var1,levels=rev(rownames(pref2)))) %>%
      mutate(var2=factor(var2,levels=rownames(pref2))) %>%
      ggplot(aes(y=var1,x=var2,fill=100-val,label=txt))+
      geom_tile(col="gray50")+
      geom_text(size=3,fontface=2)+
      scale_x_discrete(position="top")+
      scale_fill_gradient2(limits=c(0,100),low = alpha("red",0.75),
                           high=alpha("forestgreen",0.75),midpoint = 50)+
      xlab("B")+ylab("A")+labs(fill="% Wins for\nDominant Variety")
    
   p2<- pref %>%
      data.frame(var1=rownames(pref),check.names = FALSE) %>%
    gather(-var1,key = "var2",value="value") %>%
      mutate(var1=factor(var1,levels=rev(rownames(pref2))),
             var2=factor(var2,levels=rownames(pref2))) %>%
        ggplot(aes(y=value,x=var1))+
          geom_bar(aes(fill=value),stat="identity",col="black")+
            facet_wrap(~var2,scales="free_y")+
              coord_flip()+
      scale_fill_gradient2(limits=c(0,1),low = alpha("red",0.75),
                           high=alpha("forestgreen",0.75),midpoint = 0.5,labels=percent,name="% Times Preferred")+
      scale_y_continuous(labels=percent,limits=c(0,1))
    
    return(list(p1,p2))
  }
}


