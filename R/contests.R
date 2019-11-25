#' Contests
#' 
#' Pairwise contest matrix between comparisons
# contests<- function(a,b,c,best,worst,format="ABC",type="all",player=NULL){
#   
#   nalist<-is.na(best)==F&is.na(worst)==F&is.na(a)==F&is.na(b)==F&is.na(c)==F
#   
#   bw<- data.frame(favourability(a,b,c,best,worst,
#                                 format=format,out="raw"),variety.a=a[nalist],variety.b=b[nalist],variety.c=c[nalist],stringsAsFactors = FALSE)
#   
#   bw<-na.omit(bw)
#   require(tidyr)
#   require(dplyr)
#   require(scales)
#   require(ggplot2)
#   
#   if(type=="all"){
#     vars<-sort(unique(c(bw$variety.a,bw$variety.b,bw$variety.c)))
#     nvars<-length(vars)
#     
#     out<-NULL
#     for(i in 1:nvars){
#       tmp<-ifelse(bw$best1==vars[i],1,
#                   ifelse(bw$worst1==vars[i],3,
#                          ifelse(bw[,"variety.a"]==vars[i]|
#                                   bw[,"variety.b"]==vars[i]|bw[,"variety.c"]==vars[i],2,NA)))
#       out<-cbind(out,tmp)
#     }
#     
#     colnames(out)<-vars
#     
#     pref<-matrix(NA,nvars,nvars)
#     nmat<-matrix(NA,nvars,nvars)
#     for(i in 1:nvars){
#       for(j in 1:nvars){
#         if(i!=j){
#           n<-sum(is.na(out[,i])==F&is.na(out[,j])==F)
#           pij<-sum(out[,i]>out[,j],na.rm=T)
#           pji<-sum(out[,i]<out[,j],na.rm=T)
#           pref[i,j]<-pij/n
#           pref[j,i]<-pji/n
#           nmat[i,j]<-nmat[j,i]<-n
#         }
#       }
#     }
#     
#     rownames(pref)<-vars
#     colnames(pref)<-vars
#     
#     pref2<-100*round(pref[(order(rowMeans(pref,na.rm=T))),(order(rowMeans(pref,na.rm=T)))],3)
#     pref2
#     pref2[lower.tri(pref2)]<-NA
#     
#     p1<-   pref2 %>%
#       data.frame(check.names = FALSE) %>%
#       mutate(var1=rownames(pref2)) %>%
#       gather(key="var2",value="val",-var1) %>%
#       mutate(txt=ifelse(val==50,"50:50",ifelse(val<50,paste("A","\n",100-val,"%",sep=""),
#                                                paste("B","\n",val,"%",sep=""))),
#              var1=factor(var1,levels=rev(rownames(pref2)))) %>%
#       mutate(var2=factor(var2,levels=rownames(pref2))) %>%
#       ggplot(aes(y=var1,x=var2,fill=100-val,label=txt))+
#       geom_tile(col="gray50")+
#       geom_text(size=3,fontface=2)+
#       scale_x_discrete(position="top")+
#       scale_fill_gradient2(limits=c(0,100),low = alpha("red",0.75),
#                            high=alpha("forestgreen",0.75),midpoint = 50)+
#       xlab("B")+ylab("A")+labs(fill="% Wins for\nDominant Variety")
#     
#     p2<- pref %>%
#       data.frame(var1=rownames(pref),check.names = FALSE) %>%
#       gather(-var1,key = "var2",value="value") %>%
#       mutate(var1=factor(var1,levels=rev(rownames(pref2))),
#              var2=factor(var2,levels=rownames(pref2))) %>%
#       ggplot(aes(y=value,x=var1))+
#       geom_bar(aes(fill=value),stat="identity",col="black")+
#       facet_wrap(~var2,scales="free_y",ncol=3)+
#       coord_flip()+
#       scale_fill_gradient2(limits=c(0,1),low = alpha("red",0.75),
#                            high=alpha("forestgreen",0.75),midpoint = 0.5,labels=percent,name="% Times Preferred")+
#       scale_y_continuous(labels=percent,limits=c(0,1))
#     
#     return(list(p1,p2,pref,nmat))
#   }
# }