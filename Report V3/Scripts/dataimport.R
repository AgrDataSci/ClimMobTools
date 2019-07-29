##DATA IMPORT FILE##

#Full version to be developed to pull data off server

#This is just a temporary placeholder

#using internal dataset now for experimentation
#need to link in with climmob using Kauwe's script and these should be generated automatically from climmob metadata
data("breadwheat", package = "gosset")


dataset=breadwheat

#get this information from the metadata on the server - temporary hard code
vars=1:3
covars=c(4,5,7,8,9)
coords=10:11
overall=18:19
traits=12:17
exclude=6

#set text inputs for reporting style 
option="variety"
ranker="farmer"


gender=8
#create a metadata file to use later

metadat<-data.frame(type=c(rep("vars",length(vars)),
                           rep("covars",length(covars)),
                           rep("coords",length(coords)),
                           rep("overall",length(overall)),
                           rep("traits",length(traits)),
                           rep("exclude",length(exclude))),
                    num=c(vars,covars,coords,overall,traits,exclude))

metadat$colname<-colnames(dataset)[metadat$num]
metadat$trait<-ifelse(metadat$type%in%c("traits","overall"),str_split_fixed(metadat$colname,"_",2)[,1],"")
metadat$bw<-ifelse(metadat$type%in%c("traits","overall"),str_split_fixed(metadat$colname,"_",2)[,2],"")

ntrait<-length(traits)/2
ncovar<-length(covars)

### stats parameters

sig.level=0.05
minsplit=20
ci.adjust="B-H"
ci.level=0.84



#create copy of dataset for safety
dt.fr<-as.data.frame(dataset)

#create full covariate list to modify
covarlist=covars

#get trait names
trait_names<-unique(metadat$trait[metadat$type=="traits"])