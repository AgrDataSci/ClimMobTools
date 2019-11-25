#######GET DATA FROM SERVER

mydata <- data.frame(ClimMobTools::getDataCM(key, project = projname, pivot.wider = TRUE, tidynames = FALSE))
rawdata <- ClimMobTools::getDataCM(key, project = projname, pivot.wider = TRUE, tidynames = FALSE,raw=TRUE)
#mydata<-rawdata$data

#pull out components of the raw data file for use elsewhere
characteristics <- rawdata$specialfields
keys <- rawdata$importantfields
nameproj <- rawdata$project$project_name
fields <- rawdata$registry$fields

#combine data collected after registration into a single dataframe
latefields<-do.call("rbind",rawdata$assessments$fields)

#pick out variables which can be used as covariates in analysis
covardata<-fields[grep("integer|decimal|select_one|date",fields$odktype),c("name","desc","odktype")]

#pick our the variables which are the overall ranking
overalls<-keys[keys$type%in%c("OverallCharPos","OverallCharNeg"),]

#combine the "characteristics" and "overall" variables into a single trait field
trait_data<-left_join(characteristics,overalls,by=c("name"="field"))

#tidy up names of traits so that pos and neg can be identified with trait
trait_data$name2<-str_split_fixed(trait_data$name,pattern = "_char_",2)[,2]
trait_data$name3<-str_split_fixed(trait_data$name,pattern = "_",2)[,2]
trait_data$direction<-substr(trait_data$name2,nchar(trait_data$name2)-2,nchar(trait_data$name2))
trait_data$trait_shrt<-substr(trait_data$name2,1,nchar(trait_data$name2)-4)
 
#pick out the positive traits to merge the labels consistently across all traits
pos<-subset(trait_data,direction=="pos",select=c("trait_shrt","desc.x"))
colnames(pos)[2]<-"labeltext"
trait_data<-merge(trait_data,pos,all=TRUE)

#create a flag for the overall trait variable
trait_data$Overall<-ifelse(trait_data$type.y%in%c("OverallCharPos","OverallCharNeg"),"Overall",
                           ifelse(trait_data$type.x=="Characteristic","Trait","Skip"))

#remove the traits or missing values and only keep the variables which can be used in the analysis
latefields2<-subset(latefields,!name%in%trait_data$name3 & is.na(key)&
                      grepl("integer|decimal|select_one|date",latefields$odktype),select=c("name","desc","odktype"))

#combine the late fields with the fields collected at registration
covardata<-rbind(covardata,latefields2)

#look for coordinates
geodata<-any(grepl("geopoint",c(fields$odktype,latefields$odktype))==TRUE)

#can only use one coordinate set at the moment, so look to extract the coordinate set based on input set to coordset parameter
#by default it will look for the first set of coordinates it finds
coords<-NULL
if(geodata==TRUE){
  coords<-colnames(mydata)[grep("_lon$|_lat$",colnames(mydata))]
  
  if(length(coords)>2){
    if(is.null(coordset)){coordset<-1}
    coords<-coords[c((coordset*2)-1,coordset*2)]
  }
  mydata$lon<-as.numeric(mydata[,coords[1]])
  mydata$lat<-as.numeric(mydata[,coords[2]])
  
  coords<-c(ncol(mydata)-1,ncol(mydata))
}

#work with the variety data to format these together
vars<-grep("item_A|item_B|item_C",colnames(mydata))
covars<-NULL
if(nrow(covardata)>0){
  covars<-grep(paste0("_",paste(covardata$name,collapse="|")),colnames(mydata))
}

overall<-grep(paste0("_",paste(trait_data$name3[trait_data$Overall=="Overall"],collapse="|")),colnames(mydata))
traits<-grep(paste0("_",paste(trait_data$name3[trait_data$Overall=="Trait"],collapse="|")),colnames(mydata))
performance<-grep(paste0("_",paste(trait_data$name3[trait_data$Overall=="Skip"],collapse="|")),colnames(mydata))

dates<-ifelse(any(covardata$odktype=="date"),
              grep(paste0("_",paste(covardata$name[covardata$odktype=="date"],collapse="|")),colnames(mydata)),
              0)

#take a copy of the data
dataset<-mydata


#identify the column which is the forced splitting column
gender=grep(forcesplit,colnames(mydata))

#create a metadata file to use later linking up all the columns used

metadat<-data.frame(type=c(rep("vars",length(vars)),
                           rep("covars",length(covars)),
                           rep("overall",length(overall)),
                           rep("traits",length(traits))),
                    num=c(vars,covars,overall,traits))

#add some columns id-ing which is which trait and which is pos and which is neg
metadat$colname<-colnames(dataset)[metadat$num]
metadat$trait<-ifelse(metadat$type%in%c("traits","overall"),str_replace(metadat$colname,"_best|_worst|_pos|_neg",""),"")
metadat$bw<-ifelse(metadat$type%in%c("traits","overall"),
                   gsub("_","",str_extract(metadat$colname,"_best|_worst|_pos|_neg")),"")

#get time of data collection and mergeable name
metadat$time<-str_split_fixed(metadat$colname,"_",3)[,1]
metadat$metadataname<-str_split_fixed(metadat$colname,"_",3)[,3]

#merge extra fields into metadata
metadat<-left_join(metadat,covardata,by=c("metadataname"="name"))
metadat<-left_join(metadat,trait_data[,c("name3","trait_shrt","labeltext")],by=c("metadataname"="name3"))

#replace some missing values with some useful info
metadat$desc[is.na(metadat$desc)]<-metadat$labeltext[is.na(metadat$desc)]

#get rid of now obsolete final column
metadat<-metadat[,-ncol(metadat)]

#calculate number of traits and number of covariates for looping purposes
ntrait<-length(traits)/2
ncovar<-length(covars)

#rename some columns to make compatible with old style output
colnames(dataset)[vars]<-c("variety_a","variety_b","variety_c")

metadat$bw[metadat$bw=="pos"]<-"best"
metadat$bw[metadat$bw=="neg"]<-"worst"

#create full covariate list to modify
covarlist=covars

#modify date structures so that they look like dates
if(dates[1]!=0){
  for(i in 1:length(dates)){
    dataset[,dates[i]]<-as.Date(dataset[,dates[i]])
  }
}

#identify numeric columns
nums<-metadat$num[metadat$odktype%in%c("integer","decimal")]

if(length(nums)>0){
  for(i in 1:length(nums)){
    dataset[,nums[i]]<-as.numeric(as.character(dataset[,nums[i]]))
  }
}

#get trait names
trait_names<-unique(metadat$trait[metadat$type=="traits"])
trait_labels<-metadat$desc[(metadat$trait%in%trait_names)&metadat$bw=="best"]
trait_short<-metadat$trait_shrt[(metadat$trait%in%trait_names)&metadat$bw=="best"]

#get covar names

covar_full<-metadat$colname[metadat$num%in%covarlist]
covar_names<-metadat$metadataname[metadat$num%in%covarlist]
covar_labels<-metadat$desc[metadat$num%in%covarlist]

colnames(dataset)[colnames(dataset)%in%covar_full]<-covar_names

#make sure data looks like a data frame then we are good to go
dt.fr<-as.data.frame(dataset)

#make the option and ranker fields into plurals. pluralise should do this in a somewhat clever way.
options<-pluralize(option)
rankers<-pluralize(ranker)
