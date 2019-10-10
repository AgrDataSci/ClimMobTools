#Input User Parameters

####DATA PARAMETERS



#key and projname to be used in call to getDataCM(). Should be determined based on user inputs.
key<-"d39a3c66-5822-4930-a9d4-50e7da041e77"
projname<-"breadwheat"

#Variable to produce split of results into multiple groups. Should be determined based on user selecting from list of possibilities
forcesplit<-"gender"

####STYLE PARAMETERS

#Outputs requested. should be "summary" for summary report only; "farmer" for farmer report only or "both" for both sets of reports
output<-"both"


#File format to use in rendering output. Only available options should be "docx", "pdf", and "html". Output format derived from extension
extension<-"docx"


#Set how the system will refer to each of the different options and to each of the different rankers. Defaults to "farmer" and "variety". 
#Should be allowed to be open text fields

ranker="farmer"
option="variety"

#select which coordinate set to use (e.g. one at place of registration and one
#at farm) - input should be a number referring to position of coordinate set in
#the data (1=first set, 2=second set, etc).. Defaults to NULL which includes
#first set found and discards all other coordinate sets. Can make this an
#applicable option to appear if multiple coordinate sets are found in the data
coordset<-NULL


#### Statistics parameters

#significance level. Should be allowed to be 0.01,0.05 or 0.1 I guess? Probably worth preventing any other numbers being selected.
sig.level=0.05

#method for adjustments for confidence intervals and setting widths for comparison. Defaults to B-H (Benjamini an Hochberg).
#Any of the methods from p.adjust will work here though: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
ci.adjust="BH"

#confidence interval level for comparison plots with error bars. 84% to give an
#approximate 5% significance level for comparisons of non-overlapping confidence
#intervals (e.g. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC524673/)
#should probably allow alternatives to be 0.9, 0.95 or 0.99
ci.level=0.84

#set maximum proportion of missing data allowed in a variable before it is
#excluded. 20% seems to work reasonably - probably should restrict the maximum
#value to something like 30 or 40%.
missper=0.2

#Set minimum split size for tree models. Probably should be tied cleverly into
#the number of varieties being compared, but can allow user to push this back or
#forth if they want
minsplit=30

#could also allow other control options into tree models as custom parameters


###FARMER REPORT PARAMS
# eventual information to make the info table at the back of the farmer reports. Default to no info. Should be added by the user
info.table.items <- c() #info.table.items <- c("variety 1", "variety 2", "variety 3")
info.table.info <- c() #info.table.info <- c("plant it early", "good in high altitude", "")
info.table.typeinfo <- "" #info.table.typeinfo <- "expert advice"

#list of farmer ids for which we want to produce the feedback form. Should be determined based on user selecting from list of ids
ranker.ids <- c(1,2,5,7) #should probably be defaulted to the ids of all the farmers