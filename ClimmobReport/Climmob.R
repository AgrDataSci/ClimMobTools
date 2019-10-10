####Climmob.R

#On server shoudln't need to set wd if everything is hosted in sensible place with sensible defaults.
setwd("C:/Users/sdumb/Dropbox (SSD)/Climmob Project/ClimmobReport")

#1. Call all relevant libraries
source("Setup/dependencies.R")

#2. load user written functions
source("Setup/functions.R")

#3. Bring in user specified parameters for reporting process (needs to dynamically link to interface in climmob so suer can set/modify these values)
source("Setup/params.R")

#4. Get data off server and manipulate into required format
source("Setup/data.R")

#5. Run analysis of data
source("Setup/analysis.R")

#6. Output results

#determine format based on extensions
output_format = ifelse(extension=="docx","word_document",paste0(extension,"_document"))

#produce main report if output type is "summary" or "both"
if(output!="farmer"){
  rmarkdown::render("Main Report/mainreport.Rmd",output_dir=paste(getwd(),"Output",sep="/"),output_format=output_format,
                    output_file = paste0(nameproj," Report",".",extension))
}

#produce farmer reports if output type is "farmer" or "both"
if(output!="summary"){
  source("Farmer Reports/farmerreport.R")
}

##Would be good to add a line at this point putting everything into a zip file?

#Then removing the temporary versions?

rm(list=ls())
