# ................................................................
# ................................................................
# Fetch ClimMob data using an API key

#library("devtools")
#install_github("kauedesousa/gosset", upgrade = "never")
library("gosset")
library("jsonlite")
library("httr")

# the API key 
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

# get your list of projects
getProjectsCM(key)

# data for project chocolate
# a project with 4 comparisons
getDataCM(key, "chocolate")

# keep the ODK variables codes
# this format is used internaly in the analysis 
# of ClimMob data from climmob.net
getDataCM(key, "chocolate", tidynames = FALSE)

# return the dataframe in wide format
getDataCM(key, "chocolate", pivot.wider = TRUE)

# data from project colours
# a project with 3 comparisons
getDataCM(key, "colours")

# data from project "wageningen" 
# a project with no data
getDataCM(key, "wageningen")






