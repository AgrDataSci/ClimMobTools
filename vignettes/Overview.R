## ----setup, include=FALSE------------------------------------------------
TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = TRAVIS
)

## ----fetch, message=FALSE, eval=FALSE, echo=TRUE-------------------------
#  library("ClimMobTools")
#  library("tidyverse")
#  library("magrittr")
#  
#  # the API key
#  key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"
#  
#  data <- ClimMobTools::getDataCM(key = key,
#                                  project = "breadwheat",
#                                  tidynames = TRUE)
#  
#  # reshape the data into the wide format
#  data %<>%
#    filter(!str_detect(variable, "survey")) %>%
#    group_by(id) %>%
#    distinct(id, variable, value) %>%
#    spread(variable, value) %>%
#    ungroup()
#  
#  

## ----temperature, message=FALSE, eval=FALSE, echo=TRUE-------------------
#  
#  # first we convert the lon lat into numeric
#  # and the planting dates into Date
#  data %<>%
#    mutate(lon = as.numeric(lon),
#           lat = as.numeric(lat),
#           plantingdate = as.Date(plantingdate,
#                                  format = "%Y-%m-%d"))
#  
#  # then we get the temperature indices
#  # get some variables to include in the model
#  # from the planting date to the next 120 days
#  temp <- temperature(data[c("lon","lat")],
#                      day.one = data["plantingdate"],
#                      span = 120)
#  
#  

