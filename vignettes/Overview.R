## ----setup, include=FALSE-----------------------------------------------------
TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = TRAVIS
)

## ----cran, message=FALSE, eval=FALSE, echo=TRUE-------------------------------
#  install.packages("ClimMobTools")

## ----install, message=FALSE, eval=FALSE, echo=TRUE----------------------------
#  library("devtools")
#  
#  devtools::install_github("agrobioinfoservices/ClimMobTools")
#  

## ----fetch, message=FALSE, eval=FALSE, echo=TRUE------------------------------
#  library("ClimMobTools")
#  
#  # the API key
#  key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"
#  
#  data <- getDataCM(key = key,
#                    project = "breadwheat",
#                    tidynames = TRUE,
#                    pivot.wider = TRUE)
#  

## ----plrankings, message=FALSE, eval=FALSE, echo=TRUE-------------------------
#  library("PlackettLuce")
#  library("gosset")
#  
#  R <- rank_tricot(data,
#                   items = c("item_A","item_B","item_C"),
#                   input = c("overallperf_pos","overallperf_neg"),
#                   group = FALSE)
#  
#  mod <- PlackettLuce(R)
#  
#  

