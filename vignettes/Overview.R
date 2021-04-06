## ----setup, include=FALSE-----------------------------------------------------
TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = TRAVIS
)

## ----fetch, message=FALSE, eval=FALSE, echo=TRUE------------------------------
#  library("ClimMobTools")
#  
#  # the API key
#  key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"
#  
#  dt <- getDataCM(key = key,
#                  project = "breadwheat",
#                  pivot.wider = TRUE)
#  
#  
#  names(dt) <- gsub("firstassessment_|package_|lastassessment_|registration_", "",
#                    names(dt))
#  

## ----temperature, message=FALSE, eval=FALSE, echo=TRUE------------------------
#  library("climatrends")
#  library("nasapower")
#  
#  dt$plantingdate <- as.Date(dt$plantingdate, format = "%Y-%m-%d")
#  dt$lon <- as.numeric(dt$farm_geo_longitude)
#  dt$lat <- as.numeric(dt$farm_geo_latitude)
#  
#  temp <- temperature(dt[, c("lon","lat")],
#                      day.one = dt[, "plantingdate"],
#                      span = 80)
#  
#  temp

## ----plrankings, message=FALSE, eval=FALSE, echo=TRUE-------------------------
#  library("PlackettLuce")
#  #remotes::install_github("agrdatasci/gosset", build_vignettes = TRUE)
#  library("gosset")
#  
#  R <- rank_tricot(dt,
#                   items = c("item_A","item_B","item_C"),
#                   input = c("overallperf_pos","overallperf_neg"),
#                   group = TRUE)
#  
#  dat <- cbind(R, temp)
#  
#  pl <- pltree(R ~ maxNT + maxDT,
#               data = dat)
#  
#  summary(pl)
#  
#  plot(pl)
#  

