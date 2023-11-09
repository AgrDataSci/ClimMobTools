## ----setup, include=FALSE-----------------------------------------------------
TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = TRAVIS
)

library("ClimMobTools")
library("PlackettLuce")
library("climatrends")
library("nasapower")

## ----fetch, message=FALSE, eval=TRUE, echo=TRUE-------------------------------
library("ClimMobTools")
library("PlackettLuce")
library("climatrends")
library("nasapower")

# the API key
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

dat <- getDataCM(key = key,
                 project = "breadwheat",
                 userowner = "gosset",
                 pivot.wider = TRUE)


names(dat) <- gsub("firstassessment_|package_|lastassessment_|registration_", "",
                   names(dat))


## ----temperature, message=FALSE, eval=TRUE, echo=TRUE-------------------------
dat$plantingdate <- as.Date(dat$plantingdate, format = "%Y-%m-%d")
dat$lon <- as.numeric(dat$farm_geo_longitude)
dat$lat <- as.numeric(dat$farm_geo_latitude)

temp <- temperature(dat[, c("lon","lat")], 
                    day.one = dat[, "plantingdate"],
                    span = 80)

temp

## ----plrankings, message=FALSE, eval=TRUE, echo=TRUE--------------------------
R <- rankTricot(dat, 
                items = c("item_A","item_B","item_C"), 
                input = c("overallperf_pos","overallperf_neg"),
                group = TRUE)

pld <- cbind(R, temp)

pl <- pltree(R ~ maxNT + maxDT, 
             data = pld)

summary(pl)

plot(pl)


