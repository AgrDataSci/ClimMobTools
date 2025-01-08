## ----setup, include=FALSE-----------------------------------------------------
library("ClimMobTools")
library("PlackettLuce")

## ----fetch, message=FALSE, eval=TRUE, echo=TRUE-------------------------------
library("ClimMobTools")
library("PlackettLuce")

# the API key
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

dat <- getDataCM(key = key,
                 project = "breadwheat",
                 userowner = "gosset",
                 pivot.wider = TRUE)


names(dat) <- gsub("firstassessment_|package_|lastassessment_|registration_", "",
                   names(dat))


## ----plrankings, message=FALSE, eval=TRUE, echo=TRUE--------------------------
R = rankTricot(dat, 
                items = c("item_A","item_B","item_C"), 
                input = c("overallperf_pos","overallperf_neg"))

mod = PlackettLuce(R)

summary(mod)

