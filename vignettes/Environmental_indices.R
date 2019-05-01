## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fetchdata, message = FALSE------------------------------------------
library("ClimMobTools")
library("nasapower")
library("tidyverse")
library("magrittr")

# fetch ClimMob data from your ClimMob user account
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

data <- ClimMobTools::getDataCM(key = key, 
                                project = "breadwheat")


# data into wide format
data %<>% 
  filter(!str_detect(variable, "survey")) %>% 
  group_by(id) %>%
  distinct(id, variable, value) %>%
  spread(variable, value)

# convert the lon lat into numeric
# and the planting dates into Date
data %<>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat),
         plantingdate = as.Date(plantingdate, 
                                format = "%Y-%m-%d"))

# use only the first 25 observations
data <- data[1:25, ]


## ----temperature, message=FALSE------------------------------------------
temp <- temperature(data[c("lon","lat")], 
                    day.one = data["plantingdate"], 
                    span = 120)

head(temp, 10)


## ----degreedays, message=FALSE-------------------------------------------

gdd <- GDD(data[c("lon","lat")], 
           day.one = data["plantingdate"],
           degree.days = 1800,
           base = 5)

head(gdd, 10)


## ----temp_gdd, message=FALSE---------------------------------------------
temp <- temperature(data[c("lon","lat")], 
                    day.one = data["plantingdate"], 
                    span = gdd)

head(temp, 10)


## ----rain, message=FALSE-------------------------------------------------
rain <- rainfall(data[c("lon","lat")], 
                 day.one = data["plantingdate"],
                 span = gdd)

head(rain, 10)


## ----rain2, eval=FALSE, message=FALSE------------------------------------
#  rain <- rainfall(data[c("lon","lat")],
#                   day.one = data["plantingdate"],
#                   span = gdd,
#                   days.before = 15)
#  
#  

## ----eto, message=FALSE--------------------------------------------------
eto <- ETo(data[c("lon","lat")], 
           day.one = data["plantingdate"],
           span = gdd,
           lat = data[["lat"]])

head(eto, 10)


