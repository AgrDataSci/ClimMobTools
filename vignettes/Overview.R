## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fetch, message = FALSE----------------------------------------------
library("ClimMobTools")
#install.packages("devtools")
#devtools::install_github("kauedesousa/gosset", upgrade = "never")
library("gosset")
library("PlackettLuce")
library("tidyverse")
library("magrittr")

# the API key
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

data <- ClimMobTools::getDataCM(key = key, 
                                project = "breadwheat")

head(data)


## ----tidying-------------------------------------------------------------
# reshape the data into the wide format
# we also remove some variables that refer to the ODK survey employed for the data collection
data %<>% 
  filter(!str_detect(variable, "survey")) %>% 
  group_by(id) %>%
  distinct(id, variable, value) %>%
  spread(variable, value)

## ----plrankings, message = FALSE-----------------------------------------

G <- to_rankings(data, 
                 items = c("item_A","item_B","item_C"), 
                 rankings = c("overallperf_pos","overallperf_neg"),
                 type = "tricot", 
                 grouped.rankings = TRUE)

head(G, 10)


## ----temperature, message = FALSE----------------------------------------

# first we convert the lon lat into numeric
# and the planting dates into Date
data %<>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat),
         plantingdate = as.Date(plantingdate, 
                                format = "%Y-%m-%d"))

# then we get the temperature indices
# get some variables to include in the model
# from the planting date to the next 120 days
temp <- temperature(data[c("lon","lat")], 
                    day.one = data["plantingdate"], 
                    span = 120)


modeldata <- cbind(G, temp)

head(modeldata)


## ----plmodel, message=FALSE----------------------------------------------
tree <- pltree(G ~ ., data = modeldata, npseudo = 5)

print(tree)

## ----pltree, echo=FALSE, fig.height=4, fig.width=8, message=FALSE--------
plot(tree)

