Fetch ClimMob data using an API key
================
Kaue de Sousa
23 April, 2019

Preparing the data
------------------

``` r
#library("devtools")
#install_github("kauedesousa/ClimMobTools", upgrade = "never")
library("ClimMobTools")
library("jsonlite")
library("httr")
```

Here I show some examples for a test user in [ClimMob](https://climmob.net/blog/)

``` r
# the user's API key 
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"
```

We can use the fuction `ClimMobTools::getProjectCM` to get our list of projects:

``` r
myprojects <- getProjectsCM(key)

myprojects
```

    ## # A tibble: 5 x 4
    ##   project_id name               status creation_date      
    ##   <chr>      <chr>               <int> <chr>              
    ## 1 flowers    Flowers wageningen      1 2019-04-17 07:24:32
    ## 2 chocolates Chocolate brand         1 2019-04-14 11:44:58
    ## 3 breadwheat Breadwheat              1 2019-03-27 18:32:31
    ## 4 wageningen Wageningen April        1 2019-02-28 08:25:52
    ## 5 colours    Test colours            1 2019-02-12 11:30:44

Then get the data available in each project using the function `ClimMobTools::getDataCM`. Let's start with project "chocolates", which is project where each participant received four items to test. Then reported the performance of all of them by indicanting which one get the the Position 1, Position 2 and so on.

``` r
mydata <- getDataCM(key, project = "chocolates")

print(mydata)
```

    ## # A tibble: 840 x 4
    ##       id moment       variable          value                              
    ##    <int> <fct>        <chr>             <chr>                              
    ##  1     1 package      participant_name  One                                
    ##  2     1 package      item_A            Freia                              
    ##  3     1 package      item_B            Choklad Mork                       
    ##  4     1 package      item_C            Choklad Ljus                       
    ##  5     1 registration surveyid          52527dad-6978-4508-ad87-507a1a5093~
    ##  6     1 registration survey_deviceimei 358624090903126                    
    ##  7     1 registration survey_start      2019-04-17 12:35:00                
    ##  8     1 registration survey_end        2019-04-17 12:36:09                
    ##  9     1 registration participant_name  One                                
    ## 10     1 registration age               0                                  
    ## # ... with 830 more rows

We can also get the data in the wide format by enabling the argument `pivot.wider`

``` r
mydata <-  getDataCM(key, "chocolates", pivot.wider = TRUE)

print(mydata)
```

    ## # A tibble: 30 x 29
    ##       id package_partici~ package_item_A package_item_B package_item_C
    ##    <int> <chr>            <chr>          <chr>          <chr>         
    ##  1     1 One              Freia          Choklad Mork   Choklad Ljus  
    ##  2     2 Abdul S.         Rema Lys koke  Choklad Ljus   Rema Mork koke
    ##  3     3 Rosemary         Choklad Mork   Rema Mork koke Rema Lys koke 
    ##  4     4 Mary E.          Choklad Ljus   Rema Lys koke  Freia         
    ##  5     5 Ivania           Rema Mork koke Freia          Choklad Mork  
    ##  6     6 Mulatu           Freia          Rema Lys koke  Rema Mork koke
    ##  7     7 Sumon            Rema Lys koke  Choklad Mork   Choklad Ljus  
    ##  8     8 Ayomide          Choklad Mork   Rema Mork koke Freia         
    ##  9     9 Orn              Choklad Ljus   Freia          Rema Mork koke
    ## 10    10 "Omar "          Rema Lys koke  Choklad Ljus   Choklad Mork  
    ## # ... with 20 more rows, and 24 more variables:
    ## #   registration_surveyid <chr>, registration_survey_deviceimei <chr>,
    ## #   registration_survey_start <chr>, registration_survey_end <chr>,
    ## #   registration_participant_name <chr>, registration_age <chr>,
    ## #   registration_gender <chr>, registration_country <chr>,
    ## #   assessment_surveyid <chr>, assessment_survey_deviceimei <chr>,
    ## #   assessment_survey_start <chr>, assessment_survey_end <chr>,
    ## #   assessment_sweetness_pos <chr>, assessment_sweetness_neg <chr>,
    ## #   assessment_bitterness_pos <chr>, assessment_bitterness_neg <chr>,
    ## #   assessment_darkcolor_pos <chr>, assessment_darkcolor_neg <chr>,
    ## #   assessment_cocoacontent_pos <chr>, assessment_cocoacontent_neg <chr>,
    ## #   assessment_overallperf_pos <chr>, assessment_overallperf_neg <chr>,
    ## #   assessment_lon <chr>, assessment_lat <chr>

By disabling the argument `tidydata` you get the data with their original names from the ODK file. This is used internally to analyse the data in the [ClimMob](https://climmob.net/blog/) portal.

``` r
mydata <- getDataCM(key, "chocolates", tidynames = FALSE)

print(mydata)
```

    ## # A tibble: 840 x 4
    ##       id moment       variable           value                             
    ##    <int> <fct>        <chr>              <chr>                             
    ##  1     1 package      farmername         One                               
    ##  2     1 package      item_A             Freia                             
    ##  3     1 package      item_B             Choklad Mork                      
    ##  4     1 package      item_C             Choklad Ljus                      
    ##  5     1 registration REG_surveyid       52527dad-6978-4508-ad87-507a1a509~
    ##  6     1 registration REG_clm_deviceimei 358624090903126                   
    ##  7     1 registration REG_clm_start      2019-04-17 12:35:00               
    ##  8     1 registration REG_clm_end        2019-04-17 12:36:09               
    ##  9     1 registration REG_farmername     One                               
    ## 10     1 registration REG_age            0                                 
    ## # ... with 830 more rows

The project "colours" is a project in the standar *tricot* method, where each participant receives three items and report which one had the *best* and *worst* performance.

``` r
mydata <- getDataCM(key, "colours")

print(mydata)
```

    ## # A tibble: 300 x 4
    ##       id moment       variable          value                              
    ##    <int> <fct>        <chr>             <chr>                              
    ##  1     1 package      participant_name  F1                                 
    ##  2     1 package      item_A            Yellow                             
    ##  3     1 package      item_B            Black                              
    ##  4     1 package      item_C            Red                                
    ##  5     1 registration surveyid          aadd4f2b-69b6-4096-8bf5-36a29dba7f~
    ##  6     1 registration survey_deviceimei 358624090903126                    
    ##  7     1 registration survey_start      2019-02-12 13:15:33                
    ##  8     1 registration survey_end        2019-02-12 13:15:59                
    ##  9     1 registration participant_name  F1                                 
    ## 10     1 registration lon               11.179963043445964                 
    ## # ... with 290 more rows

The project "wageningen" is a valid project but has no data. It returns a smooth message.

``` r
mydata <- getDataCM(key, "wageningen")
```

    ## 
    ## Project 'wageningen' was found but has no associated data.
