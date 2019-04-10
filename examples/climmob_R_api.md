Fetch ClimMob data using an API key
================
Kauê de Sousa
10 April, 2019

Preparing the data
------------------

``` r
#library("devtools")
#install_github("kauedesousa/gosset", upgrade = "never")
library("gosset")
library("jsonlite")
library("httr")
```

Here I show some examples for a test user in [ClimMob](https://climmob.net/blog/)

``` r
# the user's API key 
key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"
```

We can use the fuction `gosset::getProjectCM` to get our list of projects:

``` r
myprojects <- getProjectsCM(key)

myprojects
```

    ## # A tibble: 4 x 4
    ##   project_id name                  status creation_date      
    ##   <chr>      <chr>                  <int> <chr>              
    ## 1 breadwheat Breadwheat                 1 2019-03-27 18:32:31
    ## 2 wageningen Wageningen April           1 2019-02-28 08:25:52
    ## 3 chocolate  Test chocolate brands      1 2019-02-22 09:04:19
    ## 4 colours    Test colours               1 2019-02-12 11:30:44

Then get the data available in each project using the function `gosset::getDataCM`. Let's start with project "chocolate", which is project where each participant received four items to test. Then reported the performance of all of them by indicanting which one get the the Position 1, Position 2 and so on.

``` r
mydata <- getDataCM(key, project = "chocolate")

print(mydata)
```

    ## # A tibble: 216 x 4
    ##       id moment       variable          value                              
    ##    <int> <fct>        <chr>             <chr>                              
    ##  1     1 package      participant_name  Maria Johnsen                      
    ##  2     1 package      item_A            Ritter Sport                       
    ##  3     1 package      item_B            Garoto                             
    ##  4     1 package      item_C            Ferrero                            
    ##  5     1 package      item_D            Toblerone                          
    ##  6     1 registration surveyid          566e82bf-0d5a-4438-8d8e-6bd882c751~
    ##  7     1 registration survey_deviceimei 358624090903126                    
    ##  8     1 registration survey_start      2019-02-25 09:10:23                
    ##  9     1 registration survey_end        2019-02-25 09:11:39                
    ## 10     1 registration participant_name  Maria Johnsen                      
    ## # ... with 206 more rows

We can also get the data in the wide format by enabling the argument `pivot.wider`

``` r
mydata <-  getDataCM(key, "chocolate", pivot.wider = TRUE)

print(mydata)
```

    ## # A tibble: 6 x 37
    ##      id package_partici~ package_item_A package_item_B package_item_C
    ##   <int> <chr>            <chr>          <chr>          <chr>         
    ## 1     1 Maria Johnsen    Ritter Sport   Garoto         Ferrero       
    ## 2     2 Bjorn Ragnarson  Freia          Lindt          Milka         
    ## 3     3 Ragnar Ragnarson Garoto         Milka          Toblerone     
    ## 4     4 Kjartan Mortroen Ferrero        Toblerone      Freia         
    ## 5     5 Kristin Kristen~ Lindt          Freia          Ritter Sport  
    ## 6     6 Maria Torsen     Milka          Ferrero        Garoto        
    ## # ... with 32 more variables: package_item_D <chr>,
    ## #   registration_surveyid <chr>, registration_survey_deviceimei <chr>,
    ## #   registration_survey_start <chr>, registration_survey_end <chr>,
    ## #   registration_participant_name <chr>, registration_district <chr>,
    ## #   registration_village <chr>, registration_lon <chr>,
    ## #   registration_lat <chr>, assessment1_surveyid <chr>,
    ## #   assessment1_survey_deviceimei <chr>, assessment1_survey_start <chr>,
    ## #   assessment1_survey_end <chr>, assessment1_q1taste_pos1 <chr>,
    ## #   assessment1_q1taste_pos2 <chr>, assessment1_q1taste_pos3 <chr>,
    ## #   assessment1_q1taste_pos4 <chr>, assessment1_q2sweet_pos1 <chr>,
    ## #   assessment1_q2sweet_pos2 <chr>, assessment1_q2sweet_pos3 <chr>,
    ## #   assessment1_q2sweet_pos4 <chr>, assessment1_item_A_vs_local <chr>,
    ## #   assessment1_item_B_vs_local <chr>, assessment1_item_C_vs_local <chr>,
    ## #   assessment1_item_D_vs_local <chr>, assessment1_overallperf_pos1 <chr>,
    ## #   assessment1_overallperf_pos2 <chr>,
    ## #   assessment1_overallperf_pos3 <chr>,
    ## #   assessment1_overallperf_pos4 <chr>, assessment1_lon <chr>,
    ## #   assessment1_lat <chr>

By disabling the argument `tidydata` you get the data with their original names from the ODK file. This is used internally to analyse the data in the [ClimMob](https://climmob.net/blog/) portal.

``` r
mydata <- getDataCM(key, "chocolate", tidynames = FALSE)

print(mydata)
```

    ## # A tibble: 216 x 4
    ##       id moment       variable           value                             
    ##    <int> <fct>        <chr>              <chr>                             
    ##  1     1 package      farmername         Maria Johnsen                     
    ##  2     1 package      item_A             Ritter Sport                      
    ##  3     1 package      item_B             Garoto                            
    ##  4     1 package      item_C             Ferrero                           
    ##  5     1 package      item_D             Toblerone                         
    ##  6     1 registration REG_surveyid       566e82bf-0d5a-4438-8d8e-6bd882c75~
    ##  7     1 registration REG_clm_deviceimei 358624090903126                   
    ##  8     1 registration REG_clm_start      2019-02-25 09:10:23               
    ##  9     1 registration REG_clm_end        2019-02-25 09:11:39               
    ## 10     1 registration REG_farmername     Maria Johnsen                     
    ## # ... with 206 more rows

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

The project "wageningen" is a valid project but has no data. It returns a message.

``` r
mydata <- getDataCM(key, "wageningen")
```

    ## 
    ## Project 'wageningen' was found but has no associated data.
