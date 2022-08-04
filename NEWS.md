ClimMobTools 0.4.5 (2021-02-14)
=========================

### IMPROVEMENTS

* Add argument `userowner =` to  `getDataCM()` to specify the owner of a given ClimMob project
* Fix API call `getProjectsCM()`


ClimMobTools 0.4.3 (2021-11-20)
=========================

### BUG FIXES

* Fix an issue in `randomise()` to check for unbalanced designs
* Remove dependency to "PlackettLuce" and "climatrends" to avoid errors in CRAN check for Windows


ClimMobTools 0.4.2 (2021-11-08)
=========================

### IMPROVEMENTS

* `randomise()` deals with unbalanced proportions of technologies, meaning that we can test a whole set of technologies even when not all have equal availability 
* `rmGeoIdentity()` is added to offer an approach to remove geographical identity of participants
* `rankTricot()` builds a PlackettLuce rankings using the tricot data 


ClimMobTools 0.3.9 (2021-04-12)
=========================

### IMPROVEMENTS

* New function `getProjectProgress()` returns the progress in a given project

### BUG FIXES

* Fix the function to set the URLs for alternative servers in ClimMob


ClimMobTools 0.3.7 (2021-03-22)
=========================

### IMPROVEMENTS

* Method for `as.data.frame()` now handles ClimMob data without the assessment info.
* `as.data.frame()` decodes values from categorical variables
* `as.data.frame()` decodes ties in the ranking data 
* Handle alternative server addresses

ClimMobTools 0.3.5 (2020-05-08)
=========================

### CHANGES IN BEHAVIOUR

* Imports 'PlackettLuce' and 'climatrends' by default
* Use `httr::RETRY()` instead of `httr::GET()` as suggested by Anna Vasylytsya
* A `print()` method is added
* New integration with the 'testing' server enabling to gather data from this server with the argument `server`
* Minor changes in documentation

ClimMobTools 0.3.2 (2020-03-16)
=========================

### CHANGES IN BEHAVIOUR

* Migrating functions from **ClimMobTools** to **gosset** and **climatrends**. ClimMobTools will keep only the functions exclusively related to the 'ClimMob' platform. Other functions are transferred to **gosset** and **climatrends** to provide a better environment for data handling, analysis and visualization.
* Retain function `getDataCM()`, `getProjectCM()`, `randomise()` and `seed_need()`
* Changes in package description
* A S3 method `as.data.frame()` is provided to coerce CM_list into a data frame
* Internal functions for pivoting data.frames to avoid dependencies
* Change license to MIT

CliMobTools 0.2.8
=========================

### IMPROVEMENTS

* `temperature` and `rainfall` now deals with one single lonlat point 

CliMobTools 0.2.7
=========================

### IMPROVEMENTS

* Update `build_rankings` to work with the new implementations of PlackettLuce v0.2-8 

### CHANGES IN BEHAVIOUR
* Argument "grouped.rankings" is replaced by "group" in `build_rankings`


CliMobTools 0.1.0
=========================

* GitHub-only release of prototype package.
