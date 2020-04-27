ClimMobTools 0.3.4 (2020-04-27)
=========================
### CHANGES IN BEHAVIOUR
* Imports 'gosset', 'PlackettLuce' and 'climatrends' by default
* Use `httr::RETRY` instead of `httr::GET` as suggested by Anna Vasylytsya
* A print method is added
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