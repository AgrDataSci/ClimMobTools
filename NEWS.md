ClimMobTools 1.6.1 (2025-07-07)
=========================

### BUG FIXES

* Fixes an issue in handling column names with the overall characteristic. 


ClimMobTools 1.6 (2025-07-06)
=========================

### CHANGES IN BEHAVIOUR

* The `as.data.frame.CM_list()` method now defaults to pivot.wider = TRUE, returning a wide-format data frame.
* The `as.data.frame.CM_list()` method has a validation line to only coerce to data.frame objects with more than 1 data point.

### IMPROVEMENTS

* Re-factored internal code structure for improved readability and performance. Introducing modular helper functions to support the `as.data.frame.CM_list()` method, improving code clarity and reusability
  - `.replace_multichoice_codes()`
  - `.handle_geolocation_columns()`
  - `.replace_rankings()`
  - `.decode_assessments()`
  - `.merge_package_info()`
  - `.clean_column_names()`
  - `.drop_odk_system_fields()`
  - `.reorder_columns()`
  - `.safe_extract()`
* Enhanced column name cleaning logic when `as.data.frame.CM_list()` tidynames = TRUE, producing more consistent and informative names.
* Restructured column ordering in the output to prioritize project, package, and registration variables.
* Simplified logic for reshaping and labelling assessment variables, removing the dependency on `.set_long()` when pivot.wider = TRUE.
* Reordered output columns to prioritize key identifiers (`project`, `package`, `registration`, and assessments).

### BUG FIXES

* Fixed an issue where geolocation columns containing only missing values (e.g., `_longitude`, `_latitude`) would lead to the final data frame having zero columns. These are now removed conditionally and safely.

ClimMobTools 1.5 (2025-03-31)
=========================

### CHANGES IN BEHAVIOUR

* `getProjectsCM()` adds a new variable in the output `project_code` that will represent the previous `project_id`. The new variable `project_id` will represent the id from the ClimMob server database not the id from the user list of projects. 

ClimMobTools 1.4 (2025-01-15)
=========================

### BUG FIXES

* Fixes issues in vignetted building reported by CRAN.

ClimMobTools 1.2 (2024-01-25)
=========================

### BUG FIXES

* Improves indexing of technologies aliases in `ClimMobTools:::as.data.frame()`

ClimMobTools 1.1 (2023-11-28)
=========================

### BUG FIXES

* Fix an issue in `.smart.round()` to handle `NA` in the `randomize()` function  

ClimMobTools 1.0 (2023-11-10)
=========================

### BUG FIXES

* Fix an issue in `randomize()` to allocate blocks 


ClimMobTools 0.4.6 (2022-08-11)
=========================

### IMPROVEMENTS

* Adds the function `getTraitList()`
* Fix the call in `getProjectsCM()` to adapt to the new version of ClimMob

ClimMobTools 0.4.5 (2022-02-14)
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
