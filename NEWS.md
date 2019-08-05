# ClimMobTools 0.2-3

## New Features

* Function `build_rankings` is added to convert the tricot data into an object of class rankings from **PlackettLuce**.
* Function `seed_need` is added to calculate the required amount of seeds/seedlings needed for the tricot project

## CRAN issues

* please makew the title title case *>>>* the title is correct **ClimMobTools**
* gosset is not in a standard repository nor do you have declared where to get this from. See the CRAN policies how to do that. No reason to put this in "dontrun{}". But protect the example to only run if gosset is available ion the user's machine. *>>>* **gosset** is a package under development. All dependencies to **gosset** were removed and function `build_rankings` was added into **ClimMobTools** as a replacement to `gosset::to_rankings`. 
* Perhaps also add an example that is easily executable by everyone even without gosset? *>>>* a practical example was added, but other functions `rainfall`, `ETo` and `GDD` (which use the same approach as `temperature`) has practical examples in "dontrun{}" to avoid elapsed time during CRAN checks. 


# ClimMobTools 0.2-2

## New Features

* Tests coverage > 90%
* Validations in `temperature`, `rainfall`, `getDataCM`, and `ETo`
* A single vignette for the package Overview

## CRAN issues

* Non canonical URLs in inst/doc/Environmental_indices.html
* Elaborate what the platform [ClimMob] is, and add web reference
* The Description field should not start with the package name
* Single cote all software names in the Description file



# ClimMobTools 0.2-1

## New Features

* Add vignettes.
* Add `getDataCM`, and `getProjectsCM` to fetch data using ClimMob API.


# ClimMobTools 0.1-0

* GitHub-only release of prototype package.