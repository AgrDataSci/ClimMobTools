# Instructions for AI agents

ClimMobTools is the R client for the ClimMob platform (https://climmob.net), which runs tricot trials. It downloads project data through the ClimMob API, designs the randomised packages for a trial, and prepares the data for analysis. People share this repository with an assistant either to **work with their ClimMob data** or to **change the package**. Find out which before you start.

## Before you write any code

1. Check the installed version with `packageVersion("ClimMobTools")` and read the help of every function you use. Arguments change between versions, so do not rely on memory.
2. The analysis of rankings happens in gosset and PlackettLuce, not here. For methods and evidence on tricot, read the knowledge base at https://github.com/AgrDataSci/tricot-knowledge-base (start with its `AGENTS.md`).

## API keys and personal data

- **Never write an API key in a script, a test, an issue or a message.** Ask the user to store it in the environment variable `CLIMMOB_KEY` and read it with `Sys.getenv("CLIMMOB_KEY")`.
- ClimMob data hold names, phone numbers and locations of participants. Never put real data in examples, tests or issues. Use simulated data.
- Before sharing coordinates, displace them with `rmGeoIdentity()` and **do not set or publish its seed**. With the seed, anyone can recover the exact locations.

## Which function for which task

| Task | Function |
|---|---|
| List the projects a key can access | `getProjectsCM()` |
| Download the data of a project | `getDataCM()`, then `as.data.frame()` options for wide or long format |
| Check how many submissions each data collection moment has | `getProjectProgress()` |
| List the traits assessed in a project | `getTraitList()` |
| Export tidy tables for analysis | `exportTricotRanks()`, `exportMeasuredTraits()`, `exportBlockData()`, `exportTrialMetadata()`, `exportVariablesDescription()` |
| Design the packages of a new trial (which items each participant receives) | `randomise()` or `randomize()` |
| Build Plackett-Luce rankings from tricot data | prefer `gosset::rank_tricot()`; `rankTricot()` here is a copy kept for compatibility |
| Displace coordinates before sharing | `rmGeoIdentity()` |

The `server` argument selects the ClimMob instance (for example `"climmob3"`, `"1000farms"`, `"avisa"`, `"rtb"`).

## Trial design

`randomise()` allocates items to packages so that items appear about equally often, are spread across the positions A, B and C, and form a connected comparison network. `availability` caps how many packages each item can go to, and `props` sets unequal shares. Call `set.seed()` before it when the design must be reproducible, and keep the design with the project records.

## Changing the package

- Use `=` for assignment in new code. Use base R where possible.
- Document with roxygen and run `devtools::document()`. Never edit files in `man/` by hand.
- Every fix needs a test in `tests/testthat/` that fails before the fix and passes after it. Tests that call the API must use `skip_without_key()` (see `tests/testthat/helper-key.R`). Run `devtools::test()` and `devtools::check()`, and report what was actually run.
- Record changes in `NEWS.md`. Keep changes backwards compatible, or record the break and the reason.
- ClimMob-analysis (https://github.com/AgrDataSci/ClimMob-analysis) depends on this package. Check it when changing what a function returns.
- Work on a branch and open a pull request. Do not merge without review and passing checks.

## Where to find more

- Reference and vignettes: https://agrdatasci.github.io/ClimMobTools/
- Issues: https://github.com/AgrDataSci/ClimMobTools/issues
