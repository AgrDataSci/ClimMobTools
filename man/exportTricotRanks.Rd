% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportTricotRanks.R
\name{exportTricotRanks}
\alias{exportTricotRanks}
\title{Extract and format tricot rankings for data export}
\usage{
exportTricotRanks(
  x,
  pattern = c("_pos", "_neg"),
  items = c("package_item_A", "package_item_B", "package_item_C"),
  ...
)
}
\arguments{
\item{x}{An object of class \code{CM_list} containing raw ClimMob trial data.}

\item{pattern}{character, the tricot ranking pattern}

\item{items}{a character or numerical vector for indexing the column(s) 
containing the item names in \code{data}}

\item{...}{Additional arguments passed to \code{getTraitList()}.}
}
\value{
A data frame in long format containing tricot rankings, 
with one row per genotype and trait, including the block identifier, 
plot label (A/B/C), genotype name, trait label, and assigned rank position.
}
\description{
Processes ClimMob ranking data into a standardized long-format data frame
suitable for export to external databases and analysis. For each trait,
the function extracts rank positions per genotype and block
based on participant responses. Compatible with Dataverse and other 
FAIR data publication systems.
}
\seealso{
Other export functions: 
\code{\link{exportBlockData}()},
\code{\link{exportMeasuredTraits}()},
\code{\link{exportTrialMetadata}()},
\code{\link{exportVariablesDescription}()}
}
\concept{export functions}
