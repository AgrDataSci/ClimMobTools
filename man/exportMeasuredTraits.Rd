% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportMeasuredTraits.R
\name{exportMeasuredTraits}
\alias{exportMeasuredTraits}
\title{Extract and format measured traits}
\usage{
exportMeasuredTraits(x)
}
\arguments{
\item{x}{An object of class \code{CM_list} containing raw ClimMob trial data.}
}
\value{
A data frame with one row per genotype observation, including block ID, plot, 
genotype name, collection moment, trait name, value, and value type.
}
\description{
Extracts non-ranking, plot-level trait data from a ClimMob trial object. The function 
detects variables ending in "_a", "_b", or "_c", infers the trait and collection moment,
and reshapes the data into long format for analysis.
}
\seealso{
Other export functions: 
\code{\link{exportBlockData}()},
\code{\link{exportTrialMetadata}()},
\code{\link{exportTricotRanks}()},
\code{\link{exportVariablesDescription}()}
}
\concept{export functions}
