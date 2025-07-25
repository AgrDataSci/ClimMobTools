% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportTrialMetadata.R
\name{exportTrialMetadata}
\alias{exportTrialMetadata}
\title{Generate metadata for tricot trial export}
\usage{
exportTrialMetadata(x)
}
\arguments{
\item{x}{An object of class \code{CM_list} containing raw ClimMob trial data.}
}
\value{
A named list containing metadata fields required for tricot data export,
including trial name, description, dates, bounding box, participant counts, 
and institutional information.
}
\description{
Extracts and compiles key metadata from a ClimMob trial object to support
standardized data documentation and publication, including fields such as 
trial identifiers, geographic bounding box, crop and taxon information, 
and participant statistics.
}
\seealso{
Other export functions: 
\code{\link{exportBlockData}()},
\code{\link{exportMeasuredTraits}()},
\code{\link{exportTricotRanks}()},
\code{\link{exportVariablesDescription}()}
}
\concept{export functions}
