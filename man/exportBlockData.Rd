% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportBlockData.R
\name{exportBlockData}
\alias{exportBlockData}
\title{Export block-level (participant) data}
\usage{
exportBlockData(x)
}
\arguments{
\item{x}{An object of class \code{CM_list} containing raw ClimMob trial data.}
}
\value{
A data.frame with one row per block (participant),
 excluding PII and ranking data.
}
\description{
Creates a wide-format data.frame of participant (block) data for external export. 
The output excludes ranking variables and personally identifiable information (PII), 
such as names, phone numbers, and location metadata. Optionally includes geolocation 
if available.
}
\seealso{
Other export functions: 
\code{\link{exportMeasuredTraits}()},
\code{\link{exportTrialMetadata}()},
\code{\link{exportTricotRanks}()},
\code{\link{exportVariablesDescription}()}
}
\concept{export functions}
