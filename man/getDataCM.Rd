% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AAA-getDataCM.R, R/as.data.frame-climmob_list.R
\name{getDataCM}
\alias{getDataCM}
\alias{as.data.frame.CM_list}
\title{Get ClimMob data}
\usage{
getDataCM(
  key,
  project,
  userowner,
  as.data.frame = TRUE,
  as.text = FALSE,
  server = "climmob3",
  ...
)

\method{as.data.frame}{CM_list}(x, ..., tidynames = TRUE, pivot.wider = FALSE)
}
\arguments{
\item{key}{a character for the user's application programming 
interface (API) key}

\item{project}{a character for the project id}

\item{userowner}{a character for user name of project's owner}

\item{as.data.frame}{logical, to return a data frame}

\item{as.text}{logical, to return a text file that can be parsed to json}

\item{server}{optional, a character to select from which server
the data will be retrieved. See details}

\item{...}{additional arguments passed to methods}

\item{x}{an object of class \code{CM_list}}

\item{tidynames}{logical, \code{TRUE} make clean column names}

\item{pivot.wider}{logical, if \code{TRUE} return a wider object 
where each tricot package is a row}
}
\value{
An object of class 'CM_list' or a text file or 
 a data.frame with class "CM_df" with the variables:
\item{id}{the participant's package id}
\item{moment}{the data collection moment}
\item{variable}{the variable name}
\item{value}{the value for each variable}
}
\description{
Fetch the data from a ClimMob project using an application 
 programming interface (API) key
}
\details{
\code{server}: the default server is "climmob" used for clients of 
\url{https://climmob.net/climmob3/}, other options are:

 "1000farms" for clients of \url{https://1000farms.climmob.net/}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

# This function only works with an API key
# the API key can be obtained once a free ClimMob account
# is created via https://climmob.net/
 
library("ClimMobTools")
my_key <- "ff05a174-28d0-4a40-ab5a-35dc486133a6"

getDataCM(key = my_key,
          project = "beanaru23",
          userowner = "student",
          server = "1000farms")
          
# get in the wide format

getDataCM(key = my_key,
          project = "beanaru23",
          userowner = "student",
          server = "1000farms",
          pivot.wider = TRUE)
\dontshow{\}) # examplesIf}
}
\seealso{
ClimMob website \url{https://climmob.net/}

Other GET functions: 
\code{\link{getProjectProgress}()},
\code{\link{getProjectsCM}()}
}
\author{
Kauê de Sousa
}
\concept{GET functions}
