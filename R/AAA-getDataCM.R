#' Get ClimMob data
#'
#' Fetch the data from a ClimMob project using an application 
#'  programming interface (API) key
#'
#' @rdname getDataCM
#' @family GET functions
#' @author Kauê de Sousa
#' @param project a character for the project id
#' @param userowner a character for user name of project's owner
#' @param as.data.frame logical, to return a data frame
#' @param as.text logical, to return a text file that can be parsed to json
#' @param ... additional arguments passed to methods. See details
#' @inheritParams getProjectsCM
#' @return An object of class 'CM_list' or a text file or 
#'  a data.frame with class "CM_df" with the variables:
#' \item{id}{the participant's package id}
#' \item{moment}{the data collection moment}
#' \item{variable}{the variable name}
#' \item{value}{the value for each variable}
#' @details 
#' \code{server}: the default server is "climmob" used for clients of 
#' \url{https://climmob.net/climmob3/}, other options are:
#' 
#'  "1000farms" for clients of \url{https://1000farms.climmob.net/} 
#'  
#'  "rtb" for clients of \url{https://rtb.climmob.net/}
#' 
#' @examplesIf interactive()
#' 
#' # This function only works with an API key
#' # the API key can be obtained once a free ClimMob account
#' # is created via https://climmob.net/
#' 
#' my_key <- "92cec84d-44f5-4858-9ef0-bd872496311c"
#' 
#' getDataCM(key = my_key,
#'           project = "testmark",
#'           userowner = "kauedesousa",
#'           server = "testing")
#' 
#' @seealso ClimMob website \url{https://climmob.net/}
#' @importFrom httr accept_json content RETRY
#' @importFrom jsonlite fromJSON
#' @export
getDataCM <- function(key, 
                      project,
                      userowner,
                      as.data.frame = TRUE, 
                      as.text = FALSE,
                      server = "climmob3", ...){
  
  
  dots <- list(...)
  
  url <- .set_url(server, extension = "readDataOfProject?Body={}&Apikey={}")

  cmdata <- httr::RETRY(verb = "GET", 
                        url = url,
                        query = list(Body = paste0('{"project_cod":"', project, '",
                                                   "user_owner":"',userowner,'"}'),
                                     Apikey = key),
                        httr::accept_json(), 
                        terminate_on = c(403, 404))
  
  cmdata <- httr::content(cmdata, as = "text")
  
  if (as.text) {
    return(cmdata)
  }
  
  cmdata <- jsonlite::fromJSON(cmdata)
  
  # check if the given project has data
  # if not then return a warning message
  if (length(cmdata) < 7) {
    pstring <- paste0("'",project,"'")
    message("Project ", pstring, " was found but has no associated data. \n")
    return(project)
  }
  
  class(cmdata) <- union("CM_list", class(cmdata))
  
  # if required, coerce to a data frame
  if (isTRUE(as.data.frame)) {
    cmdata <- as.data.frame(x = cmdata, ...)
  }
  
  return(cmdata)
  
}
