#' Get ClimMob data
#'
#' Fetch the data from a ClimMob project using an application 
#'  programming interface (API) key
#'
#' @rdname getDataCM
#' @author Kauê de Sousa
#' @param project a character for the project id
#' @param as.data.frame logical, to return a data frame
#' @param ... additional arguments passed to methods
#' @inheritParams getProjectsCM
#' @return An object of class 'CM_list' or a data.frame with the 
#' variables:
#' \item{id}{the participant's package id}
#' \item{moment}{the data collection moment}
#' \item{variable}{the variable name}
#' \item{value}{the value for each variable}
#' @examples
#' \dontrun{
#' 
#' # This function will not work without an API key  
#' # the user API key can be obtained once a free ClimMob account 
#' # is created via https://climmob.net/climmob3/
#' 
#' my_key <- "add_your_key"
#' my_project <- "my_climmob_project"
#' 
#' data <- getDataCM(key = my_key, project = my_project)
#' 
#' }
#' 
#' @seealso \code{\link{getProjectsCM}}
#' @importFrom httr accept_json content GET
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
getDataCM <- function(key = NULL, 
                      project = NULL, 
                      as.data.frame = TRUE, ...){
  
  url <- "https://climmob.net/climmob3/api/readDataOfProject?Body={}&Apikey={}"
  
  cmdata <- httr::GET(url = url,
                      query = list(Body = paste0('{"project_cod":"', project, '"}'),
                                   Apikey = key),
                      httr::accept_json())
  
  cmdata <- httr::content(cmdata, as = "text")
  
  cmdata <- jsonlite::fromJSON(cmdata)
  
  # check if the given project has data
  # if not then return a warning message
  if (length(cmdata) < 7) {
    pstring <- paste0("'",project,"'")
    stop("Project ", pstring, " was found but has no associated data. \n")
  }
  
  class(cmdata) <- union("CM_list", class(cmdata))
  
  # if required, coerce to a data frame
  if (isTRUE(as.data.frame)) {
    cmdata <- as.data.frame(x = cmdata, ...)
    cmdata <- tibble::as_tibble(cmdata)
  }
  
  return(cmdata)
  
}
