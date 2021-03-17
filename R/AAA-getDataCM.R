#' Get ClimMob data
#'
#' Fetch the data from a ClimMob project using an application 
#'  programming interface (API) key
#'
#' @rdname getDataCM
#' @family GET functions
#' @author Kauê de Sousa
#' @param project a character for the project id
#' @param as.data.frame logical, to return a data frame
#' @param ... additional arguments passed to methods. See details
#' @inheritParams getProjectsCM
#' @return An object of class 'CM_list' or a data.frame with class "CM_df" with the 
#' variables:
#' \item{id}{the participant's package id}
#' \item{moment}{the data collection moment}
#' \item{variable}{the variable name}
#' \item{value}{the value for each variable}
#' @details 
#' Additional arguments: 
#' 
#' \code{server}: a character to select from which server the data will be retrieved,
#'   "production" (the default) or an alternative server 
#' 
#' @examples
#' \dontrun{
#' 
#' # This function will not work without an API key  
#' # the user API key can be obtained once a free ClimMob account 
#' # is created via https://climmob.net/
#' 
#' my_key <- "add_your_key"
#' my_project <- "my_climmob_project"
#' 
#' data <- getDataCM(key = my_key, project = my_project)
#' 
#' }
#' 
#' @seealso ClimMob website \url{https://climmob.net/}
#' @importFrom httr accept_json content RETRY
#' @importFrom jsonlite fromJSON
#' @export
getDataCM <- function(key, 
                      project,
                      as.data.frame = TRUE, 
                      server = NULL, ...){
  
  
  dots <- list(...)
  
  url <- "https://climmob.net/climmob3/api/readDataOfProject?Body={}&Apikey={}"
  
  if (!is.null(server)) {
    
    url <- gsub("https://", paste0("https://", server, "."), url)
    
  }
  
  cmdata <- httr::RETRY(verb = "GET", 
                        url = url,
                        query = list(Body = paste0('{"project_cod":"', project, '"}'),
                                    Apikey = key),
                        httr::accept_json(), 
                        terminate_on = c(403, 404))
  
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
  }
  
  return(cmdata)
  
}
