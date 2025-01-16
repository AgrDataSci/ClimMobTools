#' Get ClimMob data
#'
#' Fetch the data from a ClimMob project using an application 
#'  programming interface (API) key
#'
#' @rdname getDataCM
#' @family GET functions
#' @author Kauê de Sousa
#' @param project character, the project id
#' @param userowner character, username of project's owner
#' @param as.data.frame logical, to return a data frame, as.data.frame = FALSE returns a list
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
#' \code{server}: options are: "climmob" or "1000farms" 
#' 
#' @examplesIf interactive()
#' 
#' # This function only works with an API key
#' # the API key can be obtained from your ClimMob account
#'  
#' library("ClimMobTools")
#' my_key = "d39a3c66-5822-4930-a9d4-50e7da041e77"
#' 
#' getDataCM(key = my_key,
#'           project = "breadwheat",
#'           userowner = "gosset",
#'           server = "climmob3")
#'           
#' # get in the wide format
#' 
#' getDataCM(key = my_key,
#'           project = "breadwheat",
#'           userowner = "gosset",
#'           server = "climmob3",
#'           pivot.wider = TRUE)
#' 
#' @importFrom httr accept_json content RETRY
#' @importFrom jsonlite fromJSON
#' @export
getDataCM = function(key, 
                     project,
                     userowner,
                     as.data.frame = TRUE, 
                     as.text = FALSE,
                     server = "climmob3", ...){
  
  
  dots = list(...)
  
  url = .set_url(server, extension = "readDataOfProject?Body={}&Apikey={}")
  
  cmdata = httr::RETRY(verb = "GET", 
                       url = url,
                       query = list(Body = paste0('{"project_cod":"', project, '",
                                                   "user_owner":"',userowner,'"}'),
                                    Apikey = key),
                       httr::accept_json(), 
                       terminate_on = c(403, 404))
  
  cmdata = httr::content(cmdata, as = "text")
  
  if (as.text) {
    return(cmdata)
  }
  
  cmdata = jsonlite::fromJSON(cmdata)
  
  # check if the given project has data
  # if not then return a warning message
  if (length(cmdata) < 7) {
    pstring = paste0("'", project, "'")
    message("Project ", pstring, " was found but has no associated data. \n")
    return(list())
  }
  
  class(cmdata) = union("CM_list", class(cmdata))
  
  # if required, coerce to a data frame
  if (isTRUE(as.data.frame)) {
    cmdata = as.data.frame(x = cmdata, ...)
  }
  
  return(cmdata)
  
}
