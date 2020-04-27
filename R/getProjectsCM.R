#' Get ClimMob projects 
#'
#' Fetch the status of ClimMob projects
#'
#' @author Kauê de Sousa
#' @family GET functions
#' @param key a character for the user's application programming 
#'  interface (API) key
#' @return A data frame with the ClimMob projects 
#' \item{project_id}{the project unique id}
#' \item{name}{the project name}
#' \item{status}{the current status}
#' \item{creation_date}{the project's creation date}
#' \item{intended_participants}{the number of participants the project 
#'  intended to register}
#' \item{registration_progress}{the percentage of intended participants 
#'  which were registered}
#' \item{last_registration_activity}{number of days since the submission 
#'  of the last registration}
#' @examples
#' \dontrun{ 
#' # This function will not work without an API key  
#' # the user API key can be obtained once a free ClimMob account 
#' # is created via https://climmob.net/
#' 
#' my_key <- "add_your_key"
#' 
#' getProjectsCM(key = my_key)
#' 
#' }
#' 
#' @seealso ClimMob website \url{https://climmob.net/}
#' @export
getProjectsCM <- function(key = NULL){

  url <- "https://climmob.net/climmob3/api/readProjects?Apikey="
  
  dat <- httr::RETRY(verb = "GET",
                     url = url,
                     query = list(Apikey = key),
                     httr::accept_json(),
                     terminate_on = c(403, 404))

  dat <- httr::content(dat, as = "text")
  
  dat <- jsonlite::fromJSON(dat)
  
  progress <- dat$progress
  
  dat <- cbind(dat, progress)
  
  dat <- dat[,c("project_cod","project_name",
                 "project_regstatus","project_creationdate",
                 "project_numobs", "regperc","lastreg")]

  names(dat) <- c("project_id","name","status","creation_date",
                  "intended_participants", "registration_progress",
                  "last_registration_activity")
  
  dat$status <- with(dat, ifelse(status == 1, "active",
                                 ifelse(status == 2, "concluded", "not_started")))
  
  dat$creation_date <- with(dat, as.Date(creation_date, origin = "1970-01-01"))

  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  
  class(dat) <- union("CM_df", class(dat))
  
  return(dat)
}
