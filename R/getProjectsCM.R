#' Get ClimMob projects 
#'
#' Fetch the status of ClimMob projects
#'
#' @author Kauê de Sousa
#' @family GET functions
#' @param key a character for the user's application programming 
#'  interface (API) key
#' @param server optional, a character to select from which server
#'  the data will be retrieved. See details
#' @param ... additional arguments passed to methods. See details
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
#'  
#' @details 
#' \code{server}: the default server is "climmob" used for clients of 
#' https://climmob.net/climmob3/, other options are:
#' 
#'  "avisa" for clients of https://avisa.climmob.net/ 
#'  
#'  "rtb" for clients of https://rtb.climmob.net/
#'  
#'  "testing" for clients of https://testing.climmob.net/climmob3/
#'  
#' 
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
getProjectsCM <- function(key, server = "climmob3", ...){
  
  dots <- list(...)
  
  url <- .set_url(server, extension = "readProjects?Apikey=")
  
  dat <- httr::RETRY(verb = "GET",
                     url = url,
                     query = list(Apikey = key),
                     httr::accept_json(),
                     terminate_on = c(403, 404))

  dat <- httr::content(dat, as = "text")
  
  dat <- jsonlite::fromJSON(dat)
  
  progress <- dat$progress
  
  dat <- cbind(dat, progress)
  
  dat <- dat[,c("project_cod","project_name", "project_cnty",
                 "project_regstatus","project_creationdate",
                 "project_numobs", "regtotal","lastreg")]

  names(dat) <- c("project_id","name", "country", "status","creation_date",
                  "intended_participants", "registered_participants",
                  "last_registration_activity")
  
  dat$status <- with(dat, ifelse(status == 1, "active",
                                 ifelse(status == 2, "concluded", "not_started")))
  
  dat$creation_date <- with(dat, as.Date(creation_date, origin = "1970-01-01"))

  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  
  class(dat) <- union("CM_df", class(dat))
  
  return(dat)
  
}



#' Set server URL
#' This will set the server URL following the argument server
#' in the main functions
#' @param server the server name
#' @param extension a character for the extension in the API call
#' @noRd
.set_url <- function(server = "climmob3", extension = NULL){
  
  other_server <- c("avisa", "rtb", "testing")
  
  known <- server %in% other_server
  
  if (known) {
    
    url <- paste0("https://", server, ".climmob.net/api/", extension)
    
  }
  
  if (server == "climmob3") {
    
    url <- paste0("https://climmob.net/climmob3/api/", extension)
    
  }
  
  if (isFALSE(known) & isFALSE(server == "climmob3")) {
    
      stop("You are trying to reach an unknown server, please choose between '", 
         paste(c("climmob", other_server), collapse = "', '"), "'\n")
    
  }
  
  return(url)
  
}

.unroll_assessments <- function(x){
  
}

