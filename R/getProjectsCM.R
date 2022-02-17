#' Set server URL
#' This will set the server URL following the argument server
#' in the main functions
#' @param server the server name
#' @param extension a character for the extension in the API call
#' @noRd
.set_url <- function(server = "climmob3", extension = NULL){
  
  other_server <- c("1000farms", "avisa", "rtb", "1000FARMS", "AVISA")
  
  known <- server %in% other_server
  
  if (known) {
    
    url <- paste0("https://", server, ".climmob.net/api/", extension)
    
  }
  
  if (server == "testing") {
    
    url <- paste0("https://", server, ".climmob.net/climmob3/api/", extension)
    
  }
  
  if (server == "climmob3") {
    
    url <- paste0("https://climmob.net/climmob3/api/", extension)
    
  }
  
  if (isFALSE(known) & isFALSE(server == "climmob3") & isFALSE(server == "testing")) {
    
    stop("You are trying to reach an unknown server, please choose between '", 
         paste(c("climmob", other_server), collapse = "', '"), "'\n")
    
  }
  
  return(url)
  
}


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
#' @details 
#' \code{server}: the default server is "climmob" used for clients of 
#' \url{https://climmob.net/climmob3/}, other options are:
#' 
#'  "1000farms" for clients of \url{https://1000farms.climmob.net/} 
#'  
#'  "rtb" for clients of \url{https://rtb.climmob.net/}
#' 
#' @return A data.frame with the variables:
#' \item{project_id}{the project's id}
#' \item{project_name}{the project's name}
#' \item{user_owner}{the account name that owns the project}
#' \item{country}{the country of project's implementation}
#' \item{status}{the current status}
#' \item{creation_date}{date where the project was created}
#' @examplesIf interactive()
#' # This function only works with an API key
#' # the API key can be obtained once a free ClimMob account
#' # is created via https://climmob.net/
#' 
#' my_key <- "92cec84d-44f5-4858-9ef0-bd872496311c"
#' 
#' getProjectsCM(key = my_key, server = "testing")
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
  
  if (length(dat) == 0) {
    return(cat("No project associated with this API key \n"))
  }
  
  progress <- dat$progress
  
  owner <- dat$owner
  
  names(owner) <- paste0("owner_", names(owner))
  
  dat <- cbind(dat, progress, owner)
  
  dat <- dat[,c("project_cod", "project_name", "owner_user_name", "project_cnty",
                 "project_regstatus","project_creationdate")]

  names(dat) <- c("project_id", "project_name", "user_owner",
                  "country", "status","creation_date")
  
  dat$status <- with(dat, ifelse(status == 1, "active",
                                 ifelse(status == 2, "concluded", "not_started")))
  
  dat$creation_date <- with(dat, as.Date(creation_date, origin = "1970-01-01"))

  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  
  class(dat) <- union("CM_df", class(dat))
  
  return(dat)
  
}



