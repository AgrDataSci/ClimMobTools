#' Get project progress
#'
#' Fetch the progress of a ClimMob project
#'
#' @author Kauê de Sousa
#' @family GET functions
#' @param key a character for the user's application programming 
#'  interface (API) key
#' @param project a character with the id of one or more projects
#' @param server optional, a character to select from which server
#'  the data will be retrieved. See details
#' @param ... additional arguments passed to methods. See details
#' @return A data frame with the ClimMob projects 
#' \item{project_id}{the project unique id}
#' \item{name}{the project name}
#' \item{moment}{either the design, registration or data collection}
#' \item{number_obs}{number of observations collected in a given moment}
#' \item{last_activity}{last activity of the given moment}
#'  
#' @details 
#' \code{server}: the default server is "climmob" used for clients of 
#' https://climmob.net/climmob3/, other options are:
#' 
#'  "1000farms" for clients of https://1000farms.climmob.net/ 
#'  
#'  "rtb" for clients of https://rtb.climmob.net/
#'  
#' @examples
#' # This function will not work without an API key  
#' # the user API key can be obtained once a free ClimMob account 
#' # is created via https://climmob.net/
#' 
#' # my_key <- "add_your_key"
#' 
#' # my_project <- "project_id"
#' 
#' # getProjectProgress(my_key, my_project)
#' 
#' 
#' @seealso ClimMob website \url{https://climmob.net/}
#' @export
getProjectProgress <- function(key, project, server = "climmob3", ...){
  
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
                "project_numobs", "regtotal","lastreg",
                "assessments")]
  
  
  p <- project %in% dat$project_cod
  
  if (isFALSE(p)) {
    stop("Unknown project '", project, "' please check the project id with getProjectsCM() \n")
  }
  
  p <- which(dat$project_cod %in% project)
  
  assessments <- dat$assessments[[p]]
  
  newnames <- c("project_id", "name", "moment", "number_obs", "last_activity")
  
  if (length(assessments) == 0) {
    progress <- NULL
  } 
  
  if (length(assessments) > 1) {
    
    progress <- data.frame(project_id = dat[p, "project_cod"],
                           project_name = dat[p, "project_name"],
                           assessments[,c("ass_desc","submissions","lastass")])
    
    names(progress) <- newnames
    
  }
  
  design <- dat[p ,c("project_cod","project_name", 
                     "project_numobs", "project_creationdate")]
  
  design$moment <- "Design"
  
  design <- design[c(1:2,5,3:4)]
  
  names(design) <- newnames
  
  regis <- dat[p ,c("project_cod","project_name", 
                    "regtotal","lastreg")]
  
  regis$moment <- "Registration"
  
  regis <- regis[c(1:2,5,3:4)]
  
  names(regis) <- newnames
  
  dat <- rbind(design, regis, progress)
  
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  
  rownames(dat) <- 1:nrow(dat)
  
  class(dat) <- union("CM_df", class(dat))
  
  return(dat)
  
}

