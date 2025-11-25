#' Set ClimMob Server URL
#'
#' Helper to construct the base URL for the ClimMob API.
#' The function accepts either a known server name (e.g. `"climmob3"`,
#' `"testing"`, `"1000farms"`, `"avisa"`, `"rtb"`) or a full/base URL
#' (including local servers such as `"http://localhost:8000"`).
#'
#' Server names are matched case-insensitively, so for example
#' `"AVISA"`, `"Avisa"`, and `"avisa"` are treated as the same server.
#'
#' @param server Character string. Either a known ClimMob server name
#'   (e.g. `"climmob3"`, `"testing"`, `"1000farms"`, `"avisa"`, `"rtb"`)
#'   or a full/base URL to a ClimMob-compatible server.
#'   Defaults to `"climmob3"`.
#' @param extension Character string specifying the path extension to be
#'   appended after `"/api/"` in the URL (e.g. `"v1/projects"`).
#'
#' @return A length-one character vector with the full API URL.
#'
#' @keywords internal
#' @examples
#' # Default ClimMob3 server
#' .set_url(extension = "v1/projects")
#'
#' # Testing server (any capitalization works)
#' .set_url("TESTING", "v1/projects")
#'
#' # AVISA server
#' .set_url("avisa", "v1/projects")
#' .set_url("AVISA", "v1/projects")
#'
#' # Local development server
#' .set_url("http://localhost:8000", "v1/projects")
#' @noRd
.set_url = function(server = "climmob3", extension = NULL) {
  
  # Normalize input (so "AVISA", "AviSa", etc. become "avisa")
  server_key = tolower(server)
  
  # Mapping of normalized server names to their base URLs
  predefined = list(
    climmob3   = "https://climmob.net/climmob3",
    testing    = "https://testing.climmob.net/climmob3",
    "1000farms" = "https://1000farms.climmob.net",
    avisa      = "https://avisa.climmob.net",
    rtb        = "https://rtb.climmob.net"
  )
  
  if (server_key %in% names(predefined)) {
    base = predefined[[server_key]]
  } else {
    # Treat as full/local URL
    base = server
  }
  
  url = paste0(base, "/api/", extension)
  return(url)
}


#' Get ClimMob projects 
#'
#' Fetch the status of ClimMob projects
#'
#' @author Kauê de Sousa
#' @family GET functions
#' @param key character, the user's API key
#' @param server character, to indicate from which server the data will be retrieved. See details
#' @param ... additional arguments passed to methods. See details
#' @details 
#' \code{server}: the default server is "climmob" used for clients of 
#' \url{https://climmob.net/climmob3/}, other options are:
#' 
#'  "1000farms" for clients of \url{https://1000farms.climmob.net/} 
#' 
#' @return A data.frame with the variables:
#' \item{project_id}{the ClimMob single id in the server database}
#' \item{project_code}{the project's code from the ClimMob user}
#' \item{project_name}{the project's name}
#' \item{user_owner}{the account name that owns the project}
#' \item{country}{the country of project's implementation}
#' \item{status}{the current status}
#' \item{creation_date}{date where the project was created}
#' @examplesIf interactive()
#' # This function only works with an API key
#' # the API key can be obtained from your ClimMob account
#' 
#' my_key = "ff05a174-28d0-4a40-ab5a-35dc486133a6"
#' 
#' getProjectsCM(key = my_key, server = "1000FARMS")
#' 
#' @export
getProjectsCM = function(key, server = "climmob3", ...){
  
  dots = list(...)
  
  url = .set_url(server, extension = "readProjects?Apikey=")
  
  dat = httr::RETRY(verb = "GET",
                    url = url,
                    query = list(Apikey = key),
                    httr::accept_json(),
                    terminate_on = c(403, 404))
  
  dat = httr::content(dat, as = "text")
  
  dat = jsonlite::fromJSON(dat)
  
  if (length(dat) == 0) {
    return(cat("No project associated with this API key \n"))
  }
  
  owner = dat$owner
  
  names(owner) = paste0("owner_", names(owner))
  
  dat = cbind(dat, owner)
  
  dat = dat[,c("project_id", "project_cod", "project_name", "project_pi", 
               "owner_user_name", "project_piemail", "project_tags",
               "project_numobs", "project_cnty", "project_creationdate")]
  
  names(dat) = c("project_id", "project_code", "project_name", "coordinator",
                 "user_owner", "email", "keywords", 
                 "npackages", "country", "creation_date")
  
  dat$creation_date = with(dat, as.Date(creation_date, origin = "1970-01-01"))
  
  dat = as.data.frame(dat, stringsAsFactors = FALSE)
  
  dat$server = server
  
  class(dat) = union("CM_df", class(dat))
  
  return(dat)
  
}
