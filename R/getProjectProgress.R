#' Get project progress
#'
#' Fetch the progress of a ClimMob project
#'
#' @author Kauê de Sousa
#' @family GET functions
#' @inheritParams getDataCM
#' @return A list with number of submissions per assessment and 
#'  submissions per assessment per enumerator
#' @details 
#' \code{server}: the default server is "climmob" used for clients of 
#' \url{https://climmob.net/climmob3/}, other options are:
#' 
#'  "1000farms" for clients of \url{https://1000farms.climmob.net/} 
#'  
#' @examplesIf interactive()
#' # This function only works with an API key
#' # the API key can be obtained from your ClimMob account
#'  
#' library("ClimMobTools")
#' my_key = "ff05a174-28d0-4a40-ab5a-35dc486133a6"
#' 
#' getProjectProgress(key = my_key,
#'                    project = "gina2024",
#'                    userowner = "student",
#'                    server = "1000FARMS")
#' @export
getProjectProgress = function(key, project, userowner, server = "climmob3"){
  
  url = .set_url(server, extension = "readDataOfProject?Body={}&Apikey={}")
  
  dat = httr::RETRY(verb = "GET", 
                    url = url,
                    query = list(Body = paste0('{"project_cod":"', project, '",
                                                   "user_owner":"',userowner,'"}'),
                                 Apikey = key),
                    httr::accept_json(), 
                    terminate_on = c(403, 404))
  
  dat = httr::content(dat, as = "text")
  
  dat = jsonlite::fromJSON(dat)
  
  # check if the given project has data
  # if not then return a warning message
  if (length(dat) < 7) {
    pstring = paste0("'",project,"'")
    message("Project ", pstring, " was found but has no associated data. \n")
    return(project)
  }
  
  result = .project_progress(dat)
  
  return(result)
  
}

#' Get the progress data 
#' @param x a list with the climmob data
#' @noRd
.project_progress = function(x) {
  
  assess_code = x$assessments[["code"]]
  assess_name = x$assessments[["desc"]]
  assess_day  = x$assessments[["intervalindays"]]
  
  # run over assessments and collect number of submissions
  nsubs = data.frame(assessment = "Registration",
                     interval_in_days = 1,
                     n_entries = length(x$data$REG__submitted_by))
  
  enumerators = data.frame(assessment = "Registration",
                           table(x$data$REG__submitted_by))
  
  names(enumerators)[2:3] = c("enumerator", "n_entries")
  
  for (i in seq_along(assess_code)) {
    
    sub_i = x$data[,paste0("ASS", assess_code[i], "__submitted_by")]
    
    y = data.frame(assessment = assess_name[i],
                   interval_in_days = 0,
                   n_entries = sum(!is.na(sub_i)))
    
    nsubs = rbind(nsubs, y)
    
    if(sum(!is.na(sub_i)) > 0) {
      
      enum_i = data.frame(assessment = assess_name[i], 
                          table(sub_i))
      
      names(enum_i)[2:3] = c("enumerator", "n_entries")
      
      enumerators = rbind(enumerators, enum_i)
      
    }
    
  }
  
  enumerators$enumerator = as.character(enumerators$enumerator)
  
  nsubs$interval_in_days = as.integer(nsubs$interval_in_days)
  
  nsubs = nsubs[order(nsubs$interval_in_days), ]
  
  rownames(nsubs) = 1:nrow(nsubs)
  
  class(nsubs) = union("CM_df", class(nsubs))
  class(enumerators) = union("CM_df", class(enumerators))
  
  r = list(submissions = nsubs,
           enumerators = enumerators)
  
  class(r) = union("CM_list", class(r))
  
  return(r)
  
}
