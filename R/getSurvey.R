#' Retrieve survey questions from a ClimMob project
#'
#' Retrieves the list of survey questions associated with a ClimMob project
#' using the ClimMob API. The function sends a request to the
#' \code{getListOfQuestionsByProject} endpoint and returns the survey
#' questions as a data frame.
#'
#' This function is useful for extracting questionnaire structures used in
#' ClimMob projects, which can then be used for documentation, translation,
#' validation, or integration into analysis pipelines.
#'
#' If the API request fails (e.g., invalid project, unsupported language,
#' or server error), the function prints a message and returns \code{NULL}
#' instead of terminating the R session.
#' @family GET functions
#' @inheritParams getDataCM
#' @param language Character. Language code for the questionnaire
#' @return A data frame containing the survey questions associated with the
#' specified project and language. Returns \code{NULL} if the request fails.
#'
#' @details
#' The function queries the ClimMob API endpoint:
#' \url{https://1000farms.climmob.net/api/getListOfQuestionsByProject}.
#'
#' The API response is returned as JSON and parsed into an R data frame.
#' If the server returns an error or invalid JSON, the function reports
#' the issue and safely returns \code{NULL}.
#'
#' @examplesIf interactive()
#' # This function only works with an API key
#' # the API key can be obtained from your ClimMob account
#'  
#' library("ClimMobTools")
#' my_key = "d39a3c66-5822-4930-a9d4-50e7da041e77"
#' 
#' @importFrom httr GET status_code http_status content
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
getSurvey = function(key, userowner, project, language) {
  url = "https://1000farms.climmob.net/api/getListOfQuestionsByProject"
  
  body_list = list(
    user_owner = userowner,
    project_cod = project,
    lang_code = language
  )
  
  payload = list(
    Apikey = key,
    Body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
  )
  
  tryCatch({
    response = httr::GET(url, query = payload)
    
    # Check HTTP status
    if (httr::status_code(response) >= 400) {
      status = httr::http_status(response)
      content_text = httr::content(response, as = "text", encoding = "UTF-8")
      
      message("HTTP error ", httr::status_code(response), ": ", status$reason)
      
      if (nchar(content_text) > 0) {
        parsed_error = tryCatch(
          jsonlite::fromJSON(content_text),
          error = function(e) NULL
        )
        
        if (!is.null(parsed_error)) {
          print(parsed_error)
        } else {
          message("Server response: ", content_text)
        }
      }
      
      return(NULL)
    }
    
    # Read response text
    json_text = httr::content(response, as = "text", encoding = "UTF-8")
    
    if (nchar(json_text) == 0) {
      message("Error: empty response from server.")
      return(NULL)
    }
    
    # Decode JSON safely
    data = tryCatch(
      jsonlite::fromJSON(json_text, flatten = TRUE),
      error = function(e) {
        message("Error decoding JSON: ", conditionMessage(e))
        message("Server response: ", json_text)
        return(NULL)
      }
    )
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Optional: if API returns an error object instead of a table
    if (!is.data.frame(data) && !is.list(data)) {
      message("Error: unexpected JSON format.")
      return(NULL)
    }
    
    output = as.data.frame(data)
    return(output)
    
  }, error = function(e) {
    msg = conditionMessage(e)
    
    if (grepl("Timeout", msg, ignore.case = TRUE)) {
      message("Error: request timed out.")
    } else if (grepl("refused", msg, ignore.case = TRUE)) {
      message("Error: connection refused.")
    } else {
      message("General error: ", msg)
    }
    
    return(NULL)
  })
}
