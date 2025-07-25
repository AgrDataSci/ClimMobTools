#' Export block-level (participant) data
#'
#' Creates a wide-format data.frame of participant (block) data for external export. 
#' The output excludes ranking variables and personally identifiable information (PII), 
#' such as names, phone numbers, and location metadata. Optionally includes geolocation 
#' if available.
#' 
#' @family export functions
#' @inheritParams exportTricotRanks
#' @return A data.frame with one row per block (participant),
#'  excluding PII and ranking data.
#' @export
exportBlockData = function(x){
  
  # get coordinates if any 
  coords = .get_trial_coordinates(x, return = c("coordinates"))
  
  xdf = as.data.frame(x)
  
  xdf$package_id = paste(xdf$project_id, xdf$package_id, sep = "-")
  
  names(xdf) = gsub("_gender1", "_gender", names(xdf))
  
  names(xdf) = gsub("package_id", "block_id", names(xdf))
  
  # remove rankings
  patterns_to_remove = c("_pos$", "_neg$", "project", "latitude", "longitude", 
                         "pointofdelivery", "geotrial", "participant_name", 
                         "submitted_by", "_village", "_district", "telephone",
                         "_a$", "_b$", "_c$")
  
  
  patterns_to_remove = paste(patterns_to_remove, collapse = "|")
  
  cols_to_remove = grepl(patterns_to_remove, names(xdf), ignore.case = TRUE)
  
  xdf = xdf[, !cols_to_remove]
  
  if (!is.null(coords)) {
    xdf$longitude = coords$longitude
    xdf$latitude = coords$latitude
  }
  
  class(xdf) =  union("CM_df", class(xdf))
  
  return(xdf)
  
}
