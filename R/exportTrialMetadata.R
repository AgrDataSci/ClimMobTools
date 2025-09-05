#' Extract dates from data submission
#' @param x an object of class CM_list
#' @return a vector with first and last dates of 
#' data submission registered by ODK
#' @noRd
.get_dates_spam = function(x){
  index = grep("submitted_date", names(x$data))
  dates = as.Date(unlist(x$data[index]))
  dates = as.character(c(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE)))
  list(start = dates[1],
       end = dates[2])
}

#' Extract GPS coordinates from data
#' @param x an object of class CM_list
#' @param return character to select the return output, options are 
#' "bbox" or "coordinates"
#' @noRd
.get_trial_coordinates = function(x, return = "bbox", ...) {
  
  coords = .handle_geolocation_columns(x$data)
  
  index = grep("longitude|latitude", names(coords))
  
  if (length(index) < 2) {
    warning("Not enough geolocation columns found.")
    return(NULL)
  }
  
  coords = coords[, index, drop = FALSE]
  
  # If there is only one longitude and one latitude column
  if (sum(grepl("_longitude", names(coords))) == 1 && 
      sum(grepl("_latitude", names(coords))) == 1) {
    
    lonlat = data.frame(
      longitude = coords[[grep("_longitude", names(coords))]],
      latitude = coords[[grep("_latitude", names(coords))]])
    
  }else{
  
    lon = grep("_longitude", names(coords))
    lon = coords[, lon]
    
    lon = as.vector(apply(lon, 1, function(x){
      # I'll take the reverse as this increases the likelihood of
      # getting the coordinates from the trial, not the point of
      # delivery
      names(x)[rev(which(!is.na(x)))[1]]
    }))
    
    lon[is.na(lon)] = grep("_longitude", names(coords))[1]
    
    lat = gsub("_longitude", "_latitude", lon)
    
    rownames(coords) = 1:nrow(coords)
    
    # keep only the selected columns, one per plot
    lonlat = data.frame(longitude = coords[cbind(1:nrow(coords), lon)],
                        latitude = coords[cbind(1:nrow(coords), lat)])
    
      
  }
  
  lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
  
  
  # longlat with 0, 0 is wrong
  lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
  lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
  
  if(return == "coordinates") {
    xy = rmGeoIdentity(lonlat, ...)
    names(xy) = c("longitude", "latitude")
    return(xy)
  }
  
  bbox = list(xmin = round(min(lonlat$longitude, na.rm = TRUE), 2),
              xmax = round(max(lonlat$longitude, na.rm = TRUE), 2),
              ymin = round(min(lonlat$latitude,  na.rm = TRUE), 2),
              ymax = round(max(lonlat$latitude,  na.rm = TRUE), 2))
  
  return(bbox)
  
}


#' Generate metadata for tricot trial export
#'
#' Extracts and compiles key metadata from a ClimMob trial object to support
#' standardized data documentation and publication, including fields such as 
#' trial identifiers, geographic bounding box, crop and taxon information, 
#' and participant statistics.
#' 
#' @family export functions
#' @param x An object of class \code{CM_list} containing raw ClimMob trial data.
#' @return A named list containing metadata fields required for tricot data export,
#' including trial name, description, dates, bounding box, participant counts, 
#' and institutional information.
#' @importFrom utils packageVersion
#' @export
exportTrialMetadata = function(x){
  
  # gender index in data
  gender = grep("_gender", names(x$data))[1]
  
  na_default = "No information provided"

  list(changelog = list(version = paste0("v", Sys.Date()), 
                        notes = "Initial release",
                        software = list(package = "ClimMobTools", 
                                        package_version = as.character(utils::packageVersion("ClimMobTools")))),
       doi = na_default,
       license = na_default,
       trial_id = .safe_extract(x, c("project", "project_id"), default = na_default),
       trial_country = .safe_extract(x, c("project", "project_cnty"), default = na_default),
       trial_name = .safe_extract(x, c("project", "project_name"), default = na_default),
       trial_type = .safe_extract(x, c("project", "trial_type"), default = na_default),
       trial_experimental_site = .safe_extract(x, c("project", "experimental_site"), default = na_default),
       trial_unit_of_analysis = .safe_extract(x, c("project", "unit_of_analysis"), default = na_default),
       trial_objective = .safe_extract(x, c("project", "trial_objective"), default = na_default),
       trial_description = .safe_extract(x, c("project", "project_abstract"), default = na_default),
       date = try(.get_dates_spam(x), silent = TRUE),
       bounding_box = .get_trial_coordinates(x, return = "bbox"),
       data_producer_name = .safe_extract(x, c("project", "project_pi"), default = na_default),
       data_producer_email = .safe_extract(x, c("project", "project_piemail"), default = na_default),
       data_producer_institute = .safe_extract(x, c("project", "project_affiliation"), default = na_default),
       program = .safe_extract(x, c("project", "project_program"), default = na_default),
       crop_name = .safe_extract(x, c("combination", "elements", 1, "technology_name", 1)),
       taxon = .safe_extract(x, c("project", "taxon"), default = na_default),
       nparticipants = try(length(x$data[,gender]), silent = TRUE),
       n_men = try(sum(x$data[, gender] %in% c("2", "Man"), na.rm = TRUE), silent = TRUE),
       n_women = try(sum(x$data[, gender] %in% c("1", "Woman"), na.rm = TRUE), silent = TRUE))
  
}

