#' Growing Degree Days
#' 
#' Compute number of days required to reach growing degree days.
#' GDD are calculated by taking the integral of warmth above a base temperature.
#' 
#' @param degree.days an integer for the degree days required by the crop (look at the physiology of the focal crop)
#' @param base an integer for the base temperature. Set 10 as default.
#' @inheritParams temperature
#' @return The number of days required to reach the growing degree days.
#' @examples
#' \dontrun{
#' # use the breadwheat data from package gosset 
#' library("gosset")
#' library("nasapower")
#' library("raster")
#' 
#' data("breadwheat", package = "gosset")
#' 
#' # Calculate the days required for the plants in these plots to reach the
#' # maturity. The crop requires ~1800 degree days for it.
#' 
#' GDD(breadwheat[c("lon","lat")], 
#'     day.one = breadwheat[["planting_date"]],
#'     degree.days = 1800,
#'     base = 5)
#'}
#'     
#' @export
GDD <- function(object, day.one = NULL, degree.days = NULL,
                base = NULL, span = NULL, ...)
{
  
  # validate parameters
  if (is.null(day.one)) {
    stop("day.one is missing with no default \n")
  }
  if (is.null(degree.days)) {
    stop("degree.days is missing with no default \n")
  }
  if (is.null(base)) {
    base <- 10
  }
  if (is.null(span)) {
    span <- 150
  }
  
  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    cat("fetching NASA POWER, this may take a little longer. \n")
    day <- .get_timespan(object, day.one, span, pars = "T2M_MAX", ...)
  } else {
    day <- .get_timespan(object[, , 1], day.one, span, ...)
  }
  
  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timespan(object, day.one, span, pars = "T2M_MIN", ...)
  } else {
    night <- .get_timespan(object[,,2], day.one, span, ...)
  }
  
  # get the difference between day and night temperature
  Y <- (((day + night) / 2) - base)
  
  # sum temperature values until reach the defined degree days
  Y <- apply(Y, 1, function(x){
    
    for (d in 1:length(x)) {
      
      i <- d
      
      if (sum(x[1:d]) > degree.days) break}
    
    return(i)
  })
  
  return(tibble::tibble(GDD = Y))
}

