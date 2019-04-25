#' Evapotranspiration
#' 
#' Compute evapotranspiration using the Blaney-Criddle method
#' 
#' @inheritParams temperature
#' @param lat the latitude (in Decimal degrees)
#' @param Kc the crop factor for water requirement
#' @param p optional, a numeric value (from 0 to 1) used if lat is not given,
#' representing the mean daily percentage of annual daytime hours 
#' for different latitudes
#' @return The evapotranspiration in mm/day
#' @examples
#' 
#' # use the breadwheat data from package gosset 
#' library("gosset")
#' library("nasapower")
#' library("raster")
#' 
#' data("breadwheat", package = "gosset")
#' 
#' # the evapotranspiration in the first 100 days after planting
#' ETo(breadwheat[c("lon","lat")], 
#'     day.one = breadwheat[["planting_date"]],
#'     span = 100,
#'     lat = breadwheat[["lat"]])
#' 
#' @export
ETo <- function(object, day.one = NULL, span = NULL, 
                lat = NULL, Kc = 1, p = NULL){
  
  # get p if lat is provided
  if (!is.null(lat)) {
    l <- .round5(lat, 5)
    m <- as.integer(format(day.one, "%m"))
    p <- daylight[cbind(match(l , daylight[, 1]), match(m , names(daylight)))]
  } 
  
  if (is.null(p)) {
    p <- 0.27
  }
  
  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    cat("fetching NASA POWER, this may take a little longer. \n")
    day <- .get_timespan(object, day.one, span, pars = "T2M_MAX")
  } else {
    day <- .get_timespan(object[, , 1], day.one, span)
  }
  
  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timespan(object, day.one, span, pars = "T2M_MIN")
  } else {
    night <- .get_timespan(object[, , 2], day.one, span)
  }
  
  # calculate Tmean
  Tmean <- (rowMeans(day, na.rm = TRUE) +  rowMeans(night, na.rm = TRUE)) / 2
  
  # evapotranspiration
  eto <- p * (0.46 * Tmean + 8) * Kc
  
  return(tibble::tibble(ETo = eto))
  
}

# Round to the nearest base value
.round5 <- function(x, base.value) {
  
  base.value * round( x / base.value )
  
}