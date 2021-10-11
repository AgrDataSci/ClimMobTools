#' Remove geographical identity
#' 
#' Build a buffer around the a set of geographical coordinates 
#'  and take a random point around the buffer. The function is 
#'  used to omit the precise location of tricot participants 
#'  but keeping a close distance to its agro-environment 
#' @param lonlat a data.frame or matrix with geographical coordinates long lat
#' @param dist numeric, buffer distance for all \var{lonlat}
#' @param nQuadSegs integer, number of segments per quadrant
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods
#' @examples 
#' xy <- matrix(c(11.097799, 60.801090,
#'                11.161298, 60.804199,
#'                11.254428, 60.822457),
#'              nrow = 3, ncol = 2, byrow = TRUE)
#' rmGeoIndentity(xy)
#'  
#' @importFrom sf st_point st_sfc st_buffer st_sample
#' @export
rmGeoIndentity <- function(lonlat, dist = 0.015, nQuadSegs = 2L, ...){
  
  lonlat <- as.matrix(lonlat)
  n <- nrow(lonlat)
  
  # split lonlat by rows
  lonlat <- split(lonlat, seq_len(n))
  
  # transform into sf points
  lonlat <- lapply(lonlat, function(l) {
    sf::st_point(l)
  })
  
  # and then into a geometry list column
  lonlat <- sf::st_sfc(lonlat)
  
  # set the buffer around the points
  lonlatb <- sf::st_buffer(lonlat,
                           dist = dist,
                           nQuadSegs = nQuadSegs, 
                           ...)
  
  result <- split(lonlatb, seq_len(n))
  
  result <- lapply(result, function(x){
    sf::st_sample(x, size = 1, type = "random", by_polygon = TRUE)
  })
  
  result <- do.call(rbind, result)
  
  result <- sf::st_sfc(result)
  
  result <- matrix(unlist(result), ncol = 2, nrow = n, byrow = TRUE)
  
  result <- as.data.frame(result)
  
  names(result) <- c("x", "y")
  
  return(result)
  
}
