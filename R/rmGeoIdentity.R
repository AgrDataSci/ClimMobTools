#' Remove geographical identity
#' 
#' Build a buffer around the a set of geographical coordinates 
#'  and take a random point around the buffer. The function is 
#'  used to omit the precise location of tricot participants 
#'  but keeping a close distance to the trial environment.
#'   
#' @param longlat a data.frame or matrix with geographical coordinates long lat
#' @param dist numeric, buffer distance for all \var{lonlat}
#' @param nQuadSegs integer, number of segments per quadrant
#' @param seed integer, the seed for random number generation.
#'  If NULL (the default), ClimMobTools will set the seed randomly
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods
#' @return A data frame with the random coordinates long lat within the buffer
#' @examplesIf interactive()
#' library("sf")
#' 
#' xy = matrix(c(11.097799, 60.801090,
#'                11.161298, 60.804199,
#'                11.254428, 60.822457),
#'              nrow = 3, ncol = 2, byrow = TRUE)
#' 
#' rmGeoIdentity(xy, seed = 1501)
#' 
#' #' the function also handles NAs
#' 
#' xy2 = matrix(c(11.097799, 60.801090,
#'                 NA, NA,
#'                 11.161298, 60.804199,
#'                 11.254428, 60.822457,
#'                 11.254428, NA),
#'               nrow = 5, ncol = 2, byrow = TRUE)
#' 
#' rmGeoIdentity(xy2)
#' 
#' @importFrom stats runif
#' @export
rmGeoIdentity = function(longlat, dist = 0.015, nQuadSegs = 2L, seed = NULL, ...){
  
  longlat = as.matrix(longlat)
  
  n = nrow(longlat)
  
  # check NAs in lonlat
  anyNAs = is.na(longlat[,1]) | is.na(longlat[,2])
  
  # put all both xy as NA
  longlat[anyNAs, ] = NA
  
  # split lonlat by rows
  longlat = split(longlat, seq_len(n))
  
  # transform into sf points
  longlat = lapply(longlat, function(l) {
    a = list(x = l)
    do.call("st_point", a)
  })
  
  # and then into a geometry list column
  longlat = do.call("st_sfc", longlat)
  
  args = list(x = longlat,
              dist = dist, 
              nQuadSegs = nQuadSegs)
  
  lonlatb = do.call("st_buffer", args)
  
  result = split(lonlatb, seq_len(n))
  
  # check if a seed is provided
  if (is.null(seed)) {
    seed = as.integer(stats::runif(1, 0, 1000000))
    message("seed ", seed, " was used in the sampling \n")
  }
  
  set.seed(seed)
  
  result[!anyNAs] = lapply(result[!anyNAs], function(x){
    a = list(x = x, size = 1, type = "random", by_polygon = TRUE)
    do.call("st_sample", a)
  })
  
  result = do.call(rbind, result)
  
  result = do.call("st_sfc", result)
  
  r = matrix(NA, nrow = n, ncol = 2)
  
  r[!anyNAs, ] = matrix(unlist(result),
                        ncol = 2,
                        nrow = sum(!anyNAs),
                        byrow = TRUE)
  
  r = as.data.frame(r)
  
  names(r) = c("long", "lat")
  
  return(r)
  
}
