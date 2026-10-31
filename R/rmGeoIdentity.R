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
#' @param seed integer, the seed for random number generation. Leave it
#'  \code{NULL} (the default) for data that will be shared. The random
#'  displacement does not depend on the original coordinates, so anyone who
#'  knows the seed can reproduce it and recover the exact locations. Use a
#'  seed only for tests or internal reproducibility, and never publish it
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
#' rmGeoIdentity(xy)
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
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("rmGeoIdentity() needs the package 'sf'; please install it")
  }
  
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
    do.call(sf::st_point, a)
  })
  
  # and then into a geometry list column
  longlat = do.call(sf::st_sfc, longlat)
  
  args = list(x = longlat,
              dist = dist, 
              nQuadSegs = nQuadSegs)
  
  lonlatb = do.call(sf::st_buffer, args)
  
  result = split(lonlatb, seq_len(n))
  
  # A seed makes the displacement reproducible, and anyone who knows it
  # can recover the original points. Without a seed the current random
  # stream is used and nothing is printed. With a seed, the caller's
  # random number state is restored on exit
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_seed = get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    } else {
      on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }, add = TRUE)
    }
    set.seed(seed)
  }
  
  result[!anyNAs] = lapply(result[!anyNAs], function(x){
    a = list(x = x, size = 1, type = "random", by_polygon = TRUE)
    do.call(sf::st_sample, a)
  })
  
  result = do.call(rbind, result)
  
  result = do.call(sf::st_sfc, result)
  
  r = matrix(NA, nrow = n, ncol = 2)
  
  r[!anyNAs, ] = matrix(unlist(result),
                        ncol = 2,
                        nrow = sum(!anyNAs),
                        byrow = TRUE)
  
  r = as.data.frame(r)
  
  names(r) = c("long", "lat")
  
  return(r)
  
}
