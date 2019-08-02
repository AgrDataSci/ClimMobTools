#' Required seed amount in a ClimMob project
#' 
#' Calculate the required amount of seeds (or other technology) required 
#' for a ClimMob project.
#' 
#' @param unit optional, a character specifying the metric unit used
#' @param nseeds an integer for the metric of seeds each bag receives
#' @inheritParams randomise
#' @return a dataframe with required number of seeds
#' @examples
#'   
#' # allocate 0.2 kg of seeds per variety in a project with 500 
#' # participants and 14 varieties
#' seed_need(nobservers = 500,
#'           ncomp = 3,
#'           nitems = 14, 
#'           nseeds = 0.2)
#' 
#' # allocate 100 seedlings per variety in a project with 400 
#' # participants, 8 varieties and 3 comparisons between varieties
#' seed_need(nobservers = 400,
#'           ncomp = 3,
#'           nitems = 9, 
#'           nseeds = 100,
#'           unit = "unit")
#' @export
seed_need <- function(nobservers = 100, ncomp = 3, 
                      nitems = 10, nseeds = 0.15, unit = "kg") {
  
  
  nbags <- nobservers * ncomp
  
  bagsvar <- ceiling(nbags / nitems)
  
  seedsvar <- ceiling(bagsvar * nseeds)
  seedstotal <- ceiling(seedsvar * nitems)
  
  namevar <- c("N bags", "Bags per variety", 
               "Seeds per variety", "Seeds total")
  
  result <- tibble::tibble(var = namevar,
                           quant= c(nbags, bagsvar, seedsvar, seedstotal),
                           unit = c(rep("unit",2), rep(unit, 2)))
  
  
  return(result)
  
  
}


