#' Organize trait ranks in a ClimMob data
#' 
#' This function helps in identifying the traits 
#'  assessed in the tricot project and validates 
#'  the data returning a list with logical vectors 
#'  to support the transformation of tricot rankings 
#'  into a PlackettLuce ranking
#' 
#' @param data data.frame, the ClimMob data
#' @param pattern character, the tricot ranking pattern
#' @param trait.labels an optional character with clean trait labels
#' @param ... additional arguments, not implemented yet
#' @return a list with trait validation data
#' @examplesIf interactive() 
#' 
#' require("gosset")
#' 
#' data("breadwheat", package = "gosset")
#' 
#' getTraitList(breadwheat, c("_best", "_worst"))
#' 
#' @export
getTraitList = function(data, pattern, trait.labels = NULL, ...){
  
  traits = grep(paste0(pattern[1], "$"), names(data))
  
  traits = names(data)[traits]
  
  traits = gsub(pattern[1], "", traits)
  
  if (is.null(trait.labels)) {
    trait.labels = traits
  }
  
  trait_list = list()
  
  # run this loop over the data to filter it
  for (i in seq_along(traits)) {
    
    trait_i = paste0(traits[i], pattern)
    
    # the values should always one of the letters A, B, or C
    ABC = apply(data[trait_i], 1, function(x) {
      all(x %in% LETTERS[1:3])
    })
    
    # also no NAs
    noNA = apply(data[trait_i], 1, function(x) {
      all(!is.na(x))
    })
    
    # also no duplicated entries
    noDups = apply(data[trait_i], 1, function(x) {
      all(!duplicated(x))
    })
    
    keep = ABC & noNA & noDups
    
    tr = list(keep = keep, 
              string = trait_i,
              trait = traits[i],
              trait_label = trait.labels[i])
    
    trait_list[[i]] = tr
    
  }
  
  return(trait_list)
  
}
