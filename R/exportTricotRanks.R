#' Extract and format tricot rankings for data export
#'
#' Processes ClimMob ranking data into a standardized long-format data frame
#' suitable for export to external databases and analysis. For each trait,
#' the function extracts rank positions per genotype and block
#' based on participant responses. Compatible with Dataverse and other 
#' FAIR data publication systems.
#' 
#' @family export functions
#' @param x An object of class \code{CM_list} containing raw ClimMob trial data.
#' @param ... Additional arguments passed to \code{getTraitList()}.
#' @inheritParams getTraitList
#' @inheritParams rankTricot
#' @return A data frame in long format containing tricot rankings, 
#' with one row per genotype and trait, including the block identifier, 
#' plot label (A/B/C), genotype name, trait label, and assigned rank position. 
#' @export
exportTricotRanks = function(x, 
                             pattern = c("_pos", "_neg"),
                             items = c("package_item_A", "package_item_B", "package_item_C"), 
                             ...){
  
  if (length(items) != 3) stop("Expecting three item columns, e.g., c('item_A', 'item_B', 'item_C')")
  
  xdf = as.data.frame(x, ...)
  
  trial_id = xdf$project_id[1]
  
  traits = getTraitList(xdf, pattern = pattern, ...)
  
  traitlabels = unlist(lapply(traits, function(x) x$trait_label))
  
  # separate trait labels from traits and collection moments
  traitlabels = strsplit(traitlabels, "_")
  
  traitlabels = lapply(traitlabels, function(x){
    c(x[1], paste(x[2:length(x)], collapse = "_"))
  })
  
  
  # now we build the PlackettLuce rankings
  R = lapply(traits, function(XX){
    rankTricot(data = xdf,
               items = items,
               input = XX$string,
               validate.rankings = FALSE)
  })
  
  rank_data = list()
  
  counter = 1
  
  for (i in seq_along(traitlabels)) {
    
    r = unclass(R[[i]])
    
    for (j in seq_along(xdf$package_id)) {
      
      # block id is package id + trial id
      id = xdf$package_id[j]
      
      plots = as.vector(unlist(xdf[xdf$package_id == id, items]))
      
      x = r[j, plots]
      
      # block id is package id + trial id
      id = paste(trial_id, id, sep = "-")
      
      d = data.frame(block_id = id, 
                     plot = LETTERS[1:3],
                     genotype_name = plots,
                     collection_moment = as.vector(traitlabels[[i]][1]),
                     trait = as.vector(traitlabels[[i]][2]),
                     value = x,
                     value_type = "rank")
      
      rank_data[[counter]] = d
      counter = counter + 1
      
    }
    
  }
  
  rank_data = do.call("rbind", rank_data)
  
  rownames(rank_data) = 1:nrow(rank_data)
  
  class(rank_data) =  union("CM_df", class(rank_data))
  
  return(rank_data)
  
}
