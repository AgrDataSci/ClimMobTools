#' Extract and format measured traits
#' 
#' Extracts non-ranking, plot-level trait data from a ClimMob trial object. The function 
#' detects variables ending in "_a", "_b", or "_c", infers the trait and collection moment,
#' and reshapes the data into long format for analysis.
#' 
#' @family export functions
#' @param x An object of class \code{CM_list} containing raw ClimMob trial data.
#' @return A data frame with one row per genotype observation, including block ID, plot, 
#' genotype name, collection moment, trait name, value, and value type.
#' @export
exportMeasuredTraits = function(x){
  
  xdf = as.data.frame(x)
  
  vars = names(xdf)
  
  plot_vars = vars[grepl("_(a|b|c)$", vars)]
  
  traits = unique(sub("_(a|b|c)$", "", plot_vars))
  
  desc = .extract_question_description(x)
  
  xdf$block_id = paste(xdf$project_id, xdf$package_id, sep = "-")
  
  long_data = lapply(traits, function(trait){
    
    trait_cols = paste0(trait, "_", c("a", "b", "c"))
    
    if (!all(trait_cols %in% names(xdf))) return(NULL)
    
    collection_moment = gsub("_.*", "", trait)
    
    trait_name = gsub(".*_", "", trait)
    
    index = grep(paste0("^", trait_name, "$"), desc$name)
    
    value_type = desc$odktype[grep(trait_name, desc$name)[1]]
    
    d = data.frame(block_id = xdf$block_id,
                   plot = rep(LETTERS[1:3], each = nrow(xdf)),
                   genotype_name = unlist(xdf[paste0("package_item_", LETTERS[1:3])]),
                   collection_moment = collection_moment, 
                   trait = trait_name,
                   value = unlist(xdf[trait_cols]),
                   value_type = value_type)
    
    return(d)
  })
  
  plot_data = do.call("rbind", long_data)
  
  rownames(plot_data) = 1:nrow(plot_data)
  
  class(plot_data) =  union("CM_df", class(plot_data))
  
  return(plot_data)
  
}
