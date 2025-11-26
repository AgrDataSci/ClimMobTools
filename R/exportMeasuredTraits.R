#' Extract and format measured traits
#' 
#' Extracts non-ranking, plot-level trait data from a ClimMob trial object. The function 
#' detects variables ending in "_a", "_b", or "_c", infers the trait and collection moment,
#' and reshapes the data into long format for analysis.
#' 
#' If no measured traits are found in the data (i.e., no variables ending in "_a", "_b",
#' or "_c"), the function returns an empty \code{CM_df} object with valid column names.
#' 
#' @family export functions
#' @param x An object of class \code{CM_list} containing raw ClimMob trial data.
#' @return A data frame of class \code{CM_df} with one row per genotype observation,
#'   including block ID, plot, genotype name, collection moment, trait name, value,
#'   and value type. Returns an empty \code{CM_df} if no measured traits are present.
#' @export
exportMeasuredTraits = function(x){
  
  xdf = as.data.frame(x)
  vars = names(xdf)
  
  # Identify measured trait variables
  plot_vars = vars[grepl("_(a|b|c)$", vars)]
  
  # Base empty structure (returned when no measured traits exist)
  empty = data.frame(
    block_id = character(0),
    plot = character(0),
    genotype_name = character(0),
    collection_moment = character(0),
    trait = character(0),
    value = character(0),
    value_type = character(0),
    stringsAsFactors = FALSE
  )

  # Early return if none found
  if (length(plot_vars) == 0) {
    return(empty)
  }
  
  traits = unique(sub("_(a|b|c)$", "", plot_vars))
  desc = .extract_question_description(x)
  
  xdf$block_id = paste(xdf$project_id, xdf$package_id, sep = "-")
  
  long_data = lapply(traits, function(trait){
    
    trait_cols = paste0(trait, "_", c("a", "b", "c"))
    
    # If incomplete trait columns (e.g. missing _b or _c) → skip this trait
    if (!all(trait_cols %in% names(xdf))) return(NULL)
    
    collection_moment = sub("_.*", "", trait)
    trait_name = sub(".*_", "", trait)
    
    index = which(desc$name == trait_name)
    value_type = if (length(index) > 0) desc$odktype[index[1]] else NA_character_
    
    d = data.frame(
      block_id = xdf$block_id,
      plot = rep(LETTERS[1:3], each = nrow(xdf)),
      genotype_name = unlist(xdf[paste0("package_item_", LETTERS[1:3])]),
      collection_moment = collection_moment, 
      trait = trait_name,
      value = unlist(xdf[trait_cols]),
      value_type = value_type,
      stringsAsFactors = FALSE
    )
    
    return(d)
  })
  
  # Remove skipped traits
  long_data = Filter(Negate(is.null), long_data)
  
  # If all traits skipped → return empty structure
  if (length(long_data) == 0) {
    return(empty)
  }
  
  plot_data = do.call("rbind", long_data)
  rownames(plot_data) = seq_len(nrow(plot_data))
  
  class(plot_data) = union("CM_df", class(plot_data))
  return(plot_data)
}
