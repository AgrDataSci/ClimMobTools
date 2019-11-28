
# Validate a tibble object
.is_tibble <- function(object) {
  
  c("tbl_df") %in% class(object)

}


