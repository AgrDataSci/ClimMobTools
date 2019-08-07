
# Validate a tibble object
.is_tibble <- function(object) {
  
  c("tbl_df") %in% class(object)

}


# Round to the nearest base value
.round5 <- function(x, base.value) {
  
  base.value * round( x / base.value )
  
}