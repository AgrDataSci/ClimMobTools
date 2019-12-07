# Set data.frame into long format
# 
# @param object a data.frame in the wide format
# @param id a character for the id
# @return a data.frame in the long format
# @examples
# data <- data.frame(
#   id = 1:10,
#   x = LETTERS[1:10],
#   y = letters[11:20],
#   z = colors()[1:10],
#   w = colors()[401:410]
# )
# 
# .set_long(data, "id")
.set_long <- function(object, id) {
  
  object <- split(object, object[, id])
  
  object <- lapply(object, function(x){
    
    x <- t(x)
    
    x <- cbind(row.names(x), x)
    
    i <- x[id, 2]
    
    x <- cbind(x, id = i)
    
    x <- x[ x[,1] != id , ]
    
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    
    names(x) <- c("variable", "value", "id")
    
    x <- x[ ,c("id", "variable", "value")]
    
    return(x)
    
  })
  
  object <- do.call("rbind", object)
  
  object <- as.data.frame(object, stringsAsFactors = FALSE)
  
  object <- with(object, object[order(as.integer(id)), ])
  
  rownames(object) <- 1:nrow(object)
  
  return(object)
  
}


# Set a data.frame into wide format
# 
# @param object a data.frame in the long format
# @param id a character for the id
# @return a data.frame in the wide format
# @examples
# data <- data.frame(
#   id = 1:10,
#   x = LETTERS[1:10],
#   y = letters[11:20],
#   z = colors()[1:10],
#   w = colors()[401:410]
# )
# 
# data_l <- .set_long(data, "id")
# 
# .set_wide(data_l, "id")
.set_wide <- function(object, id) {
  
  object <- split(object, object[, id])
  
  object <- lapply(object, function(x){
    
    x <- t(x)
    
    i <- as.integer(x[1, 1])
    
    nams <- c("id", x[2, ])
    
    val <- as.vector(c(i, x[3, ]))
    
    names(val) <- nams
    
    return(val)
    
  })
  
  object <- do.call("rbind", object)
  
  object <- as.data.frame(object, stringsAsFactors = FALSE)
  
  object <- with(object, object[order(as.integer(id)), ])
  
  return(object)
}
