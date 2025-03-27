#' Set data.frame into long format
#' 
#' @param object a data.frame in the wide format
#' @param id a character for the id
#' @return a data.frame in the long format
#' @examples
#' data = data.frame(
#'   id = 1:10,
#'   x = LETTERS[1:10],
#'   y = letters[11:20],
#'   z = colors()[1:10],
#'   w = colors()[401:410]
#' )
#' 
#' .set_long(data, "id")
#' @noRd
.set_long = function(object, id) {
  
  object = split(object, object[, id])
  
  object = lapply(object, function(x){
    
    x = t(x)
    
    x = cbind(row.names(x), x)
    
    i = x[id, 2]
    
    x = cbind(x, id = i)
    
    x = x[ x[,1] != id , ]
    
    x = as.data.frame(x, stringsAsFactors = FALSE)
    
    names(x) = c("variable", "value", "id")
    
    x = x[ ,c("id", "variable", "value")]
    
    return(x)
    
  })
  
  object = do.call("rbind", object)
  
  object = as.data.frame(object, stringsAsFactors = FALSE)
  
  object = with(object, object[order(as.integer(id)), ])
  
  rownames(object) = 1:nrow(object)
  
  return(object)
  
}


#' Set a data.frame into wide format
#' 
#' @param object a data.frame in the long format
#' @param id a character for the id
#' @return a data.frame in the wide format
#' @examples
#' data = data.frame(
#'   id = 1:10,
#'   x = LETTERS[1:10],
#'   y = letters[11:20],
#'   z = colors()[1:10],
#'   w = colors()[401:410]
#' )
#' 
#' data_l = .set_long(data, "id")
#' 
#' .set_wide(data_l, "id")
#' @noRd
.set_wide = function(object, id) {
  
  object = split(object, object[, id])
  
  object = lapply(object, function(x){
    
    x = t(x)
    
    i = as.integer(x[1, 1])
    
    nams = c("id", x[2, ])
    
    val = as.vector(c(i, x[3, ]))
    
    names(val) = nams
    
    return(val)
    
  })
  
  object = do.call("rbind", object)
  
  object = as.data.frame(object, stringsAsFactors = FALSE)
  
  object = with(object, object[order(as.integer(id)), ])
  
  return(object)
}


#' Capitalize for title case sentence
#' 
#' @param x a character
#' @return \code{x} as a capitalized character
#' @examples 
#' zz = c("try this")
#' .title_case(zz)
#' @noRd
.title_case = function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
       x, 
       perl = TRUE)
}

#' Pluralize
#' @param x a character
#' @param p a character to be use in the plural
#' @return \code{x} as a pluralized character
#' @examples 
#' zz = c("observer")
#' .pluralize(zz)
#' @noRd
.pluralize = function(x, p = "s") {
  
  pl = matrix(c("variety","varieties",
                "variedad","variedades",
                "opcion", "optiones"), 
              nrow = 3, ncol = 2, byrow = TRUE)
  
  is_here = x %in% pl[,1]
  if (isTRUE(is_here)) {
    x = pl[pl[,1] %in% x, 2]
  }
  
  if (isFALSE(is_here)) {
    x = paste0(x, p)
  }
  
  return(x)
  
}

#' Decode lkptables
#' @param x a list 
#' @return \code{x} as a data.frame 
#' @noRd
.decode_lkptable = function(x){
  name = x[["name"]]
  name = gsub("lkp", "", name)
  desc = x[["desc"]]
  desc = gsub("Lookup table ", "", desc)
  desc = gsub("[(]", "", desc)
  desc = gsub("[)]", "", desc)
  fields = x[["fields"]]
  values = x[["values"]]
  
  result = list()
  
  for(i in seq_along(name)){
    
    r = cbind(name = name[[i]],
              desc = desc[[i]],
              values[[i]])
    
    names(r) = c("name", "desc", "id", "label")
    
    r$label = .title_case(r$label)
    
    result[[name[[i]]]] = r
    
  }
  
  return(result)
  
}

#' Is a tibble object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that the 
#' object is of class "tbl_df"
#' @examples
#' .is_tibble(airquality)
#' @noRd
.is_tibble = function(object) {
  
  c("tbl_df") %in% class(object)
  
}

#' Decode rankings
#' @param items a matrix with items
#' @param rankings a matrix with rankings
#' @return a matrix with decoded rankings 
#' @examples
#' i = as.data.frame(matrix(NA, nrow = 10, ncol = 5))
#' names(i) = paste0("Item",1:5)
#' 
#' r = as.data.frame(matrix(NA, nrow = 10, ncol = 5))
#' names(r) = paste0("Position_Item",1:5)
#' 
#' for(s in 1:10) {
#'   i[s,] = sample(LETTERS[1:5])
#'   r[s,] = sample(1:5)
#' }
#' 
#' .decode_ranking(i, r)
#' @noRd
.decode_ranking = function(items, rankings) {
  
  nc = ncol(rankings)
  nr = nrow(rankings)
  
  rankings = split(rankings, rownames(rankings))
  
  index = lapply(rankings, function(y) {
    
    order(y, na.last = NA)
    
  })
  
  index = do.call("rbind", index)
  
  ranks = matrix(NA, nrow = nr, ncol = nc)
  
  for (z in seq_len(nc)) {
    
    ranks[, z ] = items[cbind(1:nr, index[, z])]
    
  }
  
  return(ranks)
  
}

