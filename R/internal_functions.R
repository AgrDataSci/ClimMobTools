#' Set data.frame into long format
#' 
#' @param object a data.frame in the wide format
#' @param id a character for the id
#' @return a data.frame in the long format
#' @examples
#' data <- data.frame(
#'   id = 1:10,
#'   x = LETTERS[1:10],
#'   y = letters[11:20],
#'   z = colors()[1:10],
#'   w = colors()[401:410]
#' )
#' 
#' .set_long(data, "id")
#' @noRd
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


#' Set a data.frame into wide format
#' 
#' @param object a data.frame in the long format
#' @param id a character for the id
#' @return a data.frame in the wide format
#' @examples
#' data <- data.frame(
#'   id = 1:10,
#'   x = LETTERS[1:10],
#'   y = letters[11:20],
#'   z = colors()[1:10],
#'   w = colors()[401:410]
#' )
#' 
#' data_l <- .set_long(data, "id")
#' 
#' .set_wide(data_l, "id")
#' @noRd
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


#' Capitalize for title case sentence
#' 
#' @param x a character
#' @return \code{x} as a capitalized character
#' @examples 
#' zz <- c("try this")
#' .title_case(zz)
#' @noRd
.title_case <- function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
       x, 
       perl = TRUE)
}

#' Pluralize
#' @param x a character
#' @param p a character to be use in the plural
#' @return \code{x} as a pluralized character
#' @examples 
#' zz <- c("observer")
#' .pluralize(zz)
#' @noRd
.pluralize <- function(x, p = "s") {
  paste0(x, p)
}

#' Decode arguments from ClimMob3
#' 
#' @param x a list of arguments given by ClimMob
#' @return a list with data frames for:
#'  chars: characteristics to be analysed, 
#'  perf:  the comparison between tested items and the local item
#'  expl:  the explanatory variables
#' @noRd
.decode_pars <- function(x) {

  chars <- x[["Characteristics"]]
  perf  <- x[["Performance"]]
  expl  <- x[["Explanatory"]]
  
  result <- list()
  
  if (length(chars) > 0) {
    
    questions <- lapply(chars$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("quest_", seq_len(dim(questions)[[2]]))
    
    questions$n_quest <- dim(questions)[[2]]
    
    questions$char_full <- chars$name
    
    questions$char <- gsub(" ","_",tolower(chars$name))
    
    # look for the overall performance
    index_overall <- which(grepl("overall", questions$char))
    # it may happen that this question is done twice
    # so for that cases, the last is taken
    index_overall <- index_overall[length(index_overall)]
    index_overall <- questions$char[index_overall]
    # put overall as first trait
    traits <- union(index_overall, questions$char)
    
    questions <- questions[match(traits, questions$char), ]
    
    rownames(questions) <- 1:nrow(questions)
    
    result[["chars"]] <- questions
  }
  
  if (length(perf) > 0) {
    questions <- lapply(perf$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("quest_", seq_len(dim(questions)[[2]]))
    
    questions$n_quest <- dim(questions)[[2]]
    
    questions$perf_full <- perf$name
    
    questions$perf <- gsub(" ","_",tolower(perf$name))
    
    result[["perf"]] <- questions
  }else{
    result[["perf"]] <- character(0L)
  }
  
  if (length(expl) > 0) {
    
    result[["expl"]] <- expl
    
  }else{
    pseudo <- data.frame(name = NA,
                         id = "0000",
                         vars = "xinterceptx", 
                         stringsAsFactors = FALSE)
    result[["expl"]] <- pseudo
  }
  
  return(result)
  
}

