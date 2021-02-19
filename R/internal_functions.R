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
  
  pl <- matrix(c("variety","varieties",
                 "variedad","variedades",
                 "opcion", "optiones"), 
               nrow = 3, ncol = 2, byrow = TRUE)
  
  is_here <- x %in% pl[,1]
  if (isTRUE(is_here)) {
    x <- pl[pl[,1] %in% x, 2]
  }
  
  if (isFALSE(is_here)) {
    x <- paste0(x, p)
  }
  
  return(x)
  
}

#' Decode arguments from ClimMob3
#' 
#' @param x a list of arguments given by ClimMob
#' @return a list with data frames for:
#'  traits: characteristics to be analysed, 
#'  tricotVSlocal:  the comparison between tested items and the local item
#'  covariates:  the explanatory variables
#' @noRd
.decode_pars <- function(x) {

  traits <- x[["Characteristics"]]
  tricotVSlocal  <- x[["Performance"]]
  covariates  <- x[["Explanatory"]]
  
  result <- list()
  
  if (length(traits) > 0) {
    
    questions <- lapply(traits$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst <- dim(questions)[[2]]
    
    questions$name <- traits$name
    
    questions$codeQst <- traits$codeQst
    
    
    questionAsked <- do.call(rbind, traits$questionAsked)
    questionAsked <- as.data.frame(questionAsked, stringsAsFactors = FALSE)
    names(questionAsked) <- paste0("questionAsked", seq_len(questions$nQst[1]))
    
    questions <- cbind(questions, questionAsked)
    
    questions$assessmentId <- traits$code$ass_cod
    
    questions$assessmentName <- traits$code$ass_desc
    
    questions$assessmentDay <- traits$code$ass_days
    
    questions$traitOrder  <- rep("otherTraits", length(questions$codeQst))
    
    # try to find something related to performance, than yield and lastly taste
    # this is going to be the reference trait for the main analysis in the report
    traits <- questions$codeQst
    tr <- tolower(traits)
    
    if (any(grepl("overallperf", tr))) {
      i <- which(grepl("overallperf", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    
    if (any(grepl("performance", tr))) {
      i <- which(grepl("performance", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    if (any(grepl("yield", tr))) {
      i <- which(grepl("yield", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    if (any(grepl("taste", tr))) {
      i <- which(grepl("taste", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
    }
    
    if (sum(grepl("performance|yield|taste|overallperf", tolower(tr))) == 0) {
      questions$traitOrder[length(questions$codeQst)] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    rownames(questions) <- 1:nrow(questions)
    
    result[["traits"]] <- questions
    
  }
  
  if (length(tricotVSlocal) > 0) {
    questions <- lapply(tricotVSlocal$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("quest_", seq_len(dim(questions)[[2]]))
    
    questions$nQuest <- dim(questions)[[2]]
    
    questions$name <- tricotVSlocal$name
    
    questions$codeQst <- tricotVSlocal$codeQst
    
    result[["tricotVSlocal"]] <- questions
    
  }else{
    result[["tricotVSlocal"]] <- character(0L)
  }
  
  if (length(covariates) > 0) {
    
    covar               <- covariates[,c("codeQst", "id")]
    covar$nameString    <- covariates$vars
    covar$name          <- covariates$name 
    covar$questionAsked <- covariates$questionAsked
    
    if (all(is.na(covariates$code))) {
      
      covar$assessmentId <- "000000000000"
      covar$assessmentName <- "Registration"
      covariates <- covar
      
    }else{
      
      covariates <- covariates$code[,c("ass_cod","ass_desc")]
      names(covariates) <- c("assessmentId", "assessmentName")
      covariates$assessmentId[is.na(covariates$assessmentId)] <- "000000000000"
      covariates$assessmentName[is.na(covariates$assessmentName)] <- "Registration"
      covariates <- cbind(covariates, covar)
      
    }
    
    result[["covariates"]] <- covariates
    
  }else{
    
    result[["covariates"]] <- character(0L)
    
  }
  
  return(result)
  
}



#' Decode lkptables
#' @param x a list 
#' @return \code{x} as a data.frame 
#' @noRd
.decode_lkptable <- function(x){
  name <- x[["name"]]
  name <- gsub("lkp", "", name)
  desc <- x[["desc"]]
  desc <- gsub("Lookup table ", "", desc)
  desc <- gsub("[(]", "", desc)
  desc <- gsub("[)]", "", desc)
  fields <- x[["fields"]]
  values <- x[["values"]]
  
  result <- list()
  
  for(i in seq_along(name)){
    
    r <- cbind(name = name[[i]],
               desc = desc[[i]],
               values[[i]])
    
    names(r) <- c("name", "desc", "id", "label")
    
    r$label <- .title_case(r$label)
    
    result[[name[[i]]]] <- r
    
  }
  
  return(result)
  
}


