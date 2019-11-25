#' Favourability scores
#'
#' Compute favourability scores
#' 
#' @inheritParams build_rankings
#' @param reorder logical, if items should be reordered from higher favourability score to least favourability score
#' @aliases favorite
#' @return a data.frame with descriptive statistics
#' \item{N}{number of times the given item was evaluated}
#' \item{best}{relative number of times (in percentage) the given item was ranked as first}
#' \item{worst}{relative number of times (in percentage) the given item was ranked as worst}
#' \item{wins}{relative number of times (in percentage) the given item wins against the others}
#' \item{fav_score}{the favourability score, which is the difference between best and worst performance}
#' @examples
#' 
#' # beans data where each observer compares 3 varieties randomly distributed 
#'  
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' favourite(data = beans,
#'           items = c(1:3),
#'           input = c(4:5))
#' 
#' # ...........................................
#' # ...........................................
#' 
#' 
#' # rankings with five items in a ClimMob project
#' 
#' items <- randomise(5, 5, 5, c("green","blue","red","white","yellow"))
#' 
#' input <- as.data.frame(matrix(NA, nrow = 5, ncol = 5))
#' names(input) <- paste0("position_item_",LETTERS[1:5])
#' 
#' for(s in 1:5) {
#'   input[s,] <- sample(1:5)
#' }
#' 
#' fav <- favourite(items = items, 
#'                  input = input)
#' @export
favourite <- function(data = NULL, items = NULL, input = NULL, reorder = TRUE){
  
  # keep only target columns in data
  if (!is.null(data)) {
    items <- names(data[, items])
    input <- names(data[, input])
    data <- data[, c(items, input)]
  }
  
  # if 'items' and 'input' are provided as data.frame
  # put all together as 'data'
  if (is.null(data)) {
    data <- cbind(items, input)
    items <- names(items)
    input <- names(input)
  }
  
  # if 'data' is an object of class tbl_df
  # convert to "data.frame"
  if (.is_tibble(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  
  # check for NAs within data
  nalist <- apply(data, 2, is.na)
  nalist <- !apply(nalist, 1, any)
  
  # apply vector to data
  data <- data[nalist, ]
  
  # take decoded rankings
  dataR <- build_rankings(data = data,
                          items = items, 
                          input = input, 
                          full.output = TRUE)[[3]]

  # get names of tested items
  itemnames <- sort(unique(as.vector(dataR)))
  
  # items ranked as first (best)
  firstR <- dataR[,1]
  
  # item ranked as last (worst)
  lastR <- dataR[,ncol(dataR)]
  
  
  # run over items to check the number of times it is ranked
  # as first and last 
  inrow <- NULL
  wins <- NULL
  losses <- NULL
  
  for (i in seq_along(itemnames)) {
    # check the row where item i is present
    inrow_i <- apply(data[, items], 1, function(x) {
      y <- any(x == itemnames[i])
      as.integer(y)
    })
    
    # put it into a single matrix
    inrow <- cbind(inrow, inrow_i)
    
    # check where item i is ranked as first (best)
    wins <- cbind(wins, ifelse(firstR == itemnames[i], 1, 0))
    
    # check where item i is ranked as last (worst)
    losses <- cbind(losses, ifelse(lastR == itemnames[i], 1, 0))
    
  }
  
  # name matrixes
  colnames(inrow)  <- paste("n", 1:(length(itemnames)), sep = "")
  colnames(wins)   <- paste("b", 1:(length(itemnames)), sep = "")
  colnames(losses) <- paste("w", 1:(length(itemnames)), sep = "")
  
  # compute
  # best performance
  best_per <- 100 * colSums(wins) / colSums(inrow)
  
  # worst performance
  worst_per <- 100 * colSums(losses) / colSums(inrow)
  
  # times it wins
  wins <- ((2 * colSums(wins)) + colSums(inrow - wins - losses)) / (2 * colSums(inrow))
  
  # favourability score
  fav_score <- best_per - worst_per
  
  sumstats <- tibble::as_tibble(data.frame(items = itemnames,
                                           N = colSums(inrow),                            
                                           best =  best_per,
                                           worst = worst_per,
                                           wins = wins,
                                           fav_score = fav_score))
  

  if (reorder) {
    sumstats <- sumstats[rev(order(sumstats$fav_score)), ]
  }
  
  return(sumstats)

}

#' @inheritParams favourite
#' @export
favorite <- function(...){
  
  favourite(...)
  
}