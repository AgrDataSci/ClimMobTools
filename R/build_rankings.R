#' Build Plackett-Luce rankings
#'
#' Create an object of class "rankings" from ClimMob data
#'
#' @param data a data frame with columns specified by items and input values to rank.
#' @param items a data frame or index of \code{data} for the column(s) containing the item names
#' @param input a data frame or index of \code{data} for the column(s) containing the values to be ranked
#' @param additional.rank optional, a data frame for the comparisons between tricot items and the local item
#' @param group optional, if TRUE return an object of class "grouped_rankings"
#' @param ... additional arguments passed to methods
#' @return a PlackettLuce "rankings" object, which is a matrix of dense rankings 
#' @seealso \code{\link[PlackettLuce]{rankings}} \code{\link[PlackettLuce]{grouped_rankings}}
#' @examples
#'  
#' # beans data where each observer compares 3 varieties randomly distributed 
#' # from a list of 11 and additionally compares these 3 varieties 
#' # with their local variety
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' # first build rankings with only tricot items
#' # and return an object of class 'rankings'
#' R <- build_rankings(data = beans,
#'                     items = c(1:3),
#'                     input = c(4:5))
#' head(R)
#' 
#' ############################################################
#' 
#' # pass the comparison with local item as an additional rankings, then
#' # each of the 3 varieties are compared separately with the local item
#' # and return an object of class grouped_rankings
#' G <- build_rankings(data = beans,
#'                     items = c(1:3),
#'                     input = c(4:5),
#'                     additional.rank = beans[c(6:8)],
#'                     group = TRUE)
#'                     
#' head(G)
#' 
#' ############################################################
#' 
#' # rankings with more than 3 items in a tricot project
#' # 5 items randomly assigned
#' 
#' input <- as.data.frame(matrix(NA, nrow = 5, ncol = 5))
#' names(input) <- paste0("position_item_",LETTERS[1:5])
#' 
#' for(s in 1:5) {
#'   input[s,] <- sample(1:5)
#' }
#' 
#' items <- randomise(5, 5, 5, c("green","blue","red","white","yellow"))
#' 
#' build_rankings(items = items, 
#'                input = input)
#' 
#' 
#' @import PlackettLuce
#' @export
build_rankings <- function(data = NULL, items = NULL,
                           input = NULL, additional.rank = NULL, 
                           group = FALSE, ...) {
  
  # get extra arguments
  dots <- list(...)
  
  if (is.null(data)) {
    data <- cbind(items, input)
    items <- names(items)
    input <- names(input)
  }
  
  if (!is.data.frame(data)){
    data <- data.frame(data, stringsAsFactors = FALSE)
  }
  
  # get nrow in object
  n <- nrow(data)

  # get the items in data
  items <- data[items]
  
  # if all data is required
  full.output <- dots[["full.output"]]
  full.output <- isTRUE(full.output)
  
  # deal with data of type "tricot"  
  ncomp <- ncol(items)
  
  # with 3 comparisons
  if (ncomp == 3) {
    rrank <- data[input]
    
    if (any(rrank[,1] == rrank[,2])) {
      stop("ties cannot be handled in objects of type 'tricot'\n")
    }
    
    if (any(is.na(unlist(rrank)))) {
      stop("NAs cannot be handled in objects of type 'tricot'\n")
    }
    
    r <- .pivot_triadic(i = items, r = data[input])
    
    # get names of all items
    itemnames <- sort(unique(as.vector(r)))
    
    # convert it into a PlackettLuce rank
    R <- PlackettLuce::as.rankings(r, input = "ordering", items = itemnames)
    
    # if pseudo-item were added, it is removed
    pseudo <- grepl("pseudoitem", itemnames) 
    if (any(pseudo)) {
      R <- R[, !pseudo]
    }
  }
  
  # with 4 or more comparisons
  if (ncomp >= 4) {
    r <- .pivot_tetra(i = items, r = data[input])
    
    # make a PlackettLuce rankings
    R <- PlackettLuce::as.rankings(r)
    
  }
  
  # check if additional rankings are required
  if (!is.null(additional.rank)) {
    # add comparisons with local rankings
    R <- .additional_rankings(i = items, R = R, add = additional.rank)
  }
  
  # and into a grouped_rankings
  gi <- rep(seq_len(n), (nrow(R) / n))
  G <- PlackettLuce::group(R, index = gi)
  
  # check if all data is required
  if (full.output) {
    R <- list(PLranking = R, PLgrouped = G, myrank = r)
  }
  
  # return a grouped_rankings if required
  if (group) {
    R <- G
  }
  
  return(R)

}

# this function deals with object in the triadic approach
# in ClimMob when three items are tested by each participant
# i, is a dataframe with items
# r, is a dataframe with rankings 
.pivot_triadic <- function(i, r) {
  
  n <- nrow(i)
  
  # fix names in rankings
  # first column must be the best item
  # and the second the worst
  names(r) <- c("best", "worst")
  
  # rankings should be LETTERS (A, B, C), 
  # we must convert it into factor and then into integer 
  # it allow us to impute the middle-ranked item
  # (a strict ranking is assumed here, so the sum of each row should always be 6)
  r <- within(r, {
    best = as.integer(factor(best, levels = LETTERS[1:3]))
    worst = as.integer(factor(worst, levels = LETTERS[1:3]))
    middle = as.integer(6 - best - worst)
  })
  
  # if there is any NA in items and observations with only two items
  # add a stopper pseudo-item which will be removed later
  if (sum(is.na(i)) > 0)  {
    i[is.na(i)] <- "pseudoitem"
  }
  
  # combine items with rankings
  r <- cbind(i, r)
  
  # convert items into a matrix
  i <- as.matrix(i)
  
  # then replace rankings integers with their respective item names
  r <- within(r, {
    best = i[cbind(seq_len(n), best)]
    worst = i[cbind(seq_len(n), worst)]
    middle = i[cbind(seq_len(n), middle)]
  })
  
  r <- r[, c("best", "middle", "worst")]
  
  r <- as.matrix(r)
  
  return(r)
  
}

# this function deals with object in the tetra approach
# in ClimMob when four or more items are tested by each participant
# i, is a dataframe with items
# r, is a dataframe with rankings 
.pivot_tetra <- function(i, r){
  
  # fix names in r data
  names(r) <- paste0("PosItem", 1:ncol(r))
  
  # get the number of possible rankings
  nrank <- ncol(r)
  # number of rows
  n <- nrow(r)
  
  # if there is any NA in items
  # add a pseudo-item which will be removed later
  if (sum(is.na(i)) > 0)  {
    for (p in seq_len(nrank)) {
      i[is.na(i[p]), p] <- paste0("pseudoitem", p)
    }
  }
  
  # add 0 if there is any missing ranking in r
  if (sum(is.na(r)) > 0)  {
    r[is.na(r)] <- 0
  }
  
  # create an id for the rankings
  id <- 1:n
  
  # combine items with rankings
  r <- cbind(id, i, r)
  
  # put rankings into a long format 
  r <- tidyr::gather(r, 
                     key = "variable",
                     value = "value",
                     names(r)[2:ncol(r)])
  
  # this vector checks which rows are the ranks and which 
  # are the item name
  pr <- grepl("PosItem", r[[2]])
  
  # create separate vectors and then merge it
  id <- r[pr,"id"]
  item <- r[!pr,"value"]
  rank <- as.numeric(r[pr,"value"])
  
  r <- data.frame(id, item, rank, stringsAsFactors = FALSE)
  
  names(r) <- c("id","item","rank")
  
  # if pseudo-item were added, it is removed now
  rmitem <- !r[["item"]] %in% paste0("pseudoitem", 1:nrank)
  r <- r[rmitem, ]
  
  # reshape data into wide format
  r <- tidyr::spread(r, item, rank)
  
  # replace possible NA's with zeros (0) as required for PlackettLuce
  r[is.na(r)] <- 0
  
  # order observations by ids
  r <- r[order(r[,"id"]), ]
  
  # drop id
  r <- r[ ,-match("id", names(r))]
  
  # dataframe into matrix
  r <- as.matrix(r)
  
  return(r)
  
} 

# this function adds additional ranks, generally when a local item 
# is tested against the tricot items
# i, is a dataframe with items
# R, is an object of class rankings from PlackettLuce
# add, is a dataframe with additional rankings characters 
## indication whether the tricot items performed "Better" or "Worse" 
## compared to the local item
.additional_rankings <- function(i, R, add){
  
  n <- nrow(add)
  
  ncomp <- ncol(i)
  
  # convert it into characters
  add[1:ncol(add)] <- lapply(add[1:ncol(add)], as.character)
  
  add <- as.matrix(add)
  
  i <- as.matrix(i)
  
  # treat these comparisons as additional rankings.
  # first we convert the orderings of the items to 
  # sub-rankings of the full set of items including the additional items 
  # so we add the paired comparisons
  
  # the comparisons with the additional items are stored 
  # in another set of columns
  
  # make sure that values in add are integers 
  # where 1 means Better and 2 means Worse
  add <- apply(add, 2, function(x) {
    x <- ifelse(x == "Better" | x == 1, 1,
                ifelse(x == "Worse" | x == 2, 2, NA))
    x
  })
  
  # stop if any NA
  if (any(is.na(add))) {
    "NAs are not allowed in additional rankings"
  }
  
  # add local to itemnames
  itemnames <- dimnames(R)[[2]]
  itemnames <- unique(c("Local", itemnames))
  
  paired <- list()
  
  for (p in seq_len(ncomp)) {
    ordering <- matrix("Local", nrow = n, ncol = 2)
    worse <- add[, p] == 2
    # name of winner
    ordering[!worse, 1] <- i[, p][!worse]
    # name of loser
    ordering[worse, 2] <- i[, p][worse]
    paired[[p]] <- ordering
  }
  
  # we then convert these orderings to sub-rankings of the full set of items
  # and combine them with the rankings
  paired <- lapply(paired, function(x) {
    x <- PlackettLuce::as.rankings(x, input = "ordering", items = itemnames)
  })
  
  paired <- do.call("rbind", paired)
  
  R <- rbind(R, paired)  
  
  return(R)
  
}
