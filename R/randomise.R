#' Randomised group of items
#'
#' Set a randomised group of items for crowdsourcing citizen science.
#' Generate designs for ranking of options. It is designed for tricot trials 
#' specifically (comparing 3 options), but it will also work with comparisons 
#' of any other number of options. 
#' The design strives for approximate A optimality, this means that it is robust 
#' to missing observations. It also strives for balance for positions of each option.
#' Options are equally divided between first, second, third, etc. position. 
#' The strategy is to create a "pool" of combinations that does not repeat 
#' combinations and is A-optimal. Then this pool is ordered to make subsets of 
#' consecutive combinations also relatively balanced and A-optimal
#' 
#' @author Jacob van Etten and Kauê de Sousa
#' @param ncomp an integer for the number of items each observer compares
#' @param nobservers an integer for the number of observers
#' @param nitems an integer for the number of items tested in the project
#' @param itemnames a character for the name of items tested in the project
#' @return A dataframe with the randomised design
#' @examples 
#' ni <- 3
#' no <- 10
#' nv <- 4
#' inames <- c("mango","banana","grape","apple")
#' 
#' randomise(ncomp = ni,
#'           nobservers = no,
#'           nitems = nv,
#'           itemnames = inames)
#'           
#' @aliases randomize
#' @importFrom Matrix Diagonal
#' @importFrom methods as
#' @importFrom RSpectra eigs
#' @importFrom utils combn
#' @export
randomise <- function(ncomp = 3, nobservers = NULL, nitems = NULL, 
                      itemnames = NULL) {
  
  if (is.null(nobservers)) {
    stop("nobservers is missing with no default")
  }
  
  if (is.null(nitems)) {
    stop("nitems is missing with no default")
  }
  
  if (is.null(itemnames)) {
    stop("itemnames is missing with no default")
  }
  
  if (nitems != length(itemnames)) {
    stop("nitems is different than provided itemnames")
  }
  
  # Varieties indicated by integers
  varieties <- 1:nitems
  
  # Full set of all combinations
  varcombinations <- t((utils::combn(varieties, ncomp)))
  
  # if the full set of combinations is small and can be covered at least once
  # the set will include each combination at least once
  ncomb <- dim(varcombinations)[1]
  n <- floor(nobservers / ncomb)
  nfixed <- ncomb * n
  vars1 <-
    varcombinations[c(rep(1:(dim(varcombinations)[1]), times = n)), ]
  
  # the remaining combinations are sampled randomly but in a balanced way
  # this means that no combination enters more than once
  nremain <- nobservers - nfixed
  
  #create set to get to full number of observers
  vars2 <- matrix(nrow = nremain, ncol = ncomp)
  
  # set up array with set of combinations
  varcomb <- matrix(0, nrow = nitems, ncol = nitems)
  
  if (dim(vars2)[1] > 0.5) {
    
    # select combinations for the vars2 set that optimize design
    for (i in 1:nremain) {
      
      # calculate frequency of each variety
      sumcomb <- rowSums(varcomb) + colSums(varcomb)
      
      # priority of each combination is equal to Shannon index of varieties 
      # in each combination
      prioritycomb <- apply(varcombinations, 1, function(x){ 
        .getShannonVector(x, sumcomb, nitems)
      })
      
      # highest priority to be selected is the combination which has the lowest 
      # Shannon index
      selected <- which(prioritycomb == min(prioritycomb))
      
      # if there are ties, find out which combination reduces Kirchhoff index most
      
      if (length(selected) > 1 & i > 25) {
        
        # randomly subsample from selected if there are too many combinations to check
        if (length(selected) > 10) {
          selected <- sample(selected, 10)
        }
        
        # get a nitems x nitems matrix with number of connections
        
        # calculate Kirchhoff index and select smallest value
        khi <- vector(length = length(selected))
        for (k in 1:length(selected)) {
          
          evalgraph <- varcomb
          index <- t(combn(varcombinations[selected[k],], 2))
          evalgraph[index] <- evalgraph[index] + 1
          khi[k] <- .KirchhoffIndex(evalgraph)
          
        }
        
        selected <- selected[which(khi == min(khi))]
        
      }
      
      # if there are still ties between ranks of combinations, selected randomly 
      # from the ties
      if (length(selected) > 1) { 
        selected <- sample(selected, 1)
      }
      
      # assign the selected combination
      vars2[i,] <- varcombinations[selected,]
      
      varcomb[t(combn(varcombinations[selected,],2))] <- 
        varcomb[t(combn(varcombinations[selected,],2))] + 1
      
      # remove used combination
      varcombinations <- varcombinations[-selected,]
      
    }
  }
  
  # merge vars1 and vars2 to create the full set of combinations
  vars <- rbind(vars1, vars2)
  
  # create empy object to contain ordered combinations of vars
  varOrdered <- matrix(NA, nrow = nobservers, ncol = ncomp)
  
  # set up array with set of combinations
  varcomb <- matrix(0, nrow = nitems, ncol = nitems)
  
  # fill first row
  selected <- sample(1:nobservers, 1)
  varcomb[t(combn(vars[selected,],2))] <- 1
  varOrdered[1,] <- vars[selected,]
  vars <- vars[-selected,]
  
  # optimize the order of overall design by repeating a similar procedure to the above
  for (i in 2:(nobservers-1)) {
    
    # calculate frequency of each variety
    sumcomb <- rowSums(varcomb) + colSums(varcomb)
    
    # priority of each combination is equal to Shannon index of varieties 
    # in each combination
    prioritycomb <- apply(vars, 1, function(x){ 
      .getShannonVector(x, sumcomb, nitems)
    })
    
    # highest priority to be selected is the combination which has the 
    # lowest Shannon index
    selected <- which(prioritycomb == min(prioritycomb))
    
    # if there are ties, find out which combination reduces Kirchhoff index most
    if (length(selected) > 1 & i > 25) {
      
      # randomly subsample from selected if there are too many combinations to check
      if (length(selected) > 10) {
        selected <- sample(selected, 7)
      }
      
      # get a nitems x nitems matrix with number of connections
      sumcombMatrix <- varcomb * 0
      
      # in this case, get matrix to calculate Kirchhoff index only for 
      # last 10 observers
      for (j in max(1,i-10):(i-1)) {
        index <- t(combn(varOrdered[j,],2))
        sumcombMatrix[index] <- sumcombMatrix[index] + 1
        
      }
      
      # calculate Kirchhoff indices for the candidate matrix corresponding
      # to each row in selected
      khi <- vector(length = length(selected))
      for (k in 1:length(selected)) {
        
        evalgraph <- sumcombMatrix
        index <- t(combn(vars[selected[k],], 2))
        evalgraph[index] <- evalgraph[index] + 1
        khi[k] <- .KirchhoffIndex(evalgraph)
        
      }
      
      # select combination that produces the lowest Kirchhoff index
      selected <- selected[which(khi == min(khi, na.rm=TRUE))]
      
    }
    
    # if there are still ties between ranks of combinations, select one randomly
    if (length(selected) > 1) { 
      selected <- sample(selected, 1) 
    }
    
    # assign the selected combination
    varOrdered[i,] <- vars[selected,]
    varcomb[t(combn(vars[selected,],2))] <- varcomb[t(combn(vars[selected,],2))] + 1
    
    # remove used combination
    vars <- vars[-selected, ]
    
  }
  
  # assign last one
  varOrdered[nobservers,] <- vars
  
  # Equally distribute positions to achieve order balance
  # First create matrix with frequency of position of each of nitems
  position <- matrix(0, ncol = ncomp, nrow = nitems)
  
  # Sequentially reorder sets to achieve evenness in positions
  # Shannon is good here, because evenness values are proportional
  # the H denominator in the Shannon formula is the same
  
  for (i in 1:nobservers) {
    
    varOrdered_all <- .getPerms(varOrdered[i,])
    varOrdered_Shannon <- apply(varOrdered_all, 1, function(x) {
      .getShannonMatrix(x, position)
    })
    varOrdered_i <- varOrdered_all[which(varOrdered_Shannon == 
                                           min(varOrdered_Shannon))[1],]
    varOrdered[i,] <- varOrdered_i
    pp <- position * 0
    pp[cbind(varOrdered_i,1:ncomp)] <- 1
    position <- position + pp
    
  }
  
  # The varOrdered matrix has the indices of the elements
  # Create the final matrix
  finalresults <- matrix(NA, ncol = ncomp, nrow = nobservers)
  
  # loop over the rows and columns of the final matrix and put
  # the elements randomized
  # with the indexes in varOrdered
  for (i in 1:nobservers){
    for (j in 1:ncomp){
      finalresults[i,j] <- itemnames[varOrdered[i,j]]
    }
  }
  
  finalresults <- tibble::as_tibble(finalresults)
  
  names(finalresults) <- paste0("item_", LETTERS[1:ncomp])
  
  return(finalresults)
  
}

#' @inheritParams randomise
#' @export
randomize <- function(...){
  
  randomise(...)
  
}

# Define function for Kirchhoff index
# This index determines which graph is connected in the most balanced way
# In this context, lower values (lower resistance) is better
.KirchhoffIndex <- function(x) {
  # The input matrix only has one triangle filled
  # First we make it symmetric
  x <- x + t(x)
  
  # Then some maths to get the Kirchhoff index
  
  # Using rARPACK:eigs, setting k to n-1 because we don't need the 
  # last eigen value
  Laplacian <- methods::as(Matrix::Diagonal(x = colSums(x)) - x, 
                           "dsyMatrix")
  lambda <-
    try(RSpectra::eigs(Laplacian,
                       k = (dim(Laplacian)[1] - 1),
                       tol = 0.01,
                       retvec = FALSE)$values, silent = TRUE)
  if (inherits(lambda, "try-error"))
    lambda <- Inf
  # RSpectra:eigs is faster than base:eigen
  # lambda <- eigen(Laplacian)$values
  # lambda <- lambda[-length(lambda)]
  
  return(sum(1 / lambda))
  
}

# get all permutations
.getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}

# Shannon (as evenness measure)
.shannon <- function(x){
  sum(ifelse(x == 0, 0, x * log(x)))
}

# Get Shannon index for order positions
.getShannonMatrix <- function(x, position) {
  pp <- position * 0
  pp[cbind(x, 1:length(x))] <- 1
  pp <- position + pp
  return(.shannon(as.vector(pp)))
  
}

.getShannonVector <- function(x, sumcomb, nitems) {
  xi <- rep(0, times = nitems)
  xi[x] <- 1
  return(.shannon(sumcomb + xi))
  
}