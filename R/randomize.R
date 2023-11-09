#' Set an experimental incomplete block design 
#'
#' Generate an incomplete block A-optional design. The function is optimized for 
#' incomplete blocks of three, but it will also work with comparisons of any 
#' other number of options. 
#' The design strives for approximate A optimality, this means that it is robust 
#' to missing observations. It also strives for balance for positions of each option.
#' Options are equally divided between first, second, third, etc. position. 
#' The strategy is to create a "pool" of combinations that does not repeat 
#' combinations and is A-optimal. Then this pool is ordered to make subsets of 
#' consecutive combinations also relatively balanced and A-optimal
#' 
#' @author Jacob van Etten
#' @param ncomp an integer for the number of items to be assigned to each incomplete block
#' @param npackages an integer for the number of incomplete blocks to be generated
#' @param itemnames a character for the name of items tested in the experiment
#' @param availability optional, a vector with integers indicating the 
#'  number of plots available for each \var{itemnames}
#' @param props optional, a numeric vector with the desired proportions
#'  for each \var{itemnames}
#' @param ... additional arguments passed to methods 
#' @references 
#'  Bailey and Cameron (2004). Combinations of optimal designs. 
#'   \url{https://webspace.maths.qmul.ac.uk/l.h.soicher/designtheory.org/library/preprints/optimal.pdf}
#' @return A dataframe with the randomized design
#' @examples 
#' ncomp = 3
#' npackages = 20
#' itemnames = c("apple","banana","grape","mango", "orange")
#' availability = c(5, 8, 50, 50, 50)
#' 
#' randomize(ncomp = ncomp,
#'           npackages = npackages,
#'           itemnames = itemnames)
#' 
#' randomize(ncomp = ncomp,
#'           npackages = npackages,
#'           itemnames = itemnames,
#'           availability = availability)
#'           
#' @aliases randomise
#' @importFrom Matrix Diagonal
#' @importFrom methods as
#' @importFrom RSpectra eigs
#' @importFrom utils tail
#' @importFrom stats runif
#' @importFrom lpSolve lp
#' @export
randomize <- function(npackages, 
                      itemnames, 
                      ncomp = 3, 
                      availability = NULL,
                      props = NULL, 
                      ...) {
  
  dots <- list(...)
  
  comp <- dots[["comp"]]
  
  if (is.null(comp)) {
    comp <- 10
  }
  
  nitems <- length(itemnames)
  
  # depth1 is the number of rows after the procedure starts to compare options with the Kirchoff index
  depth1 <- floor(nitems / ncomp )
  
  # Varieties indicated by integers
  varieties <- seq_len(nitems)
  
  # Full set of all combinations
  varcombinations <- .combn(varieties, ncomp)
  
  # check inputs
  if (!is.null(availability) & (length(availability) != nitems)) {
    stop("nitems is different than length of vector with availability")
  }
  
  if (nitems < 3) {
    stop("nitems must be larger than 2")
  }
  
  nneeded <- npackages * ncomp
  
  if (!is.null(availability)) {
    
    if (sum(availability) < nneeded) {
      stop("availability is not sufficient: smaller than npackages * ncomp \n")
    }
    if (length(availability) != nitems) {
      stop("length of vector availability should be nitems \n")
    }
  }
  
  if (!is.null(props)) {
    
    if (length(props) != nitems) {
      stop("length of vector props should be nitems")
      
    }
    
    if (sum(props) != 1) {
      
      props <- props / sum(props)
      warning("sum of props is not 1; values have been rescaled \n")
      
    }
  }
  
  #create the objects available or props if they are not available
  if (is.null(availability)) {
    availability <- rep(npackages, times = nitems)
  }
  
  available <- availability
  
  if (is.null(props)) {
    props <- rep(1/nitems, times = nitems) #available / sum(available) 
  }
  
  #order vector from low availability to high as .smart.round will favour right size of vector
  #to resolve dilemmas, it will add more of the items that are more abundant
  names(available) <- varieties
  available <- sort(available)
  props <- props[as.integer(names(available))]    
  
  # calculate the packages that are needed - this will round later numbers to higher values 
  # if needed to fill the quota
  needed <- .smart.round((nneeded * props) / sum(props)) 
  
  # prepare inputs into loop
  allocations <- rep(0, times = nitems)
  names(allocations) <- names(available) #just to check, can be removed
  tremain <- 1 
  
  while(tremain > 0) {
    
    allocate <- pmin(available, needed)
    available <- available - allocate
    
    allocations <- allocations + allocate
    tremaining <- nneeded - sum(allocations)
    
    needed <- available > 0
    needed <- needed * (tremaining / sum(needed))
    needed <- .smart.round(needed)
    
    tremain <- tremaining 
    
  }
  
  #reorder the vector with allocations back to original order
  allocations <- allocations[match(1:nitems, as.integer(names(available)))] #should be in ascending order
  allocationsMatrix <- makeAllocationsMatrix(allocations)
  
  # prepare variables
  ncomb <- dim(varcombinations)[1]
  n <- floor(npackages / ncomb)
  nfixed <- ncomb * n
  n <- ceiling(npackages / ncomb)
  
  # make a set of variety combinations
  # repeating the combinations if the unique combinations are not sufficient
  vars <- varcombinations[c(rep(1:(dim(varcombinations)[1]), times = n)), ]
  
  # create matrix that will hold the blocks
  blocks <- matrix(nrow = npackages, ncol = ncomp)
  
  # set up array with set of combinations
  varcomb <- matrix(0, nrow = nitems, ncol = nitems)
  
  if (dim(vars)[1] > 0.5) {
    
    # select combinations for the vars set that optimize design
    for (i in 1:npackages) {
      
      varcombScore <- apply(varcombinations, 1, function(x){
        .getScoreBlocks(x, varcomb, allocationsMatrix)
      })
      # highest priority to be selected is the combination which has the highest score 
      selected <- which(varcombScore >= (max(varcombScore)))
      
      # if there are ties, find out which combination reduces Kirchhoff index most
      
      if (length(selected) > 1 & i > depth1) {
        
        reduce <- max(2, min(length(selected), comp))
        
        # randomly subsample from selected if there are too many combinations to check
        if (length(selected) > reduce) {
          selected <- sample(selected, reduce)
        }
        
        # calculate Kirchhoff index and select smallest value
        khi <- vector(length = length(selected))
        for (k in 1:length(selected)) {
          
          evalgraph <- varcomb
          index <- .combn(varcombinations[selected[k],], 2)
          evalgraph[index] <- evalgraph[index] + 1
          evalgraph[cbind(index[,2], index[,1])] <- evalgraph[cbind(index[,2], index[,1])] + 1
          khi[k] <- .KirchhoffIndex(evalgraph / (allocationsMatrix+diag(nrow(allocationsMatrix))))
          
        }
        
        selected <- selected[which(khi == min(khi))]
        
      }
      
      # if there are still ties between ranks of combinations, selected randomly 
      # from the ties
      if (length(selected) > 1) { 
        selected <- sample(selected, 1)
      }
      
      # assign the selected combination
      blocks[i,] <- varcombinations[selected,]
      
      index <- .combn(varcombinations[selected,],2)
      varcomb[index] <- varcomb[index] + 1
      varcomb[cbind(index[,2], index[,1])] <- varcomb[cbind(index[,2], index[,1])] + 1
      
    }
  }
  
  varOrdered <- blocks
  
  # Equally distribute positions to achieve order balance
  # First create matrix with frequency of position of each of nitems
  position <- matrix(0, ncol = ncomp, nrow = nitems)
  
  # Sequentially reorder sets to achieve evenness in positions
  # Shannon represents evenness here
  # the H denominator in the Shannon formula is the same
  
  for (i in 1:npackages) {
    
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
  finalresults <- matrix(NA, ncol = ncomp, nrow = npackages)
  
  # loop over the rows and columns of the final matrix and put
  # the elements randomized
  # with the indexes in varOrdered
  for (i in seq_len(npackages)){
    for (j in seq_len(ncomp)){
      finalresults[i,j] <- itemnames[varOrdered[i,j]]
    }
  }
  
  dimnames(finalresults) <- list(seq_len(npackages), 
                                 paste0("item_", LETTERS[1:ncomp]))
  
  finalresults <- as.data.frame(finalresults, stringsAsFactors = FALSE)
  
  r <- table(unlist(finalresults))[itemnames] 
  
  if (!is.null(availability)){
    if (!all(r <= availability)) {
      
      few <- itemnames[!r <= availability]
      nfew <- availability[!r <= availability]
      nmin <- r[!r <= availability]
      
      warning("You indicated the availability of ", paste(nfew, collapse = ", "), " packages for ", 
              paste(few, collapse = ", "), " but you require a minimum of ", 
              paste(nmin, collapse = ", "), " for the given items. \nYou could try to run the randomization again to solve this issue." )
      
    }
  }
  
  class(finalresults) <- union("CM_df", class(finalresults))
  
  return(finalresults)
  
}

#' @inheritParams randomize
#' @export
randomise <- function(...){
  
  randomize(...)
  
}
# Define function for Kirchhoff index
# This index determines which graph is connected in the most balanced way
# In this context, lower values (lower resistance) is better
#' @noRd
.KirchhoffIndex <- function(x) {
  
  # Add a tiny bit of noise to avoid zeros
  noise <- x * 0 + stats::runif(length(x))/length(x)^3
  noise <- noise + t(noise)
  x <- x + noise
  
  # Then some maths to get the Kirchhoff index
  
  # Using rARPACK:eigs, setting k to n-1 because we don't need the 
  # last eigen value
  Laplacian <- methods::as(Matrix::Diagonal(x = colSums(x)) - x, 
                           "dsyMatrix")
  
  # RSpectra:eigs is faster than base:eigen
  # The following would also work if we want to reduce a dependency
  # lambda <- eigen(Laplacian)$values
  # lambda <- lambda[-length(lambda)]
  lambda <-
    try(RSpectra::eigs(Laplacian,
                       k = (dim(Laplacian)[1] - 1),
                       tol = 0.01,
                       retvec = FALSE)$values, silent = TRUE)
  if(inherits(lambda, "try-error")){lambda <- Inf}
  
  return(sum(1 / lambda))
  
}

# get all permutations
#' @noRd
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
#' @noRd
.shannon <- function(x){
  sum(ifelse(x == 0, 0, x * log(x)))
}

# Get Shannon index for order positions
#TODO check this function!!!
#' @noRd
.getShannonMatrix <- function(x, position) {
  
  pp <- position * 0
  pp[cbind(x, 1:length(x))] <- 1
  pp <- position + pp
  return(.shannon(as.vector(pp)))
  
}

#' @noRd
.getScoreBlocks <- function(x, varcomb, allocationsMatrix) {
  
  #get combinations from vector of varieties x
  cb <- .combn(x,2)
  
  #get progress
  progress <- sum(varcomb) / sum(allocationsMatrix)
  
  #get score pairwise
  score1 <- varcomb[cb]/allocationsMatrix[cb] < progress
  
  #get score sums
  score2 <- colSums(varcomb)[x]/colSums(allocationsMatrix)[x] < progress
  
  #the smallest available amount should be avoided, so this check penalizes it
  if(any(score2 == FALSE) & min(colSums(varcomb)[x]) == min(colSums(varcomb))){score2[1] <- score2[1]-1}
  
  #calculate total score
  score <- sum(score1+score2)
  
  return(score)
  
}

#' @noRd
makeAllocationsMatrix <- function(allocations){
  
  #prepare basic parameters and empty matrix
  n1 <- length(allocations)
  combs <- .combn(1:n1, 2)
  n2 <- nrow(combs)
  a <- matrix(0,nrow=n1, ncol=n2)
  
  #fill matrix with constraints on row/column sums
  for(i in 1:n1){
    
    a[i,] <- (combs[,1] == i | combs[,2] == i)
    
  }
  
  #include auxiliary variable z to the matrix
  #this variable will be maximized, pushing all values up equally
  minShare <-  pmin(allocations[combs[,1]], allocations[combs[,2]]) / ((n1-1)/2)
  f.con <- rbind(a, -diag(n2)) 
  f.con <- cbind(f.con, c(rep(0, times=n1), minShare))
  
  # set up vector with allocations (column sums) and zeros for z constraint
  f.rhs <- c(allocations, rep(0, times=n2)) 
  
  #objective function emphasizes raising the z value, which increases an equal spread
  f.obj <- c(rep(1, times=n2), max(allocations)^2) 
  f.dir <- rep("<=", times=n1+n2) 
  
  sol <- lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs)
  x <- sol$solution[1:n2]
  result <- matrix(0, nrow=n1, ncol=n1)
  result[combs] <- x
  result <- result + t(result)
  return(result)
  
}

#' @noRd
.combn <- function (x, m, FUN = NULL, simplify = TRUE, ...) 
{
  stopifnot(length(m) == 1L, is.numeric(m))
  if (m < 0) 
    stop("m < 0", domain = NA)
  if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == 
      x) 
    x <- seq_len(x)
  n <- length(x)
  if (n < m) 
    stop("n < m", domain = NA)
  x0 <- x
  if (simplify) {
    if (is.factor(x)) 
      x <- as.integer(x)
  }
  m <- as.integer(m)
  e <- 0
  h <- m
  a <- seq_len(m)
  nofun <- is.null(FUN)
  if (!nofun && !is.function(FUN)) 
    stop("'FUN' must be a function or NULL")
  len.r <- length(r <- if (nofun) x[a] else FUN(x[a], ...))
  count <- as.integer(round(choose(n, m)))
  if (simplify) {
    dim.use <- if (nofun) 
      c(m, count)
    else {
      d <- dim(r)
      if (length(d) > 1L) 
        c(d, count)
      else if (len.r > 1L) 
        c(len.r, count)
      else c(d, count)
    }
  }
  if (simplify) 
    out <- matrix(r, nrow = len.r, ncol = count)
  else {
    out <- vector("list", count)
    out[[1L]] <- r
  }
  if (m > 0) {
    i <- 2L
    nmmp1 <- n - m + 1L
    while (a[1L] != nmmp1) {
      if (e < n - h) {
        h <- 1L
        e <- a[m]
        j <- 1L
      }
      else {
        e <- a[m - h]
        h <- h + 1L
        j <- 1L:h
      }
      a[m - h + j] <- e + j
      r <- if (nofun) 
        x[a]
      else FUN(x[a], ...)
      if (simplify) 
        out[, i] <- r
      else out[[i]] <- r
      i <- i + 1L
    }
  }
  if (simplify) {
    if (is.factor(x0)) {
      levels(out) <- levels(x0)
      class(out) <- class(x0)
    }
    dim(out) <- dim.use
  }
  return(t(out))
}

# Rounding values to closest integer while retaining the same sum
# From https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum
#' @noRd
.smart.round <- function(x) {
  y <- floor(x)
  indices <- utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

# #-----------------Run an example-------------------
# 
# ncomp = 3
# npackages = 238
# vars = 15
# proportions = rep(1, vars)/vars
# itemnames = c(LETTERS[1:vars])
# availability = rep(ceiling(238*3/vars), times=vars)
# availability[2] = availability[2]*2
# availability[5] = availability[5]/2
# 
# a = randomise(ncomp = ncomp,
#               npackages = npackages,
#               itemnames = itemnames,
#               availability = availability)
# 
# 
# a
# 
# #----------Diagnostics-------------
# #the only input assumed here is table a
# #get item names from the table
# ua = sort(unique(c(t(a))))
# ncomp = ncol(a)
# 
# comb_balance = matrix(0, nrow=length(ua), ncol=length(ua))
# 
# for(i in 1:nrow(a)){
# 
#   j = t(combn(match(a[i,], ua),2))
#   comb_balance[j] = comb_balance[j] + 1
# 
# }
# 
# cb = comb_balance + t(comb_balance)
# cb
# #show total number of times options are included in packages
# cb = rbind(cb, rowSums(cb)/2)
# 
# #show result nicely
# rownames(cb) = c(ua, "Total")
# colnames(cb) = ua
# cb
# #check the distances between packages that contain the same option
# #as a measure of sequential balance
# d = matrix(NA, nrow = length(itemnames), ncol=ncomp)
# 
# for(i in 1:length(ua)) {
#   s = apply(a, 1, function(x){sum(x==ua[i])})
#   di = diff(which(s==1))
#   hist(di)
#   d[i,] = c(mean(di), sd(di), max(di))
# }
# colnames(d) = c("mean", "sd", "max")
# rownames(d) = ua
# 
# # print d nicely
# format(as.data.frame(d), digits=3)
# 
# #check if options are equally distributed across columns
# f = matrix(NA, nrow=length(ua), ncol=ncomp)
# rownames(f) = ua
# f
# for(i in 1:ncomp){
# 
#   tai = table(a[,i])
#   f[names(tai),i] = tai
# 
# }
# 
# f
# comb_balance
# # connection graph
# g = graph_from_adjacency_matrix(comb_balance+t(comb_balance), mode = "lower", weighted = "weight")
# g
# plot(g, edge.width = E(g)$weight, edge.label = E(g)$weight)

