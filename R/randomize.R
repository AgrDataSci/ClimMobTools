#' Set an experimental incomplete block design
#'
#' Generate a sequentially balanced incomplete block design for tricot
#' trials. Every prefix of the design is kept close to balanced, so the
#' design stays good when only the first packages are distributed or
#' returned.
#'
#' At each step the function
#' \enumerate{
#'   \item keeps only blocks whose items all have remaining quota (hard
#'   availability);
#'   \item prefers blocks with the most items behind their replication
#'   schedule;
#'   \item breaks ties by the exact A-optimality gain of the prefix,
#'   evaluated for all tied candidates with a rank-k update of the
#'   Laplacian inverse;
#'   \item breaks remaining ties at random.
#' }
#' Item order within blocks is then balanced sequentially across positions.
#' Call \code{set.seed()} before the function to make the design
#' reproducible.
#'
#' @author Jacob van Etten
#' @param npackages an integer for the number of incomplete blocks (packages)
#' @param itemnames a character vector with the unique names of the items
#' @param ncomp an integer for the number of items in each block (default 3)
#' @param availability optional, an integer vector with the maximum number
#'  of plots for each item in \var{itemnames}
#' @param props optional, a numeric vector with the desired proportions for
#'  each item in \var{itemnames}
#' @param ridge small ridge added to the Laplacian to handle a disconnected
#'  early design. Disconnected components are then strongly penalised
#' @param polish logical, if \code{TRUE} run a final local search on the
#'  position balance of the complete design. This can reduce the position
#'  balance of prefixes
#' @param schedule logical, if \code{TRUE} (the default) use the replication
#'  schedule as primary key. If \code{FALSE}, select on A-optimality alone
#'  (slower, similar quality)
#' @param ... arguments of the previous algorithm, such as \code{comp},
#'  are accepted for compatibility and ignored with a warning
#' @references
#'  Bailey and Cameron (2004). Combinations of optimal designs.
#'   \url{https://webspace.maths.qmul.ac.uk/l.h.soicher/designtheory.org/library/preprints/optimal.pdf}
#' @return A data.frame with one row per package and the columns item_A,
#'  item_B, ...
#' @examples
#' set.seed(1)
#' design = randomize(npackages = 20,
#'                    itemnames = c("apple", "banana", "grape", "mango", "orange"))
#' table(unlist(design))
#'
#' # limited availability of some items
#' set.seed(1)
#' randomize(npackages = 20,
#'           itemnames = c("apple", "banana", "grape", "mango", "orange"),
#'           availability = c(5, 8, 50, 50, 50))
#'
#' @aliases randomise
#' @importFrom utils combn tail
#' @export
randomize <- function(npackages, itemnames, ncomp = 3, availability = NULL,
                      props = NULL, ridge = 1e-3, polish = FALSE,
                      schedule = TRUE, ...) {

  # arguments of the previous algorithm (such as comp) are accepted and ignored
  dots <- list(...)
  if (length(dots) > 0) {
    warning("arguments ignored by randomize(): ", paste(names(dots), collapse = ", "),
            call. = FALSE)
  }

  n <- length(itemnames)
  k <- as.integer(ncomp)
  N <- as.integer(npackages)

  # ---- input checks -------------------------------------------------------
  if (anyDuplicated(itemnames)) stop("itemnames must be unique")
  if (k < 2) stop("ncomp must be at least 2")
  if (n < k) stop("number of items must be at least ncomp")
  if (N < 1) stop("npackages must be at least 1")
  if (is.null(props)) props <- rep(1, n)
  if (length(props) != n) stop("length of props must equal number of items")
  if (any(props < 0) || sum(props) <= 0) stop("props must be non-negative with a positive sum")
  props <- props / sum(props)
  if (is.null(availability)) availability <- rep(N, n)
  if (length(availability) != n) stop("length of availability must equal number of items")
  availability <- pmin(availability, N)          # an item can appear at most once per block
  if (sum(availability) < N * k) stop("availability is smaller than npackages * ncomp")

  # ---- quotas: proportional targets capped by availability (water-filling) --
  alloc <- numeric(n)
  left <- N * k
  repeat {
    free <- alloc < availability & props > 0
    if (!any(free)) free <- alloc < availability   # fall back to items with zero props
    w <- props * free
    if (sum(w) == 0) w <- as.numeric(free)
    tgt <- .smart.round(left * w / sum(w))
    add <- pmin(tgt, availability - alloc)
    if (sum(add) == 0) {                          # rounding stalled: add one plot at a time
      add[which(free)[which.max((availability - alloc)[free])]] <- 1
    }
    alloc <- alloc + add
    left <- N * k - sum(alloc)
    if (left <= 0) break
  }

  # ---- greedy block selection -----------------------------------------------
  V <- t(utils::combn(n, k))                      # all candidate blocks
  S <- k * diag(k) - 1                            # Laplacian of one block (clique)
  used <- numeric(n)
  L <- matrix(0, n, n)
  blocks <- matrix(0L, N, k)
  relaxed <- FALSE

  for (i in seq_len(N)) {
    remaining <- alloc - used
    has_quota <- matrix(remaining[V] > 0, ncol = k)
    ok <- rowSums(has_quota) == k
    # an item whose remaining quota equals the remaining packages must be in
    # this block, otherwise its quota can no longer be met (keeps the rest feasible)
    forced <- remaining >= N - i + 1
    if (any(forced)) ok <- ok & rowSums(matrix(forced[V], ncol = k)) == min(sum(forced), k)
    if (!any(ok)) {                               # quotas cannot be met exactly
      relaxed <- TRUE
      nq <- rowSums(has_quota)
      ok <- nq == max(nq)
    }
    if (schedule) {
      behind <- used / pmax(alloc, 1) < (i - 1) / N
      s <- rowSums(matrix(behind[V], ncol = k))
    } else {
      s <- numeric(nrow(V))
    }
    s[!ok] <- -Inf
    cand <- which(s == max(s))

    if (length(cand) > 1) {
      # trace((L + J/n + ridge I)^-1) after adding block e equals
      # trace(M) - trace((I + S M[e,e])^-1 S M2[e,e]). Maximise the reduction.
      M <- chol2inv(chol(L + 1 / n + ridge * diag(n)))
      M2 <- M %*% M
      Ik <- diag(k)
      gain <- vapply(cand, function(cc) {
        e <- V[cc, ]
        sum(diag(solve(Ik + S %*% M[e, e], S %*% M2[e, e])))
      }, numeric(1))
      cand <- cand[gain >= max(gain) - 1e-9 * abs(max(gain))]
    }

    sel <- cand[sample.int(length(cand), 1)]
    e <- V[sel, ]
    blocks[i, ] <- e
    used[e] <- used[e] + 1
    L[e, e] <- L[e, e] + S
  }

  if (relaxed) {
    warning("availability could not be met exactly for all packages; ",
            "some items exceed their quota")
  }

  # ---- column (position) balance: sequential greedy, random ties ------------
  perms <- .getPerms(seq_len(k))
  pos <- matrix(0, n, k)
  f <- function(x) x * log(pmax(x, 1))            # convex cost, favours even counts
  colidx <- rep(seq_len(k), each = nrow(perms))

  for (i in seq_len(N)) {
    P <- matrix(blocks[i, ][perms], ncol = k)
    idx <- cbind(c(P), colidx)
    cost <- rowSums(matrix(f(pos[idx] + 1) - f(pos[idx]), ncol = k))
    w <- which(cost <= min(cost) + 1e-12)
    b <- P[w[sample.int(length(w), 1)], ]
    blocks[i, ] <- b
    pos[cbind(b, seq_len(k))] <- pos[cbind(b, seq_len(k))] + 1
  }

  if (polish) {                                   # optional, whole-design only
    r <- rowSums(pos)
    repeat {
      changed <- FALSE
      for (i in seq_len(N)) {
        b <- blocks[i, ]
        pos[cbind(b, seq_len(k))] <- pos[cbind(b, seq_len(k))] - 1
        P <- matrix(b[perms], ncol = k)
        idx <- cbind(c(P), colidx)
        tr <- r[idx[, 1]] / k
        cost <- rowSums(matrix((pos[idx] + 1 - tr)^2 - (pos[idx] - tr)^2, ncol = k))
        cur <- which(apply(P, 1, function(p) all(p == b)))
        nb <- if (cost[cur] > min(cost) + 1e-12) P[which.min(cost), ] else b
        if (!all(nb == b)) changed <- TRUE
        blocks[i, ] <- nb
        pos[cbind(nb, seq_len(k))] <- pos[cbind(nb, seq_len(k))] + 1
      }
      if (!changed) break
    }
  }

  out <- as.data.frame(matrix(itemnames[blocks], N, k,
                              dimnames = list(seq_len(N),
                                              paste0("item_", LETTERS[seq_len(k)]))),
                       stringsAsFactors = FALSE)
  class(out) <- union("CM_df", class(out))
  out
}

#' @rdname randomize
#' @export
randomise <- function(...){

  randomize(...)

}

# Round to integers while preserving the sum
.smart.round <- function(x) {
  x[is.na(x)] <- 0
  y <- floor(x)
  extra <- round(sum(x)) - sum(y)
  if (extra > 0) {
    idx <- utils::tail(order(x - y), extra)
    y[idx] <- y[idx] + 1
  }
  y
}

# All permutations of a vector, one per row
.getPerms <- function(x) {
  if (length(x) == 1) return(matrix(x, 1, 1))
  res <- NULL
  for (i in seq_along(x)) res <- rbind(res, cbind(x[i], .getPerms(x[-i])))
  res
}
