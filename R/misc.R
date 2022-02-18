source("R/probmat2amat.R")

shortnum <- function(n) {
  if (n < 1000) {
    return(n)
  } 
  if (n < 10^6) {
    return(paste(n %/% 1000, "K", sep = ""))
  } else {
    return(paste(n %/% 10^6, "M", sep = ""))
  }
}


comboframe <- function(...) {
  args <- list(...)
  nx <- length(args)
  xnames <- names(args)
  ncats <- sapply(args, length)
  
  out <- data.frame(i = 1:prod(ncats))
  for (i in 1:nx) {
    eachrep <- prod(ncats[0:(i-1)])
    allrep <- prod(ncats[ifelse(i == nx, 0, (i+1):nx)])
    out <- cbind(out, data.frame(rep(rep(args[[i]], each = eachrep), allrep)))
  }
  out <- out[-1]
  names(out) <- xnames
  if (any(duplicated(out))) stop("comboframe() has a bug!")
  out
}


as.graphNEL <- function(amat) {
  as(t(amat), "graphNEL")
}

is_pdag <- function(amat) {
  pcalg::isValidGraph(amat, "pdag")
}

is_cpdag <- function(amat) {
  pcalg::isValidGraph(amat, "cpdag")
}

graph2amat <- function(graph) {
  t(as(graph, "matrix"))
}


#apply function for matrix array with dim = (nrep, p, p) where
#p is nrow = ncol for the matrices. Outputs same format. 
matarray_apply <- function(matarray, ...) {
  thisdim <- dim(matarray)
  p <- thisdim[2]
  res <- apply(matarray, ...)
  aperm(array(res, dim = c(p, p, thisdim[1])), c(3, 1, 2))
}



pad4d_matrixarray <- function(matrixarray, target_slice_shape, value = 0) {
  #  browser()
  thesedim <- dim(matrixarray)
  nrep <- thesedim[1]
  nrow <- thesedim[2]
  ncol <- thesedim[3]
  out <- array(value, c(nrep, target_slice_shape))
  
  for(i in 1:nrep) {
    out[i, 1:nrow, 1:ncol, ] <- matrixarray[i, , , , drop = TRUE]
  }
  
  out
}


maxnedges <- function(p) {
  sum(1:(p-1))
}


which2indicator <- function(x, p) {
  out <- rep(0, p)
  out[x] <- 1
  out
}

essgraph2amat <- function(essgraph, p = length(essgraph$field(".nodes"))) {
  inlist <- essgraph$field(".in.edges")
  out <- t(sapply(inlist, which2indicator, p = p))
  colnames(out) <- rownames(out)
  out
}
