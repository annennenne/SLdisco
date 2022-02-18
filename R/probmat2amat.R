probmat2amat <- function(probmat, threshold, method = "cutoff",
                         keep_vnames = TRUE, graph_criterion = "pdag",
                         deletesym = FALSE) {
  if (keep_vnames) vnames <- rownames(probmat)
 
  p <- nrow(probmat)
  
  if (method == "cutoff") {
    out <- matrix(as.numeric(probmat >= threshold), p, p)
  } 
  
  
  if (method == "bpco") {
    probmat[probmat < threshold] <- 0
    ord <- order(probmat, decreasing = TRUE)
    ord <- ord[probmat[ord] > 0]
    n_nonzero <- length(ord)
    i <- 1
    
    new <- probmat2amat(probmat, threshold, "cutoff")
    ord <- rev(ord)
    new_is_valid <- is_cpdag(new)
    
    badmat <- t(matrix(1:(p^2), p, p))
    
    while(!new_is_valid & i <= n_nonzero) {
      deleteind <- ord[i]
      new[deleteind] <- 0
      if (deletesym) new[which(badmat == deleteind)] <- 0
      
      new_is_valid <- is_cpdag(new) 
      if (!new_is_valid) {
        trynew <- pcalg:::correspondingCpdag(new)
        if (is_cpdag(trynew)) {
          new <- trynew
          new_is_valid <- TRUE
        }
      }
      i <- i + 1
    }
    if (new_is_valid) out <- new
  }
  
  if (!pcalg::isValidGraph(out, graph_criterion) & method != "cutoff") warning("result is not a valid graph!")
  
  
  if (keep_vnames) dimnames(out) <- list(vnames, vnames)
  out
} 







