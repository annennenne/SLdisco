gausCorScore <- function(cormat, n, p, 
                          lambda = NULL, ...)  {
  if (is.null(lambda)) lambda <- log(n)/2
  
  outscore <- new("GaussL0penObsScore", matrix(1,1,1),
                         lambda = lambda, intercept = FALSE, ...)
  
  #drop entries not needed 
  outscore$pp.dat$data <- NULL
  outscore$pp.dat$non.int <- NULL
  outscore$pp.dat$target.index <- NULL
  
  #fill in entries with custom calculations
  outscore$pp.dat$vertex.count <- p
  outscore$pp.dat$data.count <- rep(n, p)
  outscore$pp.dat$total.data.count <- n
  outscore$pp.dat$scatter.index <- rep(1, p)
  outscore$pp.dat$scatter<- list(cbind(rbind(cormat, rep(0, p)), c(rep(0, p),1)) * (n - 1))
  outscore$.nodes <- rownames(cormat)
  
  outscore
}


