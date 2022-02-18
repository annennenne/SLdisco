source("./R/misc.R")
source("./R/evalfunctions.R")

#paralization
library(doParallel)
registerDoParallel(20)

#Combo matrix n/p/alpha
ns <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(2, 5, 10, 20)
alphas <- c(10^(-8), 10^(-4), 10^(-3), 0.01, 0.05, 0.1, 0.2, 0.5, 0.8)
combos <- comboframe(alphas = alphas, ns = ns, ps = ps)

usemetrics_adj_dir <- c("precision", "recall", "specificity", "NPV", "F1", "G1") 
usemetrics_other <- c("shd", "average_degree_true", "average_degree_est",
                      "nedges_est", "nedges_true")
metricvarnames <-  c(paste("adj_", usemetrics_adj_dir, sep = ""), paste("dir_", usemetrics_adj_dir, sep = ""),
                     usemetrics_other)

n_combos <- nrow(combos)

foreach(i=1:n_combos, .combine = rbind) %dopar% {
  p <- combos[i, "ps"]
  n <- combos[i, "ns"]
  alpha <- combos[i, "alphas"]
  
  resname <- paste("pc_", alpha, "_p", p, "_n", shortnum(n), sep = "")
  outfilename <-  paste("eval/eval_", resname, "_beta0.1-1", ".rda", sep = "")
  
  if (TRUE) {#!file.exists(outfilename)) {
    loadsucces <- tryCatch({load(paste("./pc/", resname, ".rda", sep = ""));TRUE}, 
                           warning = function(e) {FALSE},
                           error = function(e) {FALSE})
    
    if (loadsucces) {
      thresval <- combos[i, "thresvals"]
      adjmethod <- combos[i, "adjmethods"]
      
      thisres <- evaluate(preds, ydata, metrics = list(adj = usemetrics_adj_dir, 
                                                       dir = usemetrics_adj_dir, 
                                                       other = usemetrics_other))
      
      #out <- colMeans(thisres)
      thisres$ns <- n
      thisres$ps <- p
      thisres$alphas <- alpha
      
      save(thisres, file = outfilename)
    }
  }
}

