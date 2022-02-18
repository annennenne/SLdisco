source("./R/misc.R")
source("./R/evalfunctions.R")
library(graph)
library(foreach)

#paralization
library(doParallel)
registerDoParallel(20)


#Combo matrix n/p/arch/thres
ntrains <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(5, 10, 20)
archs <- "N6" 
thresvals <- c(0.01, 0.05, seq(0.1, 0.7, 0.1))
adjmethods <- c("cutoff", "bpco"")
combos <- comboframe(ntrains = ntrains, ntests = ntrains, ps = ps, archs = archs, thresvals = thresvals,
                    adjmethods = adjmethods)
combos <- combos[combos$ntrains == combos$ntests, ]

usemetrics_adj <- c("F1", "NPV")
usemetrics_dir <- c("G1", "precision")
usemetrics_other <- c("shd", "average_degree_true", "average_degree_est",
                      "nedges_est", "nedges_true")
metricvarnames <-  c(paste("adj_", usemetrics_adj, sep = ""), paste("dir_", usemetrics_dir, sep = ""),
                     usemetrics_other)
ressubfix <- "" 
n_combos <- nrow(combos)

foreach(i=1:n_combos, .combine = rbind) %dopar% {
  arch <- combos[i, "archs"]
  p <- combos[i, "ps"]
  ntrain <- combos[i, "ntrains"]
  ntest <- combos[i, "ntests"]
  thresval <- combos[i, "thresvals"]
  adjmethod <- combos[i, "adjmethods"]
  subfix <- "_pdagcrit"
  
  
  resname <- paste(arch, "_p", p, "_ntrain", shortnum(ntrain),
                   "_ntest", shortnum(ntest), ressubfix, sep = "")
  loadsucces <- tryCatch({load(paste("./results/nnpred_", resname, ".rda", sep = ""));TRUE}, 
                         warning = function(e) {FALSE},
                         error = function(e) {FALSE})
 
  outname <-  paste("eval/eval_", resname, "_beta0.1-1", 
                    "_thres", thresval, "_", adjmethod, subfix,
                    ".rda", sep = "")
  
#  if (loadsucces & !(file.exists(outname))) {
    preds <- matarray_apply(preds, MARGIN = 1, FUN = probmat2amat, threshold = thresval, 
                            method = adjmethod, graph_criterion = "pdag")
    thisres <- evaluate(preds, ydata, metrics = list(adj = usemetrics_adj, 
                                                     dir = usemetrics_dir, 
                                                     other = usemetrics_other))

    #out <- colMeans(thisres)
    thisres$ntrains <- ntrain
    thisres$ntests <- ntest
    thisres$p <- p
    thisres$archs <- arch
    thisres$thresvals <- thresval
    thisres$adjmethods <- adjmethod 
    
    save(thisres, file = outname)
#  }
}

