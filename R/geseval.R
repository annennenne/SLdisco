#GESEVAL BASED ON USING JOES TETRAD FGES RESULTS

source("./R/misc.R")
source("./R/evalfunctions.R")

#parallization
library(doParallel)
registerDoParallel(60) 

#Combo matrix n/p/alpha
ns <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(5, 10, 20)
lambdas <- c("BIC", "BIC2")
combos <- comboframe(ns = ns, ps = ps,
                     lambdas = lambdas)

usemetrics_adj <- c("F1", "NPV")
usemetrics_dir <- c("G1", "precision")
usemetrics_other <- c("shd", "average_degree_true", "average_degree_est",
                      "nedges_est", "nedges_true")
metricvarnames <-  c(paste("adj_", usemetrics_adj, sep = ""), paste("dir_", usemetrics_dir, sep = ""),
                     usemetrics_other)
n_combos <- nrow(combos)
restypename <- "ges_TETRAD_"
datasubfix <- "_beta0.1-1"


foreach(i=1:n_combos) %dopar% {
  p <- combos[i, "ps"]
  n <- combos[i, "ns"]
  lambda <- combos[i, "lambdas"]
  
  if (lambda == "BIC") penalty <- 1
  if (lambda == "BIC2") penalty <- 2
  
  #resname <- paste("sldisco_adjout_b5K_penalty", penalty, "/",
  #                 "adjmatsout_p", p, "_n", shortnum(n), "_b5K.txt", sep = "")
  resname <- paste("cpdag_out_penalty", penalty, "/cpdagmat_out_p", p, "_n", shortnum(n), "_b5K.txt", sep = "")
  
  outname <- paste(restypename, lambda, "_p", p, "_n", shortnum(n), sep = "")
  outfilename <-  paste("eval/eval_", outname, "_beta0.1-1", ".rda", sep = "")
  
  #load true CPDAGs
  datafilename <- paste("data_", "p", p, "_n", shortnum(n), 
                    "_b5K", datasubfix,
                    ".rda", sep = "")
  load(paste("data/", datafilename, sep = "")) #loads ydata, xdata
  xdata <- NULL
  
    
  #load estimated CPDAGs
  tetradres <- read.table(paste("ges_TETRAD/", resname, sep = ""), header = TRUE)
  preds <- array(as.numeric(as.matrix(tetradres)), dim = c(5000, p, p))
   
  #evaluate
  thisres <- evaluate(preds, ydata, metrics = list(adj = usemetrics_adj, 
                                                    dir = usemetrics_dir, 
                                                    other = usemetrics_other))
   
  #add info and save   
  thisres$ns <- n
  thisres$ps <- p
  thisres$lambdas <- lambda
  save(thisres, file = outfilename)
}


