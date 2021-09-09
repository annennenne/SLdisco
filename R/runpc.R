rm(list = ls())

library(pcalg) 
source("./R/misc.R")

#Combo matrix n/p/alpha
ns <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(2, 5, 10, 20)
alphas <- c(10^(-8), 10^(-4), 10^(-3), 0.01, 0.05, 0.1, 0.2, 0.5, 0.8)
combos <- data.frame(alphas = rep(alphas, each = length(ns)*length(ps)),
                     ns = rep(ns, length(alphas)*length(ps)),
                     ps = rep(rep(ps, each = length(ns)), length(alphas)))


#Settings for this run
k <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
p <- combos[k, "ps"]
n <- combos[k, "ns"]
alpha <- combos[k, "alphas"]


#Load data
btest <- 5000
datasubfix <- "_causord"
testdataname <- paste("data_p", p, "_n", shortnum(n), "_b", shortnum(btest), 
                     datasubfix, ".rda", sep = "")
load(paste("./data/", testdataname, sep = "")) #xdata ydata orderdata
orderdata <- NULL #drop orderdata for now
realy <- ydata #rename ydata
ydata <- NULL


#Run PC
pcy <- array(NA, dim = c(btest, p, p))

for (i in 1:btest) {
  pcy[i, , ] <- as(pc(list(C = xdata[i, , , 1], n = n),  labels = letters[1:p],
                        indepTest = gaussCItest, alpha = alpha), "amat")
}

#Save results
mname <- paste(alpha, "_p", p, "_n", shortnum(n), sep = "")
save(list = c("pcy", "realy", "btest"), file = paste("./pc/pc_", mname, ".rda", sep = ""))

