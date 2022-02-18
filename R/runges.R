rm(list = ls())

library(pcalg) 
library(R.utils)
source("./R/misc.R")
source("./R/gausCorScore.R")


#paralization
library(doParallel)
registerDoParallel(50)


#Combo matrix n/p/alpha
ns <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(2, 5, 10, 20)
lambdas <- c("BIC", "BIC2")
combos <- comboframe(ns = ns, ps = ps,
                     lambdas = lambdas)

combos <- combos[(combos$ns == 10000 & combos$ps == 20 & combos$lambdas == "BIC") | 
                 (combos$ns == 50000 & combos$ps == 20 & combos$lambdas == "BIC") | 
                 (combos$ns == 50000 & combos$ps == 20 & combos$lambdas == "BIC2"), ]


#Settings for this run
k <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
p <- combos[k, "ps"]
n <- combos[k, "ns"]
lambda <- combos[k, "lambdas"]


#Load data
btest <- 5000
datasubfix <- "_beta0.1-1"
testdataname <- paste("data_p", p, "_n", shortnum(n), "_b", shortnum(btest), 
                     datasubfix, ".rda", sep = "")
load(paste("./data/", testdataname, sep = "")) 
#comptimes <- numeric(btest)
#computed <- logical(btest)

#Run GES
preds <- array(NA, dim = c(btest, p, p))
if (lambda == "BIC") {
  lval <- log(n)/2
} 
if (lambda == "BIC2") { #CHECK IF THIS IS CORRECT
  lval <- log(n)
}
if (lambda == "BIC40") {
  lval <- log(n) * 20
}

timeoutlimit <- 60*60*2 #2 hours timeout

predlist <- foreach(i=1:btest) %dopar% {
  starttime <- Sys.time()
  
  thiscor <- xdata[i, , , 1]
  rownames(thiscor) <- colnames(thiscor) <- letters[1:p]
  tryCatch(withTimeout({
    res <<- essgraph2amat(ges(gausCorScore(thiscor, n = n, p = p,
                                                   lambda = lval,
                                                   use.cpp = FALSE))$essgraph, p = p)
    elapsetime <- Sys.time() - starttime
    cat(paste("i = ", i, " is done. Elapsed comp. time: ", round(as.numeric(elapsetime),4), 
                " ", attr(elapsetime, "unit"),  "\n", sep = ""))
  }, timeout = timeoutlimit), TimeoutException = function(ex) {
    res <<- NULL
    elapsetime <<- NA
    message(paste("dataset", i, "reached timeout."))
  })
 
  list(res = res, comptime = elapsetime)
}

#Save results
mname <- paste("predlist_", lambda, "_p", p, "_n", shortnum(n), sep = "")
save(list = c("predlist", "ydata", "btest"), file =  paste("./ges/ges_", mname, ".rda", sep = ""))
