library(pcalg)
source("./R/misc.R")


#initialize from bash/other loop
slurm <- TRUE
bs <- c(1000, 5000, 10^6)
ns <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(5, 10, 20)
combos <- data.frame(bs = rep(bs, each = length(ns)*length(ps)),
                     ns = rep(ns, length(bs)*length(ps)),
                     ps = rep(rep(ps, each = length(ns)), length(bs)))
k <- ifelse(slurm, as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")), k)

#settings
b <- combos[k, "bs"]
p <- combos[k, "ps"]
n <- combos[k, "ns"]
verbose <- TRUE
makeCoefMat <- FALSE
makeCausalOrder <- FALSE
standardizeData <- FALSE
regpars <- c(0.1, 1) #note: data files w/o this info were for regpar = (0.1, 2)

b; p; n;


stdindicator <- ""
coefmatindicator <- ""
causalorderindicator <- ""
if (standardizeData) stdindicator <- "_std"
if (makeCoefMat) coefmatindicator <- "_coefmat"
if (makeCausalOrder) causalorderindicator <- "_causord"
filename <- paste("data_", "p", p, "_n", shortnum(n), 
                  "_b", shortnum(b),
                  stdindicator,
                  coefmatindicator,
                  causalorderindicator,
                  "_beta", paste(regpars, collapse = "-"),
                  ".rda", sep = "")

if (file.exists(paste("data/", filename, sep = ""))) {
  paste("Skipping - data file already exists.")
  stop()
}


#helper functions
as.cpdag.adjm <- function(dag.adjm) {
  t(as(dag2cpdag(as(t(dag.adjm), "graphNEL")), "matrix"))
}



#data initialization
xdata <- array(NA, dim = c(b, p, p, 1))
ydata <- array(NA, dim = c(b, p, p))
if (makeCoefMat) ycoefmatdata <- array(NA, dim = c(b, p, p))
if (makeCausalOrder) orderdata <- array(NA, dim = c(b, p))

#simulation
set.seed(NULL)
useseed <- .Random.seed

for (j in 1:b) {
  #sim DAG adjacency matrix
  sparsity <- runif(1, min = 0, max = 0.8)
  adjm <- matrix(1, p, p)
  adjm[upper.tri(adjm, diag = TRUE)] <- 0
  rownames(adjm) <- colnames(adjm) <- letters[1:p]
  adjm_lowtri <- which(lower.tri(adjm))
  adjm[sample(adjm_lowtri, round(sparsity * length(adjm_lowtri)))] <- 0
  
  #convert to cpdag adjacency matrix, use vector version 
  thisLab <- as.cpdag.adjm(adjm)
  
  if (makeCoefMat) thisCoefMat <- matrix(0, p, p, dimnames = list(letters[1:p], letters[1:p]))
  
  #sim multiv. Gaus data from adjmat
  
  #note: need 0s for matrix multiplication in loop below to work, NA does not work
  data <- matrix(0, n, p, dimnames = list(NULL, letters[1:p]))
  
  residualsds <- runif(p, min = 0.5, max = 2)
  
  #first col (always exogenous)
  data[, 1] <- rnorm(n, sd = residualsds[1])
  
  
  for (i in 2:p) {
    #regression parameters
    pars <- runif(p, min = regpars[1], max = regpars[2]) * sample(c(-1, 1), p, replace = TRUE, prob = c(0.4, 0.6))
    usepars <- pars * adjm[i, ]
    
    if (makeCoefMat) thisCoefMat[i, ] <- usepars
    
    thisVar <- data %*% usepars + rnorm(n, sd = residualsds[i])
    
    if (standardizeData) thisVar <- (thisVar - mean(thisVar)) / sd(thisVar)
    
    data[, i] <- thisVar
  }
  
  #permute order of variables
  perm <- sample.int(p, replace = FALSE)
  data <- data[, perm]
  thisLab <- thisLab[perm, perm]
  if (makeCoefMat) thisCoefMat <- thisCoefMat[perm, perm]
  
  thisX <- WGCNA::cor(data)
  diag(thisX) <- 1 #rounding issues may result on diag cor = 0.9999... 
  
  #remove data, free memory
  #data <- NULL
  #gc()
  
  #store output
  xdata[j, , , 1] <- thisX
  ydata[j, , ] <- thisLab
  if (makeCoefMat) ycoefmatdata[j, , ] <- thisCoefMat
  if (makeCausalOrder) orderdata[j, ] <- perm
  
  #write out status 
  if (verbose && (j %% 1000 == 0)) cat(paste("j = ", j, "done. \n"))
}



savefiles <- c("useseed", "xdata", "ydata")
if (makeCoefMat) savefiles <- c(savefiles, "ycoefmatdata")
if (makeCausalOrder) savefiles <- c(savefiles, "orderdata")
save(list = savefiles, 
     file = paste("data/", filename, sep = ""))
