library(keras) 
source("./R/misc.R")

ntrains <-  c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(5, 10, 20) # c(2, 5, 10, 20)
archs <- "N6" #c("N1", "N2", "N3")
ntests <- c(50, 100, 500, 1000, 5000, 10000, 50000)
combos <- comboframe(ntrains = ntrains, ps = ps, archs = archs,
                     ntests = ntests) 
combos <- combos[combos$ntrains == combos$ntests, ]

b <- shortnum(5000)
datasubfix <- "_beta0.1-1"
modelsubfix <- "_val10K_earlystop"
allnns <- list.files("models")
outputsubfix <- "_earlystop" #""

for (i in 1:nrow(combos)) {
  ntrain <- combos[i, "ntrains"]
  p <- combos[i, "ps"]
  arch <- combos[i, "archs"]
  ntest <- combos[i, "ntests"]
  
  mname <- paste("nn_", arch, "_p", p, "_n", shortnum(ntrain), datasubfix, modelsubfix, ".h5", sep = "")
  dataname <- paste("data_p", p, "_n", shortnum(ntest), "_b", b, 
                    datasubfix, ".rda", sep = "")
  outname <- paste("nnpred_", arch, "_p", p, "_ntrain", shortnum(ntrain),
                   "_ntest", shortnum(ntest), outputsubfix, sep = "")
  
  if (mname %in% allnns & !(outname %in% list.files("results"))) {
    load(paste("./data/", dataname, sep = ""))
    model <- load_model_hdf5(paste("./models/", mname, sep = ""))
    
    if (arch != "N3") {
    preds <- predict(model, xdata)
    } else {
     preds <- predict(model, pad4d_matrixarray(xdata, c(32, 32, 3)))
    }
    
    save(list = c("preds", "ydata"), file = paste("./results/", outname, ".rda", sep = ""))
  }
}
