library(keras)
source("./R/misc.R")

############### Subsampling #############
ns <- c(50, 100, 500, 1000)

for (n in ns) {
  cormat <- read.table(paste("application_metropolit/application_cormat_n", n, ".txt", sep = ""),
                       header = TRUE)
  varnames <- names(cormat)
  cormat <- array(as.matrix(cormat), dim = c(1, 10, 10))

  model <- load_model_hdf5(paste("models/nn_N6_p10_n", shortnum(n), "_beta0.1-1_val10K.h5", sep = ""))
  preds <- predict(model, cormat)[ , , , drop = TRUE]

  dimnames(preds) <- list(varnames, varnames)

  save(list = c("preds"), 
     file = paste("application_metropolit/preds_n", shortnum(n), ".rda", sep = ""))
}






####### Full dataset #############

cormat <- read.table("application_metropolit/application_cormat_n2928.txt",
                     header = TRUE)
varnames <- names(cormat)
cormat <- array(as.matrix(cormat), dim = c(1, 10, 10))

model1K <- load_model_hdf5("models/nn_N6_p10_n1K_beta0.1-1_val10K.h5")
model5K <- load_model_hdf5("models/nn_N6_p10_n5K_beta0.1-1_val10K.h5")

preds1K <- predict(model1K, cormat)[ , , , drop = TRUE]
preds5K <- predict(model5K, cormat)[ , , , drop = TRUE]

dimnames(preds1K) <- dimnames(preds5K) <- list(varnames, varnames)

save(list = c("preds1K", "preds5K"), 
     file = "application_metropolit/preds_n2928.rda")