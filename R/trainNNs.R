rm(list = ls())

library(keras) 
source("./R/misc.R")

#Combo matrix n/p/architecture
ns <- c(50, 100, 500, 1000, 5000, 10000, 50000)
ps <- c(2, 5, 10, 20)
archs <- c("N1", "N2")
combos <- data.frame(archs = rep(archs, each = length(ns)*length(ps)),
                     ns = rep(ns, length(archs)*length(ps)),
                     ps = rep(rep(ps, each = length(ns)), length(archs)))

#Settings for this run
nepochs <- 1000
batchsize <- 100
k <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
p <- combos[k, "ps"]
n <- combos[k, "ns"]
arch <- combos[k, "archs"]


######################
# DELETE THIS
p <- 2
n <- 10000
arch <- "N2"
nepochs <- 2
batchsize <- 100
######################

btrain <- shortnum(10^6)
bval <- shortnum(1000)
datasubfix <- "_causord"

traindataname <- paste("data_p", p, "_n", shortnum(n), "_b", btrain, 
                       datasubfix, ".rda", sep = "")
valdataname <- paste("data_p", p, "_n", shortnum(n), "_b", bval, 
                     datasubfix, ".rda", sep = "")


#Load training data
load(paste("./data/", traindataname, sep = ""))
trainxdata <- xdata
trainydata <- ydata
orderdata <- NULL #drop orderdata for now

#Load validation data
load(paste("./data/", valdataname, sep = ""))
valxdata <- xdata
valydata <- ydata
orderdata <- NULL #drop orderdata for now

#Define NN architecture
source("./R/NNs.R")

#compile NN
model %>% 
  compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metric = list("binary_accuracy", "Precision", "Recall"))

#train NN
#note: provide val data in order to be able to see metrics along the way + check for 
#overfitting
history <- model %>% fit(x = trainxdata, 
                         y = trainydata,
                         epochs = nepochs,
                         batch_size = batchsize,
                         validation_data = list(valxdata, valydata),
                         view_metrics = FALSE,
                         verbose = 2)


#save NN and fitting history
mname <- paste(arch, "_p", p, "_n", shortnum(n), sep = "")

save(list = c("history", "nepochs", "batchsize"), file = paste("./history/history_", mname, ".rda", sep = ""))
save_model_hdf5(model, paste("./models/nn_", mname, ".h5", sep = ""))
