rm(list = ls())

library(keras) 
source("./R/misc.R")

#Combo matrix n/p/architecture
ns <- c(1000, 5000, 50, 100, 500, 10000, 50000)
ps <- c(5, 10, 20) 
archs <- c("N6")
combos <- comboframe(archs = archs,
                     ps = ps,
                     ns = ns)

#Settings for this run
skipifdone <- FALSE
nepochs <- 500 
batchsize <- 2^8 
k <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
p <- combos[k, "ps"]
n <- combos[k, "ns"]
arch <- combos[k, "archs"]
datasubfix <- "_beta0.1-1"
modelsubfix <- "_val10K_earlystop"

p; n; arch

mname <- paste(arch, "_p", p, "_n", shortnum(n), datasubfix, modelsubfix, sep = "")
if (skipifdone) {
  if (file.exists(paste("./models/nn_", mname, ".h5", sep = ""))) {
    print("File exists - skipping")
    stop()
  }
}


btrain <- shortnum(10^6)
bval <- shortnum(10000)

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

#Define NN architecture (and possibly prep data)
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
                         verbose = 2,
                         callbacks = callback_early_stopping(patience = 50,
                                                             restore_best_weights = TRUE))


#save NN and fitting history


save(list = c("history", "nepochs", "batchsize"), file = paste("./history/history_", mname, ".rda", sep = ""))
save_model_hdf5(model, paste("./models/nn_", mname, ".h5", sep = ""))
