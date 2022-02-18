#library(foreach)
#path <- "/home/ahp/bshome/NNdisco/eval/"
path <- "eval/"

files <- list.files(path) 

nnfiles <- files[substr(files, 6, 6) == "N"]

metricvnames <- c("adj_NPV", "adj_F1",
                  "dir_precision", "dir_G1", "shd",
                  "nedges_true", "nedges_est",
                 "average_degree_true", "average_degree_est")
nn_settingvnames <- c("ntrains", "p", "archs", "thresvals", "adjmethods")

usearch <- "N6"
nnfiles <- nnfiles[substr(nnfiles, 6,7) == usearch]

#USE PDAG CRITERION FOR POST-PROCESSING
nnfiles <- nnfiles[grepl("*_pdagcrit*", nnfiles)]

nn_meanres <- foreach(file = nnfiles, .combine = rbind) %do% {
  load(paste(path, file, sep = ""))
  thissetting <- as.list(thisres[1, nn_settingvnames])
  thismeanres <- lapply(thisres[, metricvnames], mean)
  as.data.frame(c(thissetting, thismeanres))
}


############

nn_meanres$shd <- NULL
nn_meanres$n <- nn_meanres$ntrains
nn_meanres$ntrains <- NULL

nn_meanres2 <- melt(nn_meanres, id.vars = c(setdiff(nn_settingvnames, "ntrains"),
                                            "n"))

nn_meanres2$metric <- substring(nn_meanres2$variable, 5)
nn_meanres2$type <- substr(nn_meanres2$variable, 1,3)


##########
#loadsucces <- tryCatch({load(paste(path, file, sep = ""));TRUE}, 
#                       warning = function(e) {FALSE},
#                       error = function(e) {FALSE})
#
#if (loadsucces) {
#}
#