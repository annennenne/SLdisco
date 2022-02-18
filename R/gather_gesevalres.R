
path <- "eval/"

files <- list.files(path) 

ges_files <- files[substr(files, 6, 8) == "ges"]

#USE TETRAD COMPUTATIONS
ges_files <- ges_files[grepl("*TETRAD*", ges_files)]

ges_metricvnames <-  c("adj_NPV", "adj_F1",
                       "dir_precision", "dir_G1", "shd",
                       "nedges_true", "nedges_est",
                       "average_degree_true", "average_degree_est")
ges_settingvnames <- c("ns", "ps", "lambdas")


ges_meanres <- foreach(file = ges_files, .combine = rbind) %do% {
  load(paste(path, file, sep = "")) 
  thissetting <- as.list(thisres[1, ges_settingvnames])
  thismeanres <- lapply(thisres[, ges_metricvnames], mean)
  as.data.frame(c(thissetting, thismeanres))
}

ges_meanres$shd <- NULL

ges_meanres2 <- melt(ges_meanres, id.vars = c("ns", "ps", "lambdas"))

ges_meanres2$metric <- substring(ges_meanres2$variable, 5)
ges_meanres2$type <- substr(ges_meanres2$variable, 1,3)
ges_meanres2$p <- ges_meanres2$ps

# 
# 
# 
# 
# 
# 
