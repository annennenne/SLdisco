

#path <- "/home/ahp/bshome/NNdisco/eval/"
path <- "eval/"

files <- list.files(path) 

pc_files <- files[substr(files, 6, 7) == "pc"]


metricvnames <- c("adj_precision",  "adj_recall", "adj_specificity", "adj_NPV", "adj_F1",
                  "adj_G1", "dir_G1",
                  "dir_precision", "dir_recall", "dir_specificity", "dir_NPV", "dir_F1","shd",
                  "nedges_true", "nedges_est",
                  "average_degree_true", "average_degree_est")
settingvnames <- c("ns", "ps", "alphas")

pc_meanres <- foreach(file = pc_files, .combine = rbind) %do% {
  load(paste(path, file, sep = "")) 
 # if (all(metricvnames %in% names(thisres))) {
    names(thisres)[names(thisres) == "alpas"] <- "alphas"
    thissetting <- as.list(thisres[1, settingvnames])
    thismeanres <- lapply(thisres[, metricvnames], mean)
    as.data.frame(c(thissetting, thismeanres))
 # }
}

pc_meanres$shd <- NULL
pc_meanres <- pc_meanres[pc_meanres$ps %in% c(5, 10, 20),]

pc_meanres2 <- melt(pc_meanres, id.vars = c("ns", "ps", "alphas"))

pc_meanres2$metric <- substring(pc_meanres2$variable, 5)
pc_meanres2$type <- substr(pc_meanres2$variable, 1,3)
pc_meanres2$p <- pc_meanres2$ps


#p <- 5
#ggplot(pc_meanres2[pc_meanres2$ps == p,], aes(x = factor(alphas),
#                        y = value,
#                        col = factor(ns),
#                        group = factor(ns))) + 
#  geom_vline(xintercept = "0.1", lty = "dashed", col = "darkgrey") +
#  geom_hline(yintercept = 0.9, lty = "dashed", col = "darkgrey") +
#  geom_point() +
#  geom_line() +
#  facet_grid(type ~ metric) +
#  scale_y_continuous(limits = c(0,1)) +
#  ggtitle(paste("PC, p = ", p, sep = ""))


