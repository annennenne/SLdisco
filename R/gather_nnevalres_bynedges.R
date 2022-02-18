
#path <- "/home/ahp/bshome/NNdisco/eval/"
path <- "eval/"

files <- list.files(path) 

nnfiles <- files[substr(files, 6, 6) == "N"]

metricvnames <- c("adj_NPV", "adj_F1",
                  "dir_precision", "dir_G1", "shd",
                  "nedges_true", "nedges_est",
                  "average_degree_true", "average_degree_est")
nn_settingvnames <- c("ntrains", "p", "archs", "thresvals", "adjmethods")

usearch <- "N6_"
nnfiles <- nnfiles[substr(nnfiles, 6,8) == usearch]

#USE PDAG CRITERION FOR POST-PROCESSING
nnfiles <- nnfiles[grepl("*_pdagcrit*", nnfiles)]

nn_nedges_meanres <- foreach(file = nnfiles, .combine = rbind) %do% {
  load(paste(path, file, sep = ""))
  thissetting <- as.list(thisres[1, nn_settingvnames])
  p <- thissetting$p
  
  if (p %in% c(5, 10)) {
        thisres$nedges_cat <- cut(thisres$nedges_true,
                                  breaks = round(quantile(thisres$nedges_true,
                                                    c(0, 0.25, 0.5, 0.75, 1)),0),
                                  include.lowest = TRUE)
  } else if (p == 20) {
        thisres$nedges_cat <- cut(thisres$nedges_true,
                                  breaks = c(38, 76, 115, 152, 190),
                                  include.lowest = TRUE) #set manually to avoid different 
                                  			   #values due to rounding differences
  }
        
  theseres <- summarize_all(group_by(thisres[, c(metricvnames, "nedges_cat")], nedges_cat), 
                            mean)
        
  as.data.frame(c(thissetting, theseres))
}

#########

nn_nedges_meanres$shd <- NULL
nn_nedges_meanres$n <- factor(nn_nedges_meanres$ntrains, 
                            levels = sort(unique(nn_nedges_meanres$ntrains)))
nn_nedges_meanres$ntrains <- NULL

nn_nedges_meanres2 <- melt(nn_nedges_meanres, id.vars = c(setdiff(nn_settingvnames, "ntrains"),
                                                          "n",  "nedges_cat"))

nn_nedges_meanres2$metric <- substring(nn_nedges_meanres2$variable, 5)
nn_nedges_meanres2$type <- substr(nn_nedges_meanres2$variable, 1,3)
