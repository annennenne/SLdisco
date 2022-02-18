
path <- "eval/"

files <- list.files(path) 

pc_files <- files[substr(files, 6, 7) == "pc"]

pc_metricvnames <- c("adj_precision",  "adj_recall", "adj_specificity", "adj_NPV", "adj_F1",
                     "adj_G1", "dir_G1",
                  "dir_precision", "dir_recall", "dir_specificity", "dir_NPV", "dir_F1","shd",
                  "nedges_true", "nedges_est",
                  "average_degree_true", "average_degree_est")
pc_settingvnames <- c("ns", "ps", "alphas")

pc_nedges_meanres <- foreach(file = pc_files, .combine = rbind) %do% {
  loadsucces <- tryCatch({load(paste(path, file, sep = ""));TRUE}, 
                         warning = function(e) {FALSE},
                         error = function(e) {FALSE})
  if (loadsucces) {
    names(thisres)[names(thisres) == "alpas"] <- "alphas"
    thissetting <- as.list(thisres[1, pc_settingvnames])
    p <- thissetting$ps
    
    if (p != 2) {
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
      theseres <- summarize_all(group_by(thisres[, c(pc_metricvnames, "nedges_cat")], 
                                         nedges_cat), 
                                mean)
      
      as.data.frame(c(thissetting, theseres))
      }
  }
}

pc_nedges_meanres$shd <- NULL


pc_nedges_meanres$n <- factor(pc_nedges_meanres$ns, 
                           levels = sort(unique(pc_nedges_meanres$ns)))
pc_nedges_meanres$ns <- NULL

pc_nedges_meanres2 <- melt(pc_nedges_meanres, id.vars = c("n", "ps", "alphas",
                                                    "nedges_cat"))

pc_nedges_meanres2$metric <- substring(pc_nedges_meanres2$variable, 5)
pc_nedges_meanres2$type <- substr(pc_nedges_meanres2$variable, 1,3)

