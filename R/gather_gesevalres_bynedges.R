
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

ges_nedges_meanres <- foreach(file = ges_files, .combine = rbind) %do% {
  loadsucces <- tryCatch({load(paste(path, file, sep = ""));TRUE}, 
                         warning = function(e) {FALSE},
                         error = function(e) {FALSE})
    thissetting <- as.list(thisres[1, ges_settingvnames])
    p <- thissetting$ps

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
      theseres <- summarize_all(group_by(thisres[, c(ges_metricvnames, "nedges_cat")], 
                                         nedges_cat), 
                                mean)
      
      as.data.frame(c(thissetting, theseres))

}

ges_nedges_meanres$shd <- NULL


ges_nedges_meanres$n <- factor(ges_nedges_meanres$ns, 
                           levels = sort(unique(ges_nedges_meanres$ns)))
ges_nedges_meanres$ns <- NULL

ges_nedges_meanres2 <- melt(ges_nedges_meanres, id.vars = c("n", "ps",
                                                            "lambdas", 
                                                            "nedges_cat"))

ges_nedges_meanres2$metric <- substring(ges_nedges_meanres2$variable, 5)
ges_nedges_meanres2$type <- substr(ges_nedges_meanres2$variable, 1,3)

