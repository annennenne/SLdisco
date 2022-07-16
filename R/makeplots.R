library(ggplot2)
library(GGally)
library(reshape2)
library(foreach)
library(dplyr)

options(scipen = 0)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
nvals <- c(50, 100, 500, 1000, 5000, 10000, 50000)
colscale <-   scale_color_manual("n", breaks = rev(nvals),
                                 values = cbPalette)

colscale2 <-   scale_color_manual("Method", breaks = c("SLdisco-cutoff", "SLdisco-BPCO", "GES", "PC"),
                                  values = cbPalette)
longnames <- function(x) {
  x[x == "adj"] <- "adjacencies"
  x[x == "dir"] <- "orientations"
  x
}
plabels <- function(x) {
  paste("p =", x)
}

adjmethlabels <- function(x) {
  x[x == "cutoff"] <- "SLdisco-cutoff"#"Cutoff"
  x[x == "greedy_backward"] <- "Greedy backwards"
  x[x == "greedy_forward"] <- "Greedy forward"
  x[x == "bpco"] <- "SLdisco-BPCO" #"Backwards PC orientation"
  x
}

typemetlabels <- function(x) {
  x[x == "adj:F1"] <- "Adjacency F1" #"F1 \n (adjacencies)"
  x[x == "adj:NPV"] <- "Adjacency NPV" #"NPV \n (adjacencies)"
  x[x == "dir:G1"] <- "Orientation G1" #"G1 \n (orientations)"
  x[x == "dir:precision"] <- "Orientation precision" #"Precision \n (orientations)"
  x
}


metlabels <- function(x) {
  x[x == "F1"] <- "F1 (informativeness)" #"F1 \n (adjacencies)"
  x[x == "NPV"] <- "NPV (conservativeness)" #"NPV \n (adjacencies)"
  x[x == "G1"] <- "G1 (informativeness)" #"G1 \n (orientations)"
  x[x == "precision"] <- "Precision (conservativeness)" #"Precision \n (orientations)"
  x
}

metlabels2 <- function(x) {
  x[x == "F1"] <- "Adjacency F1" #"F1 \n (adjacencies)"
  x[x == "NPV"] <- "Adjacency NPV" #"NPV \n (adjacencies)"
  x[x == "G1"] <- "Orientation G1" #"G1 \n (orientations)"
  x[x == "precision"] <- "Orientation precision" #"Precision \n (orientations)"
  x
}

metlabels3 <- function(x) {
  x[x == "es_est"] <- "Number of edges"
}


nedgescatlabels <- function(x) {
  paste("No. edges: ", x)
}

colalph <- 0.8

source("R/misc.R")

folder <- "/home/ahp/Dropbox/bscopiesDB/SLdisco/article/figures/"
setwd("/home/ahp/Dropbox/bscopiesDB/SLdisco")

source("gather_nnevalres.R")
source("gather_gesevalres.R")
source("gather_pcevalres.R")
source("gather_nnevalres_bynedges.R")

usepostproc <- c("cutoff", "bpco")
usemetrics <- c("adj_NPV", "adj_F1", "dir_precision", "dir_G1", "nedges_est")
usethresvals <- as.character(seq(0.1, 0.6, 0.1))

nn_meanres <- nn_meanres[nn_meanres$adjmethods %in% usepostproc,]
nn_meanres2 <- nn_meanres2[nn_meanres2$adjmethods %in% usepostproc,]

nn_nedges_meanres <- nn_nedges_meanres[nn_nedges_meanres$adjmethods %in% usepostproc,]
nn_nedges_meanres2 <- nn_nedges_meanres2[nn_nedges_meanres2$adjmethods %in% usepostproc,]

nn_meanres3 <- nn_meanres2
ges_meanres3 <- ges_meanres2
pc_meanres3 <- pc_meanres2

pc_meanres3$Method <- "PC"
ges_meanres3$Method <- "GES"
nn_meanres3$Method <- "SLdisco-BPCO"
nn_meanres3$Method[nn_meanres3$adjmethods == "cutoff"] <- "SLdisco-cutoff"

pc_meanres3 <- pc_meanres3[pc_meanres3$alphas == 0.1,]
pc_meanres3 <- pc_meanres3[pc_meanres3$variable %in% usemetrics,]
pc_meanres3 <- pc_meanres3[, c("ns", "value", "metric", "type", "p", "Method")]
names(pc_meanres3)[1] <- "n"

ges_meanres3 <- ges_meanres3[ges_meanres3$lambdas == "BIC",]
ges_meanres3 <- ges_meanres3[ges_meanres3$variable %in% usemetrics,]
ges_meanres3 <- ges_meanres3[, c("ns", "value", "metric", "type", "p", "Method")]
names(ges_meanres3)[1] <- "n"

nn_meanres3 <- nn_meanres3[(nn_meanres3$p == 5 & nn_meanres3$thresvals == "0.4") | 
                           (nn_meanres3$p == 10 & nn_meanres3$thresvals == "0.4") |  
                           (nn_meanres3$p == 20 & nn_meanres3$thresvals == "0.3"), ] 
nn_meanres3 <- nn_meanres3[nn_meanres3$variable %in% usemetrics,]
nn_meanres3 <- nn_meanres3[nn_meanres3$adjmethods %in% usepostproc,]
nn_meanres3 <- nn_meanres3[, c("n", "value", "metric", "type", "p", "Method")]


allres <- rbind(nn_meanres3, ges_meanres3, pc_meanres3)
allres$ncat <- sapply(allres$n, shortnum)
allres$ncat <- factor(allres$ncat, levels = c("50", "100", "500", "1K", "5K", "10K", "50K"))

#############################################################################
# Estimated number of edges vs. actual number of edges 
#############################################################################
true_no_edges <- summarise(group_by(nn_meanres, p, adjmethods), 
                           nedges = mean(nedges_true),
                           .groups = "keep")

ggplot(nn_meanres2[nn_meanres2$variable %in% c("nedges_est") & 
                     nn_meanres2$thresvals %in% usethresvals,], 
       aes(x = factor(thresvals), y = value,
           col = factor(n),
           group = factor(variable):factor(n))) +
  geom_hline(data = true_no_edges,
             aes(yintercept = nedges),
             col = "black") +
  geom_point() +
  geom_line() +
  colscale + theme_bw() +
  facet_grid(p ~ adjmethods, scales = "free_y",
             labeller = labeller(p = plabels, adjmethods = adjmethlabels)) + 
  ylab("") + xlab(expression(tau)) 
ggsave(paste(folder,
             "SLdisco-nedges", ".png", sep  = ""), 
       width = 8, height = 4)


##############################################################################
### Nedges - compare SLdisco, GES, PC ########################################
##############################################################################

ggplot(allres[allres$metric== "es_est",], aes(x = factor(n), y = value, 
                                              col = Method, group = Method)) +
  geom_hline(data = true_no_edges,
             aes(yintercept = nedges),
             col = "black") +
  geom_point() +
  geom_line() + 
  facet_grid(p ~ metric, labeller = labeller(p = plabels, metric = metlabels3),
             scales = "free_y") +
  colscale2 + theme_bw() +
  xlab("n") +
  ylab("") 
ggsave(paste(folder,
             "all-nedges", ".png", sep  = ""), 
       width = 6, height = 3)


##############################################################################
### SLdisco, PC, GES adj metrics #############################################
##############################################################################


ggplot(allres[allres$type == "adj",], aes(x = factor(n), y = value, col = Method, group = Method)) +
  geom_point() + 
  facet_grid(p ~ metric, labeller = labeller(p = plabels, metric = metlabels2)) +
  geom_line() +
  colscale2 + theme_bw() +
  xlab("n") +
  ylab("") 

ggsave(paste(folder,
             "adjmetrics", ".png", sep  = ""), 
       width = 8, height = 4)

##############################################################################
### SLdisco, PC, GES dir metrics #############################################
##############################################################################


ggplot(allres[allres$type == "dir",], aes(x = factor(n), y = value, col = Method, group = Method)) +
  geom_point() + 
  facet_grid(p ~ metric, labeller = labeller(p = plabels, metric = metlabels2)) +
  geom_line() +
  xlab("n") +
  ylab("") +
  theme_bw() +
  colscale2 + theme_bw() 

ggsave(paste(folder,
             "dirmetrics", ".png", sep  = ""), 
       width = 8, height = 4)


##############################################################################
### PC - GES COMPARISON - allmetrics #########################################
##############################################################################

options(scipen = 0)
ggplot(pc_meanres2[(pc_meanres2$type %in% c("adj") & pc_meanres2$metric %in% c("NPV", "F1")) | 
                     (pc_meanres2$type %in% c("dir") & pc_meanres2$metric %in% c("precision", "G1")),], 
       aes(x = factor(alphas), y = value, col = factor(ns), group = factor(ns):factor(ps))) + 
  geom_hline(data = ges_meanres2[ges_meanres2$lambdas == "BIC" &
                                   ((ges_meanres2$type %in% c("adj") & ges_meanres2$metric %in% c("NPV", "F1")) |
                                      (ges_meanres2$type %in% c("dir") & 
                                         ges_meanres2$metric %in% c("precision", "G1"))), ], 
             aes(yintercept = value, col = factor(ns)),
             alpha = colalph) +
  geom_point() +
  geom_line() +
  facet_grid(p ~ factor(type):factor(metric),
             labeller = labeller(p = plabels,
                                 `factor(type):factor(metric)` = typemetlabels)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() + colscale +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Significance level") + ylab("") 
ggsave(paste(folder,
             "pc-ges-comparison-allmetrics", ".png", sep  = ""), width = 8, height = 5)

