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
longnames <- function(x) {
  x[x == "adj"] <- "adjacencies"
  x[x == "dir"] <- "orientations"
  x
}
plabels <- function(x) {
  paste("p =", x)
}

adjmethlabels <- function(x) {
  x[x == "cutoff"] <- "Cutoff"
  x[x == "greedy_backward"] <- "Greedy backwards"
  x[x == "greedy_forward"] <- "Greedy forward"
  x[x == "bpco"] <- "Backwards PC orientation"
  x
}

typemetlabels <- function(x) {
  x[x == "adj:F1"] <- "Adjacency F1"
  x[x == "adj:NPV"] <- "Adjacency NPV"
  x[x == "dir:G1"] <- "Orientation G1" 
  x[x == "dir:precision"] <- "Orientation precision" 
  x
}

nedgescatlabels <- function(x) {
  paste("No. edges: ", x)
}


source("R/misc.R")

folder <- "/home/ahp/Dropbox/bscopiesDB/NNdisco/article/figures/"
setwd("/home/ahp/Dropbox/bscopiesDB/NNdisco")

source("article/R/gather_nnevalres.R")
source("article/R/gather_gesevalres.R")
source("article/R/gather_pcevalres.R")
source("article/R/gather_pcevalres_bynedges.R")
source("article/R/gather_nnevalres_bynedges.R")
source("article/R/gather_gesevalres_bynedges.R")


usepostproc <- c("cutoff", "bpco")

nn_meanres <- nn_meanres[nn_meanres$adjmethods %in% usepostproc,]
nn_meanres2 <- nn_meanres2[nn_meanres2$adjmethods %in% usepostproc,]

nn_nedges_meanres <- nn_nedges_meanres[nn_nedges_meanres$adjmethods %in% usepostproc,]
nn_nedges_meanres2 <- nn_nedges_meanres2[nn_nedges_meanres2$adjmethods %in% usepostproc,]


usethresvals <- as.character(c(0.01, 0.05, seq(0.1, 0.5, 0.1)))

#############################################################################
# Estimated number of edges vs. actual number of edges - ges comparison
#############################################################################

true_no_edges <- summarise(group_by(nn_meanres, p, adjmethods), 
                               nedges = mean(nedges_true),
                        .groups = "keep")

ggplot(nn_meanres2[nn_meanres2$variable %in% c("nedges_est"),], 
       aes(x = factor(thresvals), y = value,
           col = factor(n),
           group = factor(variable):factor(n))) +
  geom_hline(data = true_no_edges,
            aes(yintercept = nedges),
            col = "black",
            size = 1) +
  geom_hline(data = ges_meanres2[ges_meanres2$variable %in% c("nedges_est") &
                                 ges_meanres2$lambdas == "BIC",],
             aes(yintercept = value, col = factor(ns)),
             alpha = 1) +
  geom_point() +
  geom_line() +
  colscale + theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  facet_grid(p ~ adjmethods, scales = "free_y",
             labeller = labeller(p = plabels, adjmethods = adjmethlabels)) + 
  ylab("") + xlab("Threshold") 
ggsave(paste(folder,
             "nedges", ".png", sep  = ""), 
       width = 8, height = 5)


#############################################################################  
# Number of edges true vs. estimated - by quartiles 
# comp. with GES (BIC)
#############################################################################

plots <- list()
i <- 1
  for (p in c(5, 10, 20)) {
    for (method in c("SLdisco", "GES", "PC")) {
      
    minval <- min(nn_nedges_meanres[nn_nedges_meanres$p == p &
                                  nn_nedges_meanres$adjmethods %in% "bpco", 
                                  c("nedges_true", "nedges_est")])
    if (p == 5) usethres <- "0.4"
    if (p == 10) usethres <- "0.4" 
    if (p == 20) usethres <- "0.3"
    if (method == "SLdisco") {
      usedata <- nn_nedges_meanres[nn_nedges_meanres$p == p &
                                     nn_nedges_meanres$adjmethods %in% "bpco" &
                                     nn_nedges_meanres$thresvals %in% usethres, ]
    } else if (method == "GES") {
      usedata <- ges_nedges_meanres[ges_nedges_meanres$p == p &
                                       ges_nedges_meanres$lambdas == "BIC", ]
    } else if (method == "PC") {
      usedata <- pc_nedges_meanres[pc_nedges_meanres$p == p &
                                    pc_nedges_meanres$alphas == 0.1, ]
    }
    usedata$nedges_true
    
    outplot <- ggplot(usedata,
             aes(x = nedges_true,  y = nedges_est, col = n)) + 
        geom_hline(yintercept = sum(1:(p-1)), lty = "dotted") + 
        geom_vline(xintercept = sum(1:(p-1)), lty = "dotted") + 
        geom_point() +
        geom_line() + 
        geom_abline(slope = 1, intercept = 0) +
        expand_limits(x = floor(minval), y = floor(minval)) +
        xlab("No. edges (true)") +
        ylab("No. edges (estimated)") +
      theme(axis.text.y = element_text(vjust = -0.7,
                                       margin = margin(l = 20, r = -50)))
    
    plots[[i]] <- outplot
    i <- i + 1
  }
}


ggmatrix(plots, nrow = 3, ncol = 3,
                xAxisLabels = c("SLdisco", "GES", "PC"),
                yAxisLabels = paste("p =", c(5, 10, 20)),
         legend = 1) +
  theme_bw() + colscale

ggsave(paste(folder, "nedgebytruth.png", sep = ""), width = 8, height = 7)

#############################################################################
#OVERVIEW PLOTS, MEAN RESULTS, F1 and NPV
#############################################################################


options(scipen = 0)
lambda <- "BIC"


for (tmcombo in list(c("adj", "F1"), c("adj", "NPV"), c("dir", "precision"),
                     c("dir", "G1"))) {
  type <- tmcombo[1]
  usemetric <- tmcombo[2]
  
  ggplot(nn_meanres2[nn_meanres2$type == type &
                     nn_meanres2$thresvals %in% usethresvals &
                       nn_meanres2$metric == usemetric, ], 
         aes(x = factor(thresvals), y = value,
             col = factor(n), 
             group = factor(n))) + 
    geom_hline(data = ges_meanres2[ges_meanres2$lambdas == lambda &
                                     ges_meanres2$type == type &
                                     ges_meanres2$metric == usemetric, ], 
               aes(yintercept = value, col = factor(ns)),
               alpha = 0.5) +
    geom_point() +
    geom_line() +
    facet_grid(p ~ adjmethods,
              labeller = labeller(p = plabels, adjmethods = adjmethlabels)) + 
    colscale + 
    theme_bw() + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
    ylab("") + xlab("Threshold")
  ggsave(paste(folder, type,
               "res-", usemetric, ".png", sep = ""), width = 8, height = 5)
}


#############################################################################
# Results stratified by number of edges  - ges-comparison
#############################################################################

use_adjmethods <- c("bpco")
lambda <- "BIC"
p <- 10
    ggplot(nn_nedges_meanres2[nn_nedges_meanres2$p == p &
                                nn_nedges_meanres2$type %in% c("adj") & 
                                nn_nedges_meanres2$adjmethods %in% use_adjmethods &
                                nn_nedges_meanres2$metric %in% c("F1", "NPV") & 
                                nn_nedges_meanres2$thres %in% usethresvals, ], 
           aes(x = factor(thresvals), y = value,
               col = n, 
               group = factor(n):factor(adjmethods))) + 
      geom_hline(data = ges_nedges_meanres2[ges_nedges_meanres2$lambdas == lambda &
                                              ges_nedges_meanres2$ps == p & 
                                              ges_nedges_meanres2$type %in% c("adj") &
                                              ges_nedges_meanres2$metric  %in% c("F1", "NPV"), ], 
                 aes(yintercept = value, col = n),
                 alpha = 0.5) +
      geom_point() +
      geom_line() +
      facet_grid(nedges_cat ~metric,
                 labeller = labeller(nedges_cat = nedgescatlabels)) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw() + colscale + 
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
      xlab("Threshold") + ylab("") 
ggsave(paste(folder, "f1npv-bynedges-p10.png", sep  = ""), width = 8, height = 8)

#############################################################################
# Results stratified by number of edges  - ges-comparison - all mets
#############################################################################

use_adjmethods <- c("bpco")
lambda <- "BIC"
p <- 10
ggplot(nn_nedges_meanres2[nn_nedges_meanres2$p == p &
                            nn_nedges_meanres2$adjmethods %in% use_adjmethods &
                            nn_nedges_meanres2$variable %in% c("adj_F1", "adj_NPV",
                                                               "dir_precision", "dir_G1") &
                            nn_nedges_meanres2$thres %in% usethresvals, ], 
       aes(x = factor(thresvals), y = value,
           col = n, 
           group = factor(n):factor(adjmethods))) + 
  geom_hline(data = ges_nedges_meanres2[ges_nedges_meanres2$lambdas == lambda &
                                          ges_nedges_meanres2$ps == p & 
                                          ges_nedges_meanres2$variable %in% 
                                          c("adj_F1", "adj_NPV","dir_precision", "dir_G1"), ], 
             aes(yintercept = value, col = n),
             alpha = 0.5) +
  geom_point() +
  geom_line() +
  facet_grid(nedges_cat ~ factor(type):factor(metric),
             labeller = labeller(nedges_cat = nedgescatlabels,
                                 `factor(type):factor(metric)` = typemetlabels)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() + colscale + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Threshold") + ylab("") 
ggsave(paste(folder, "allmetrics-bynedges-p10.png", sep  = ""), width = 8, height = 8)



    
##############################################################################
### PC - GES COMPARISON ######################################################
##############################################################################

pvals <- c(5, 10, 20)
options(scipen = 0)
ggplot(pc_meanres2[pc_meanres2$type %in% c("adj") &
                     pc_meanres2$metric %in% c("NPV", "F1") &
                     pc_meanres2$ps %in% pvals,], aes(x = factor(alphas),
                                                      y = value,
                                                      col = factor(ns),
                                                      group = factor(ns):factor(ps))) + 
  geom_hline(data = ges_meanres2[ges_meanres2$lambdas == lambda &
                                   ges_meanres2$type %in% c("adj") &
                                   ges_meanres2$lambdas == "BIC" & 
                                   ges_meanres2$metric %in% c("NPV", "F1") &
                                   ges_meanres2$ps %in% pvals, ], 
             aes(yintercept = value, col = factor(ns)),
             alpha = 0.5) +
  geom_point() +
  geom_line() +
  facet_grid(p ~ metric, labeller = labeller(p = plabels)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() + colscale +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Significance level") + ylab("") 
ggsave(paste(folder,
             "pc-ges-comparison-adjmetrics", ".png", sep  = ""), width = 8, height = 5)

ggplot(pc_meanres2[pc_meanres2$type %in% c("dir") &
                     pc_meanres2$metric %in% c("G1", "precision") &
                     pc_meanres2$ps %in% pvals,], aes(x = factor(alphas),
                                                      y = value,
                                                      col = factor(ns),
                                                      group = factor(ns):factor(ps))) + 
  geom_hline(data = ges_meanres2[ges_meanres2$lambdas == lambda &
                                   ges_meanres2$type %in% c("dir") &
                                   ges_meanres2$lambdas == "BIC" & 
                                   ges_meanres2$metric %in% c("G1", "precision") &
                                   ges_meanres2$ps %in% pvals, ], 
             aes(yintercept = value, col = factor(ns)),
             alpha = 0.5) +
  geom_point() +
  geom_line() +
  facet_grid(p ~ metric, labeller = labeller(p = plabels)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() + colscale +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Significance level") + ylab("") 
ggsave(paste(folder,
             "pc-ges-comparison-dirmetrics", ".png", sep  = ""), width = 8, height = 5)



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
             alpha = 0.5) +
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


##############################################################################
### All metrics for SLdisco w cutoff #########################################
##############################################################################

options(scipen = 0)
lambda <- "BIC"
use_adjmethods <- "cutoff"

    ggplot(nn_meanres2[((nn_meanres2$type == "adj" & nn_meanres2$metric == "NPV") |
                        (nn_meanres2$type == "adj" & nn_meanres2$metric == "F1") |
                        (nn_meanres2$type == "dir" & nn_meanres2$metric == "precision") | 
                        (nn_meanres2$type == "dir" & nn_meanres2$metric == "G1")) &
                       nn_meanres2$thresvals %in% usethresvals &
                       nn_meanres2$adjmethods %in% use_adjmethods, ], 
           aes(x = factor(thresvals), y = value,
               col = factor(n), 
               group = factor(n))) + 
      geom_hline(data = ges_meanres2[((ges_meanres2$type == "adj" & ges_meanres2$metric == "NPV") |
                                      (ges_meanres2$type == "adj" & ges_meanres2$metric == "F1") |
                                      (ges_meanres2$type == "dir" & ges_meanres2$metric == "precision") | 
                                      (ges_meanres2$type == "dir" & ges_meanres2$metric == "G1")) &
                                     ges_meanres2$lambdas == lambda, ], 
                 aes(yintercept = value, col = factor(ns)),
                 alpha = 0.5) +
      geom_point() +
      geom_line() +
     facet_grid(p ~ as.factor(type):as.factor(metric),
                 labeller = labeller(p = plabels,
                                     `as.factor(type):as.factor(metric)` = typemetlabels)) + 
      colscale + 
      theme_bw() + 
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
      ylab("") + xlab(expression(tau))
    ggsave(paste("/home/ahp/Dropbox/bscopiesDB/Talks/SDSS2022/",
                 "res-cutoff-allmetrics", ".png", sep = ""), 
           width = 7.5, height = 3
)
    
    
##############################################################################
### All metrics for ges with both scores #####################################
##############################################################################
    
options(scipen = 0)
ggplot(ges_meanres2[((ges_meanres2$type == "adj" & ges_meanres2$metric == "NPV") |
                       (ges_meanres2$type == "adj" & ges_meanres2$metric == "F1") |
                       (ges_meanres2$type == "dir" & ges_meanres2$metric == "precision") | 
                       (ges_meanres2$type == "dir" & ges_meanres2$metric == "G1")), ],
           aes(x = factor(lambdas), y = value,
               col = factor(ns), group = factor(ns))) + 
      geom_point() +
      geom_line() +
      facet_grid(p ~ as.factor(type):as.factor(metric),
                 labeller = labeller(p = plabels,
                                     `as.factor(type):as.factor(metric)` = typemetlabels)) + 
      colscale + 
      theme_bw() + 
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
      ylab("") + xlab("Score")
ggsave(paste(folder,
                 "ges-BIC-BIC2-comparison-allmet", ".png", sep  = ""), 
       width = 8, height = 7
)
    
    
