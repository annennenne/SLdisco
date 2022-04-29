library(causalDisco)
library(pcalg)
source("R/misc.R")
source("R/evalfunctions.R")

folder <- "/home/ahp/Dropbox/bscopiesDB/NNdisco/article/figures/"

varlabs <- list(`birth_weight` = "Weight",
                `birth_length` = "Length",
                `birth_mother_married` = "Mother married",
                `birth_father_socialclasslow` = "Father low social class",
                `child_iq` = "Intelligence score",
                `child_creativity` = "Creativity score",
                `child_father_socialclasslow` = "Father low social class",
                `child_likeschool` = "Positive towards school",
                `child_mother_smoke` = "Maternal smoking",
                `child_father_smoke` = "Paternal smoking",
                `child_bullied` = "Bullied",
                `youth_cognition` = "Intelligence score",
                `youth_height` = "Height",
                `youth_bmi` = "BMI",
                `adult_no_children` = "Number of children",
                `adult_cohabit` = "Cohabitation",
                `adult_see_family` = "See family weekly",
                `adult_see_friends` = "See friends weekly",
                `adult_contact_family` = "In contact with family weekly",
                `adult_contact_friends` = "In contact with friends weekly",
                `adult_own_housing` = "Own housing",
                `adult_own_car` = "Own car",
                `adult_own_summerhouse` = "Own summerhouse",
                `adult_smoke_now` = "Smoke now",
                `adult_smoke_years` = "Total years of smoking",
                `adult_bmi` = "BMI",
                `adult_alco_bingeepisodes` = "Alcohol binging frequency",
                `adult_allteethleft` = "Still have all teeth",
                `adult_education_undergrad` = "Undergraduate education",
                `adult_employed` = "Employment status",
                `adult_income` = "Disposable income",
                `adult_depression_any` = "Depression",
                `elderly_depression_any` = "Depression")


ord <- c("birth", "child", "youth", "adult")


########### Full metropolit ##############################################################################

postproc <- "bpco"
graph_crit <- "cpdag"
thres <- 0.4
alpha <- 0.1 
 
n <- 2928 
  
 #load sldisco results
  nload <- n
  if (nload != 2928) nload <- shortnum(nload)
  load(paste("application_metropolit/preds_n", nload, ".rda", sep = ""))
  if (n == 2928) {
    preds <- preds5K
  }
  
  sldiscores <- probmat2amat(preds, threshold = thres, method = postproc, graph_criterion = graph_crit)
  png(paste(folder, "application-sldisco-n", n, ".png", sep = ""), 
      width = 600, height = 600, pointsize = 14)
  plot(tamat(sldiscores, order = ord),  varLabels = varlabs)
  dev.off()
  
  #load cormat
  thiscormat <- read.table(paste("application_metropolit/application_cormat_n", n, ".txt", sep = ""), 
                           header = TRUE)
  thiscormat <- as.matrix(thiscormat)
  rownames(thiscormat) <- colnames(thiscormat)
  
  #do pc 
  pcres <- as(pc(list(C = thiscormat, n = n),  labels = rownames(thiscormat),
                 indepTest = gaussCItest, alpha = alpha), "amat")
  png(paste(folder, "application-pc-n", n, ".png", sep = ""), 
      width = 600, height = 600, pointsize = 14)
  plot(tamat(pcres, order = ord),  varLabels = varlabs)
  dev.off()
  
  #do ges
  gesres <- essgraph2amat(ges(gausCorScore(thiscormat, n = n, p = 10))$essgraph, p = 10)
  png(paste(folder, "application-ges-n", n, ".png", sep = ""), 
      width = 600, height = 600, pointsize = 14)
  plot(tamat(gesres, order = ord),  varLabels = varlabs)
  dev.off()


####### Subsampling: SLdisco ##############################################################################

ns <- c(50, 100, 500, 1000, 2928)

source("R/evalfunctions.R")
source("R/misc.R")

dec <- 2
comp <- 5

usemodel <- preds5K
thres <- 0.4 
graph_crit <- "cpdag"
postproc <- "bpco"

sldiscoret <- matrix(NA, length(ns), 10^2)

for (i in 1:length(ns)) {
  n <- ns[i]
  if (n != 2928) n <- shortnum(n)
 load(paste("application_metropolit/preds_n", n, ".rda", sep = ""))
 if (n == 2928) preds <- usemodel
 res <- probmat2amat(preds, threshold = thres, method = postproc, graph_criterion = graph_crit)
 sldiscoret[i, ] <- as.numeric(res) 
}

 
round(NPV(adj_confusion(matrix(sldiscoret[1, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))),dec)
round(NPV(adj_confusion(matrix(sldiscoret[2, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))),dec)
round(NPV(adj_confusion(matrix(sldiscoret[3, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(sldiscoret[4, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)

round(F1(adj_confusion(matrix(sldiscoret[1, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(F1(adj_confusion(matrix(sldiscoret[2, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(F1(adj_confusion(matrix(sldiscoret[3, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(F1(adj_confusion(matrix(sldiscoret[4, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)

round(precision(dir_confusion(matrix(sldiscoret[1, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(precision(dir_confusion(matrix(sldiscoret[2, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(precision(dir_confusion(matrix(sldiscoret[3, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(precision(dir_confusion(matrix(sldiscoret[4, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)

round(G1(dir_confusion(matrix(sldiscoret[1, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(G1(dir_confusion(matrix(sldiscoret[2, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(G1(dir_confusion(matrix(sldiscoret[3, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)
round(G1(dir_confusion(matrix(sldiscoret[4, ], 10, 10), 
	matrix(sldiscoret[comp ,], 10, 10))), dec)


####### Subsampliong: PC ##################################################################################
pcret <- matrix(NA, length(ns), 10^2)

for (i in 1:length(ns)) {
  n <- ns[i]
  thiscormat <- read.table(paste("application_metropolit/application_cormat_n", n, ".txt", sep = ""), 
                           header = TRUE)
  thiscormat <- as.matrix(thiscormat)
  rownames(thiscormat) <- colnames(thiscormat)
  res <- as(pc(list(C = thiscormat, n = n),  labels = rownames(thiscormat),
               indepTest = gaussCItest, alpha = alpha), "amat")
  pcret[i, ] <- as.numeric(res) #as.numeric(res + t(res) > 0)
}


round(NPV(adj_confusion(matrix(pcret[1, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(pcret[2, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(pcret[3, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(pcret[4, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)

round(F1(adj_confusion(matrix(pcret[1, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(F1(adj_confusion(matrix(pcret[2, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(F1(adj_confusion(matrix(pcret[3, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(F1(adj_confusion(matrix(pcret[4, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)

round(precision(dir_confusion(matrix(pcret[1, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(precision(dir_confusion(matrix(pcret[2, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(precision(dir_confusion(matrix(pcret[3, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(precision(dir_confusion(matrix(pcret[4, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)

round(G1(dir_confusion(matrix(pcret[1, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(G1(dir_confusion(matrix(pcret[2, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(G1(dir_confusion(matrix(pcret[3, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)
round(G1(dir_confusion(matrix(pcret[4, ], 10, 10), matrix(pcret[5,], 10, 10))), dec)


####### Subsampling: GES ##################################################################################
gesret <- matrix(NA, length(ns), 10^2)

for (i in 1:length(ns)) {
  n <- ns[i]
  thiscormat <- read.table(paste("application_metropolit/application_cormat_n", n, ".txt", sep = ""), 
                           header = TRUE)
  thiscormat <- as.matrix(thiscormat)
  rownames(thiscormat) <- colnames(thiscormat)
  res <-essgraph2amat(ges(gausCorScore(thiscormat, n = n, p = 10))$essgraph, p = 10)
  gesret[i, ] <- as.numeric(res) #as.numeric(res + t(res) > 0)
}

round(NPV(adj_confusion(matrix(gesret[1, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(gesret[2, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(gesret[3, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(NPV(adj_confusion(matrix(gesret[4, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)

round(F1(adj_confusion(matrix(gesret[1, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(F1(adj_confusion(matrix(gesret[2, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(F1(adj_confusion(matrix(gesret[3, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(F1(adj_confusion(matrix(gesret[4, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)

round(precision(dir_confusion(matrix(gesret[1, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(precision(dir_confusion(matrix(gesret[2, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(precision(dir_confusion(matrix(gesret[3, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(precision(dir_confusion(matrix(gesret[4, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)

round(G1(dir_confusion(matrix(gesret[1, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(G1(dir_confusion(matrix(gesret[2, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(G1(dir_confusion(matrix(gesret[3, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)
round(G1(dir_confusion(matrix(gesret[4, ], 10, 10), matrix(gesret[5,], 10, 10))), dec)



