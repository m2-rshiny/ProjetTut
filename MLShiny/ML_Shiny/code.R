library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)
library(broom)
library(glmnet)
library(caret)
library(parallel)
library(foreach)
library(doParallel)
library(Boruta)
library(reshape)
library(plotly)
library(kableExtra)

options(OutDec = ".")
theme_set(theme_minimal())
set.seed(7323)
# sqAnoDf <- read_tsv(file = "www/anonymized-sq-dataset.tsv") %>% select(-BARCODE)
# sqAnoDf <- read_tsv(file = "ML_Shiny/www/anonymized-sq-dataset.tsv")
# sqAnoDf <- read.csv2("www/spam.csv", header = TRUE)

#-------------------Choix des radios---------------------#
algo_choices <- c("XGBoost", "Tree", "RF", "RfRanger", "Knn", "Lasso")


### set-up ------------------------------------------------------------------
nCores <- detectCores()
pSubSample <- 0.01
indResp <- 1

#------------------Paramètres algo---------------------------------#
grid <- list(
  "knn" = expand.grid(k = seq(1, 21, by = 2)),
  "rf" = data.frame(mtry = seq(1, 50, by = 3)),
  "glmnet" = expand.grid(alpha = 1, lambda = seq(0, 0.2, by = 0.01))
)

#------Sélectionne le meilleur Accuracy pour chaque modèle---------#
get_best_result <- function(caret_fit) {
  best <- which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result <- caret_fit$results[best, ]
  rownames(best_result) <- NULL
  round(best_result, 2)
}
