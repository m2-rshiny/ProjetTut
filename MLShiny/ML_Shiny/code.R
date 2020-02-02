library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)
library(broom)
library(reshape)
library(glmnet)
library(caret)
library(parallel)
library(foreach)
library(doParallel)
library(Boruta)
library(plotly)
library(kableExtra)

options(OutDec=".")
theme_set(theme_minimal())
mycolors <- c("orange","#0072B2")

sqAnoDf <- read_tsv(file = "www/anonymized-sq-dataset.tsv") %>% select(-BARCODE)
#sqAnoDf <- read_tsv(file = "ML_Shiny/www/anonymized-sq-dataset.tsv")
#sqAnoDf <- read.csv2("www/spam.csv", header = TRUE)
#sqAnoDf <- read.csv2("ML_Shiny/www/spam.csv", header = TRUE)
#sqAnoDf <- read.csv("www/adult.csv", header = TRUE)
#sqAnoDf <- read.csv("ML_Shiny/www/adult.csv", header = TRUE)
#-------------------Choix des radios---------------------#
algo <- c("RF", "Knn", "Lasso")
var_response <- names(sqAnoDf)


### set-up ------------------------------------------------------------------
nCores <- detectCores()
pSubSample <- 0.01
indResp <- 1


#------------------Paramètres algo---------------------------------#
grid <- list(
  'knn' = expand.grid(k = seq(1, 21, by = 2)),
  'rf' = data.frame(mtry = seq(1, 50, by = 3)),
  'glmnet' = expand.grid(alpha = 1, lambda = seq(0,0.2,by = 0.01))
  #'svmLinear' = data.frame(C=c(0.01,0.1,1,10))
  #'svmPoly'= expand.grid(degree=1:3,scale=c(0.01,0.1),C=c(0.25,0.5,1))
)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  round(best_result,2)
}



