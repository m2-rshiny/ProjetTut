library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)
library(broom)
library(rpart)
library(rpart.plot)
library(randomForest)
library(glmnet)
library(caret)
library(Hmisc)
library(parallel)
library(foreach)
library(doParallel)
library(Boruta)
library(GGally)
library(ggthemes)
library(plotly)
library(kableExtra)
library(formattable)

options(OutDec=".")
theme_set(theme_minimal())

#-------------------Choix des radios---------------------#
algo <- c("Tree", "Knn", "RF", "Lasso", "Ridge", "SVM")

sqAnoDf <- read_tsv(file = "www/anonymized-sq-dataset.tsv")

#-------------------Train / Test---------------------#
n_train <- sample(1:nrow(sqAnoDf), round(.8 * nrow(sqAnoDf)), replace = FALSE)

sqAnoDf <- sqAnoDf[n_train,]
Xtest <- sqAnoDf[-n_train,]

### set-up ------------------------------------------------------------------
useSeed <- TRUE
nSeed <- 123456789
nCores <- detectCores()
pSubSample <- 0.01
indResp <- 1

sumList <- list()
plotList <- list()
fitList <- list()


### list predictive features to be included in training process -------------
### features are organized by rows of 5

#Variable à expliquer
responsesOfInterest <- c("LABEL")

#Variables expliquatives
featuresOfInterest <-  paste0("X", 1:144)


### load data set and give generic name -------------------------------------
if(useSeed)
  set.seed(nSeed)



# Il séléctionne les labels en essayant de garder les mêmes proportions que sur les données 
# D'où le fait qu'il utlise un group_by
# compDf est donc un échantillon de 52 x 146
# Pour moi: responsesOfInterest[indResp] est pareil que responsesOfInterest

compDf <- sqAnoDf %>%   # Notre data frame
  #dplyr::filter(!is.na(delta) & delta >= 0) %>%
  dplyr::select(responsesOfInterest[indResp], featuresOfInterest) %>% # Sélectionne la colonne LABEL et les colonnes commençant par X
  na.omit(.) %>% # Enlève les lignes ayant des NA
  dplyr::group_by(responsesOfInterest[indResp]) %>% # Regroupe par rapport à la variable LABEL
  dplyr::sample_frac(pSubSample) %>%  # Sélectionne pour chaque LABEL une proportion de 0.01
  dplyr::ungroup()  # Enlève l'effet group_by 

# Récupère la classe de la variable LABEL
classResp <- class(compDf[, responsesOfInterest[indResp]][[1]]) 
yTrain <- compDf[, responsesOfInterest[indResp]][[1]]

#-------------------Boruta---------------------#
resBoruta <- Boruta(x = compDf[, featuresOfInterest],
                    y = as.numeric(as.factor(yTrain)),
                    doTrace = 2,
                    num.threads = nCores - 1,
                    ntree = 100)

resBorutaImpDf <- as.data.frame(resBoruta$ImpHistory) %>%
  tidyr::gather(key = "Feature", value = "Importance") %>% 
  dplyr::mutate(Decision = as.character(resBoruta$finalDecision[match(Feature,
                                                                      names(resBoruta$finalDecision))]),
                Decision = factor(ifelse(grepl("shadow", Feature),
                                         "Shadow",
                                         Decision),
                                  levels = c("Confirmed", "Tentative", "Rejected", "Shadow")))

xLevels <- as.character((resBorutaImpDf %>%
                           dplyr::group_by(Feature) %>%
                           dplyr::summarize(median = median(Importance,
                                                            na.rm = TRUE)) %>%
                           dplyr::arrange(median) %>%
                           as.data.frame(.))$Feature)

inputBoruta <- round(nrow(resBorutaImpDf) / 4,0)
nrowBoruta <- nrow(resBorutaImpDf)

#-------------------LASSO---------------------#
compMat <- model.matrix(~ .-1, compDf[, featuresOfInterest])


### run cv lasso to identify best lambda ------------------------------------
# Il faut utiliser family = multinomial au lieu de binomial
cv.lasso <- cv.glmnet(x = compMat,
                      y = yTrain,
                      intercept = FALSE,
                      family = "multinomial",
                      alpha = 1,
                      nfolds = 10)
tidy.lasso <- tidy(cv.lasso)
glance.lasso <- glance(cv.lasso)

### run once to get an idea of variable importance --------------------------
fit.lasso <- glmnet(x = compMat,
                    y = as.numeric(as.factor(yTrain)),
                    intercept = FALSE,
                    family = "multinomial",
                    alpha = 1)

#-------------------Ridge---------------------#
cv.ridge <- cv.glmnet(x = compMat,
                      y = yTrain,
                      intercept = FALSE,
                      family = "multinomial",
                      alpha = 0,
                      nfolds = 10)

tidy.ridge <- tidy(cv.ridge)
glance.ridge <- glance(cv.ridge)

### run once to get an idea of variable importance --------------------------
fit.ridge <- glmnet(x = compMat,
                    y = as.numeric(as.factor(yTrain)),
                    intercept = FALSE,
                    family = "multinomial",
                    alpha = 0)


#-------------------knn---------------------#
control <- trainControl(method="cv", number=10, repeats=5)

fit.knn <- train(compMat,yTrain,  method="knn", trControl=control)


#-------------------arbre de decision---------------------#
fit.tree <- train(compMat,yTrain, method = "rpart", trControl = control)

#-------------------randomforest---------------------#
fit.rf <- train(compMat,yTrain, method = "rf", trControl = control)

#-------------------lasso---------------------#
fit.lass <- train(compMat, yTrain, method = "glmnet", trControl = control,
                tuneGrid = expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.001)))

#-------------------ridge---------------------#
fit.ridg <- train(compMat, yTrain, method = "glmnet", trControl = control,
                tuneGrid = expand.grid(alpha = 0,lambda = seq(0,0.5,by = 0.01)))

#-------------------svm---------------------#
fit.svm <- train(compMat, yTrain, method="svmRadial", trControl=control)

#-------------------resultats---------------------#
results <- resamples(list(LASSO=fit.lass, RIDGE=fit.ridg, SVM=fit.svm, KNN=fit.knn, TREE = fit.arbre, RF=fit.rf))

#summary(results)
#library(highcharter)
scales <- list(x=list(relation="free"), y=list(relation="free"))
compareAlgo <- bwplot(results, scales=scales)
#hcboxplot(results$values)
