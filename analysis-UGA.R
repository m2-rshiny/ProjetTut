library(tidyverse)
library(readxl)
library(BDbasics)
library(caret)
library(Hmisc)
library(parallel)
library(foreach)
library(doParallel)
library(Boruta)
library(GGally)
library(glmnet)

#Chemin
setwd("/home/chabeau/Documents/UGA/ProjetTut")
setwd("/Users/LamineDiamban/Desktop/SSD/S9/ProjetTut")

### load serialized data ----------------------------------------------------
sqAnoDf <- read_tsv(file = "anonymized-sq-dataset.tsv")


### load training configuration ---------------------------------------------
trainConfigDf <- read_xlsx(path = "MLShiny/Training Configuration.xlsx", sheet = "Sheet1") %>% dplyr::filter(use == 1)


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
  # "character"

### sanity check ------------------------------------------------------------
table(compDf[, responsesOfInterest[indResp]][[1]])  # Table de contigence de notre échantillon
# L0 L1 L2 L3 L4 
# 16  6  8 10 12 
#  Low High 
# 3611  963 


### summary plot ------------------------------------------------------------
# plot(compDf)
  
# Cette fonction prend beacoup de temps à s'exécuter
# On pourrait le mettre de coté en attendant
plotList[["corPlot"]] <- ggpairs(compDf,
                                 upper = list(continuous = wrap('cor',
                                                                method = "spearman")),
                                 mapping = ggplot2::aes_string(colour = responsesOfInterest[indResp])) +
  theme_bw()

plotList[["corPlot"]]


### summary stats -----------------------------------------------------------
# La fonction summaryTable vient surement de leur package BDbasics
sumList[["overall"]] <- summaryTable(compDf %>%
                                       dplyr::select(responsesOfInterest[indResp],
                                                     featuresOfInterest) %>%
                                       as.data.frame(.),
                                     cont.options = list(mean.ci = "boot.perc",
                                                         nDigit = 2),
                                     cat.options = list(show.percent = TRUE,
                                                        percent.calc = "use.all",
                                                        show.total = TRUE),
                                     make.latex.safe = FALSE)


# per level of response if either factor or character
if(classResp %in% c("factor", "character"))
  sumList[["perResp"]] <- summaryTable(compDf %>%
                                         dplyr::select(featuresOfInterest) %>%
                                         as.data.frame(.),
                                       by = compDf[, responsesOfInterest[indResp]][[1]],
                                       cont.options = list(mean.ci = "boot.perc",
                                                           nDigit = 2),
                                       cat.options = list(show.percent = TRUE,
                                                          percent.calc = "use.all",
                                                          show.total = TRUE),
                                       make.latex.safe = FALSE)

### Boruta ------------------------------------------------------------------
# Boruta permet la sélection de variables importantes. 
# Il s'appuie sur le randomForest pour faire la sélection
# On sélecionne les variables(x) par rapport à la variable LABEL(y)
resBoruta <- Boruta(x = compDf[, featuresOfInterest],
                    y = as.numeric(as.factor(compDf[, responsesOfInterest[indResp]][[1]])),
                    doTrace = 2,
                    num.threads = nCores - 1,
                    ntree = 100)

# gather permet d'avoir 2 variables: Feature(contient le nom des variables) et Importance(leur valeur). 
# En gros, ça permet de pivoter le data frame
# Ensuite il crée une variable Décision qui contient les 4 modalités: Confirmed,Tentative,Rejected,Shadow
# Il se base sur l'objet finalDecision qui renvoie ces modalités puis les attache avec les 2 variables de gather 
resBorutaImpDf <- as.data.frame(resBoruta$ImpHistory) %>%
  tidyr::gather(key = "Feature", value = "Importance") %>% 
  dplyr::mutate(Decision = as.character(resBoruta$finalDecision[match(Feature,
                                                                      names(resBoruta$finalDecision))]),
                Decision = factor(ifelse(grepl("shadow", Feature),
                                         "Shadow",
                                         Decision),
                                  levels = c("Confirmed", "Tentative", "Rejected", "Shadow")))
# Feature Importance  Decision
# X1 -1.056539e+00  Rejected
# X1  0.000000e+00  Rejected

# group_by: Regroupe les données par rapport aux variables
# Summarize: permet de faire des calcules, ici il crée 2 variables
# Importance: qui contient la médiane de l'importance de chaque variable
# Décision: il utilise "unique" pour enlever les doublons des variables
# Puis il classe les médianes de chaque variable de manière décroissante
# Mutate permet de créer des variables. Il crée Rank qui est le rang de chaque varaible
sumImpSqBorutaDf <- resBorutaImpDf %>%
  dplyr::group_by(Feature) %>%
  dplyr::summarize(Importance = median(Importance,
                                       na.rm = TRUE),
                   Decision = unique(Decision)) %>%
  dplyr::arrange(desc(Importance)) %>%
  dplyr::mutate(Rank = 1:n())

# Il récupère le nom des variables en commençant par celle qui a la plus grande médiane
xLevels <- as.character((resBorutaImpDf %>%
                           dplyr::group_by(Feature) %>%
                           dplyr::summarize(median = median(Importance,
                                                            na.rm = TRUE)) %>%
                           dplyr::arrange(median) %>%
                           as.data.frame(.))$Feature)

# Fait des boxplot pour voir les variables importantes
plotList[["Boruta"]] <- ggplot(data = resBorutaImpDf %>%
                                 dplyr::mutate(Feature = factor(Feature,
                                                                levels = xLevels)),
                               aes(x = Feature,
                                   y = Importance,
                                   fill = Decision)) +
  geom_boxplot() +
  scale_fill_manual(values = c("green", "yellow", "red", "blue"),
                    drop = FALSE) +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8,
                                   hjust = 1,
                                   vjust = 1)) +
  ggtitle("Output of Boruta Algorithm") +
  theme_bw()

plotList[["Boruta"]]


### LASSO -------------------------------------------------------------------
### reformat using one-hot encoding -----------------------------------------
compMat <- model.matrix(~ .-1, compDf[, featuresOfInterest])


### run cv lasso to identify best lambda ------------------------------------
# Il faut utiliser family = multinomial au lieu de binomial
cv.lasso <- cv.glmnet(x = compMat,
                      y = compDf[, responsesOfInterest[indResp]][[1]],
                      intercept = FALSE,
                      family = "multinomial",
                      alpha = 1,
                      nfolds = 10)

plot(cv.lasso, xvar = "lambda", label = TRUE)


### run once to get an idea of variable importance --------------------------
fit.lasso <- glmnet(x = compMat,
                    y = as.numeric(as.factor(compDf[, responsesOfInterest[indResp]][[1]])),
                    intercept = FALSE,
                    family = "multinomial",
                    alpha = 1)
plot(fit.lasso,
     xvar = "lambda",
     label = TRUE)
abline(v = log(cv.lasso$lambda.min),lty = 3)
abline(v = log(cv.lasso$lambda.1se),lty = 3)


### get list of variables and coefficients ----------------------------------
sumList[["lassoCoeff"]] <- as.data.frame(as.matrix(coef(cv.lasso,
                                                        s = "lambda.min"))) %>%
  tibble::rownames_to_column("Feature") %>%
  dplyr::filter(Feature != "(Intercept)") %>%
  dplyr::rename(Coefficient = `1`) %>%
  tibble::add_column(Index = 1:nrow(.),
                     .after = 1) %>%
  dplyr::arrange(desc(abs(Coefficient)))

sumList[["lassoCoeff"]]


### caret processes ---------------------------------------------------------
# parallelized with foreach and doParallel package
cl <- makePSOCKcluster(nCores - 1)
registerDoParallel(cl)

fitList <- foreach(iCond = 1:nrow(trainConfigDf)) %do% {
  feMethod <- trainConfigDf$method[iCond]
  fePreProc <- if(!is.na(trainConfigDf$preProc[iCond])){
    unlist(strsplit(trainConfigDf$preProc[iCond], split = "\\s"))
  }else{
    NULL
  }
  
  feTrainMetric <- trainConfigDf$trainMetric[iCond]
  
  feTrainControl <-  if(!is.na(trainConfigDf$trainControl[iCond])){
    eval(parse(text = paste0("trainControl(",
                             trainConfigDf$trainControl[iCond],
                             ")")))
  }else{
    trainControl()
  }
  
  
  feTuneGrid <- if(!is.na(trainConfigDf$tuneGrid[iCond])){
    eval(parse(text = paste0("expand.grid(",
                             trainConfigDf$tuneGrid[iCond],
                             ")")))
  }else{
    NULL
  }
  
  if(useSeed)
    set.seed(nSeed)
  
  train(as.formula(paste0(responsesOfInterest[indResp],
                          " ~",
                          paste(featuresOfInterest,
                                collapse = "+"))),
        data = compDf,
        method = feMethod,
        preProcess = fePreProc,
        tuneGrid =  feTuneGrid,
        metric = feTrainMetric,
        trControl = feTrainControl)
}
names(fitList) <- trainConfigDf$name
stopCluster(cl)


### summarize performance ---------------------------------------------------
reSampledFit <- resamples(fitList)
summary(reSampledFit)

plotList[["dotPlot"]] <- dotplot(reSampledFit,
                                 metric = feTrainMetric) #temporary: 
#uses latest choice in foreach loop

plotList[["dotPlot"]]

gc()


### use best model ----------------------------------------------------------
# bestModel <- fitList[["XGBoost"]]$finalModel
# 
# predict(bestModel,
#         compDf)


### serialize results -------------------------------------------------------
save(sumList,
     plotList,
     fitList,
     reSampledFit,
     file = "RData/analysisCaret.RData")
# load(file = "RData/analysisCaret.RData")
