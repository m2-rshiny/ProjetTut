



'svmLinear'=list(form=y ~ .,
                 data = mydata,
                 #preProcess = c('scale','center'),
                 method = 'svmLinear',
                 trControl = control,
                 tuneGrid=tuneParams[['svmLinear']])
'svmPoly'= list(form=y ~ .,
                data = mydata,
                preProcess = c('scale','center'),
                method = 'svmPoly',
                trControl = control,
                tuneGrid=tuneParams[['svmPoly']])


output$compare <- renderPlot({
    scales <- list(x=list(relation="free"), y=list(relation="free"))
    bwplot(results, scales=scales)
})


#-------------------knn---------------------#


fit.knn <- train(compMat,yTrain,  method="knn", trControl=control,
                 tuneGrid = expand.grid(k = c(2,3,5,7,10,15,19,21)))


#-------------------arbre de decision---------------------#
fit.tree <- train(compMat,yTrain, method = "rpart", trControl = control)

#-------------------randomforest---------------------#
fit.rf <- train(compMat,yTrain, method = "rf",trControl = control)

#-------------------lasso---------------------#
fit.lass <- train(compMat, yTrain, method = "glmnet", trControl = control,
                  tuneGrid = expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.001)))

#-------------------ridge---------------------#
fit.ridg <- train(compMat, yTrain, method = "glmnet", trControl = control,
                  tuneGrid = expand.grid(alpha = 0,lambda = seq(0,0.5,by = 0.01)))

#-------------------svm---------------------#
fit.svm <- train(compMat, yTrain, method="svmRadial", trControl=control)

#-------------------resultats---------------------#


results <- resamples(list(LASSO=fit.lass, RIDGE=fit.ridg, SVM=fit.svm, KNN=fit.knn, TREE = fit.tree, RF=fit.rf))

#summary(results)
#library(highcharter)
scales <- list(x=list(relation="free"), y=list(relation="free"))
compareAlgo <- bwplot(results, scales=scales)
#hcboxplot(results$values)


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
                  Decision = factor(ifelse(grepl("shadow", Feature),"Shadow",Decision),
                                    levels = c("Confirmed", "Tentative", "Rejected", "Shadow")))

xLevels <- as.character((resBorutaImpDf %>%
                             dplyr::group_by(Feature) %>%
                             dplyr::summarize(median = median(Importance,
                                                              na.rm = TRUE)) %>%
                             dplyr::arrange(median) %>%
                             as.data.frame(.))$Feature)

inputBoruta <- round(nrow(resBorutaImpDf) / 4,0)
nrowBoruta <- nrow(resBorutaImpDf)


# graphe boruta
output$boruta <- renderPlot({
    resBorutaImpDf %>%
        arrange(Decision) %>% 
        slice(1:input$nrowBoruta) %>% 
        dplyr::mutate(Feature = factor(Feature,levels = xLevels)) %>% 
        ggplot(aes(x = Feature, y = Importance, fill = Decision)) +
        geom_boxplot() +
        scale_fill_manual(values = c("green", "yellow", "red", "blue"), drop = FALSE) +
        coord_flip() +
        theme(axis.text.x = element_text(size = 8,hjust = 1,vjust = 1)) +
        ggtitle("Output of Boruta Algorithm") 
})


column(6, plotOutput("boruta"), # graph boruta
       sliderInput("nrowBoruta", "Variables importantes:", 1, nrowBoruta, inputBoruta))


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

# graphe lasso
output$lasso <- renderPlot({
    ggplot(tidy.lasso, aes(lambda, estimate)) +
        geom_point(color = "red") +
        scale_x_log10() + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
        geom_vline(xintercept = glance.lasso$lambda.min, lty = 2) +
        geom_vline(xintercept = glance.lasso$lambda.1se, lty = 2)
})

,

fluidRow(
    column(12, plotOutput("lasso")), # graphe lasso
    
    
)

