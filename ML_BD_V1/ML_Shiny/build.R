require(caret)
control <- trainControl(method = "cv", number = 5)
data_config <- read.csv2("ML_Shiny/ressources/config-2020-02-13.csv")
data_inp <- read.csv("ML_Shiny/www/iris.csv")

form_algo <- list()
apply(data_config, 1, function(x){
    preproc <- x[which(names(data_config)=="preProc")]
    tunegrid <- paste("expand.grid(", x[which(names(data_config)=="tuneGrid")], ")", sep="")
    method <- paste("'", x[which(names(data_config)=="method")], "'")
    expr1 <- paste("form_algo[['", x[which(names(data_config)=="method")], "']]", sep="")
    expr2 <- paste(
        "form = y ~ ." ,
        "data = data_inp",
        paste("preProcess", preproc, sep=" = "),
        paste("method", method, sep=" = "),
        "trControl = control",
        paste("tuneGrid", tunegrid, sep=" = "),
        sep = ", " 
        )
    expr3 <- paste("list(", expr2, ")", sep="")
    expr <- paste(expr1, expr3, sep="<<-")
    eval(parse(text = expr))
})

form_algo <- list(
    "adaboost" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "adaboost",
        trControl = control,
        tuneGrid = grid[["adaboost"]]
    ),
    "AdaBoost.M1" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "AdaBoost.M1",
        trControl = control,
        tuneGrid = grid[["AdaBoost.M1"]]
    ),
    "rpart" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "rpart",
        trControl = control,
        tuneGrid = grid[["rpart"]]
    ),
    "ranger" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "ranger",
        trControl = control,
        tuneGrid = grid[["ranger"]]
    ),
    "rf" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "rf",
        trControl = control,
        tuneGrid = grid[["rf"]],
        ntree = 500
    ),
    "knn" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "knn",
        trControl = control,
        tuneGrid = grid[["knn"]]
    ),
    "glmnet" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "glmnet",
        trControl = control,
        # family="multinomial",
        tuneGrid = grid[["glmnet"]]
    )
)