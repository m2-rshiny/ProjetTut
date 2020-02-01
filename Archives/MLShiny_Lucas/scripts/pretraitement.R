require(tidyverse)
require(kernlab)
require(caret)
data(spam)

prop.table(table(spam$type))

trainIndex <- createDataPartition(spam$type)

args(trainControl)$method
test <- help(trainControl)
formals(trainControl)$method
body(trainControl)
match.arg(trainControl)
args(trainControl)
match.call(trainControl)
trainControl(method = )
formals(trainControl)
require(ggplot2)
sub("geom_","",apropos("^geom_"))
apropos("^method")
