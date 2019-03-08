library(caret)
library(tidyverse)

rm(list=ls())

data <- readRDS('Transformed.rds')
numLevels <- lapply(data,function(x) nlevels(x))

#Calulate variance
vars <- lapply(data,var)

oddsRatio <- function(variable){
    varTable <- table(variable,data[,'Status'])
    test <- fisher.test(varTable)
}

#Fisher's exact test for binary variables
ORs <- lapply(data[,which(numLevels == 2)],oddsRatio)
ORPs <- select(ORs,p.value)

chiSq <- function(variable){
    varTable <- table(variable,data[,'Status'])
    test <- chisq.test(varTable)
}

#Chi-square test for categorical variables
CSs <- lapply(data[,which(numLevels > 0)],chiSq)
CSPs <- select(CSs,p.value)

isNumeric <- lapply(data, is.numeric)
Xnumeric <- data[,which(isNumeric == TRUE)]
Y <- data[,c('Status')]

#ROC values for numeric variables
rocValues <- filterVarImp(x=Xnumeric,y=Y)
