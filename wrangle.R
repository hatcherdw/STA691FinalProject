library(plyr)
library(tidyverse)
library(readxl)
library(magrittr)
library(forcats)

rm(list=ls())

#Read metadata file
meta <- read_xlsx("PermDisclosureData/meta.xlsx",col_names=c("Names","Type"),
    col_types="text")

#Get file names
dataFiles <- dir("PermDisclosureData",pattern="\\d{4}",full.names=TRUE)

#Define missing value strings
missing <- c("#############","NULL")

#XLSX import function
import <- function(dataFile) {
    read_xlsx(dataFile,col_names=meta$Names,col_types=meta$Type,skip=1,
        na=missing)    
}

#Import files into list
PERMALLlist <- vector("list",length(dataFiles))
for (i in seq_along(dataFiles)) {
    PERMALLlist[[i]] <- import(dataFiles[[i]])
}

#Collapse list to tibble
PERMALL <- bind_rows(PERMALLlist)

#Convert text columns to factors (other types preserved)
PERMALLDF <- as.data.frame(unclass(PERMALL),stringsAsFactors = TRUE)

#Count number of levels
numLevels <- lapply(PERMALLDF, function(x) nlevels(x))

PERMALL %>%
    select(which(numLevels < 2000)) %>%
    
    #Select columns with less then 50% missing
    select(which(colMeans(is.na(.)) < 0.5)) %>%
    
    #Filter out withdrawn observations
    filter(CASE_STATUS %in% c("Certified","Certified-Expired","Denied")) %$%
    
    #Revalue "Certified-Expired" and add as new variable
    mutate(.,status = revalue(CASE_STATUS,
        c("Certified-Expired" = "Certified"))) %$%
    
    #EXtract SOC major group and add as new variable
    mutate(.,SOCMajorGroup = str_sub(PW_SOC_CODE,1,2))

    #Test comment
    
    
    
    
    
           