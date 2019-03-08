library(plyr)
library(tidyverse)
library(readxl)
library(stringr)
library(beepr)

rm(list=ls())

#Read metadata file
post2015Meta <- read_xlsx("PermDisclosureData/meta.xlsx",
    col_names=c("Names","Type"),col_types="text",skip=1)

#Generate list of data files
post2015DataFiles <- dir("PermDisclosureData",pattern="[2][0][1][5-8].xlsx"
    ,full.names=TRUE)

#Missing value strings
post2015Missing <- c("#############","NULL","N/A")

#Data file import function
post2015Import <- function(dataFile) {
    raw <- read_xlsx(dataFile,col_names=post2015Meta$Names,
        col_types=post2015Meta$Type,skip=1,na=post2015Missing)    
}

#Import data
PERM2015 <- post2015Import(post2015DataFiles[1])
PERM2016 <- post2015Import(post2015DataFiles[2])
PERM2017 <- post2015Import(post2015DataFiles[3])
PERM2018 <- post2015Import(post2015DataFiles[4])
#PERM2019 <- post2015Import(post2015DataFiles[5])

#Add fiscal year variable
PERM2015 <- mutate(PERM2015,FY=2015)
PERM2016 <- mutate(PERM2016,FY=2016)
PERM2017 <- mutate(PERM2017,FY=2017)
PERM2018 <- mutate(PERM2018,FY=2018)
#PERM2019 <- mutate(PERM2019,FY=2019)

#Append rows
post2015 <- bind_rows(PERM2015,PERM2016,PERM2017,PERM2018)

#Save as named binary object
save(post2015,file='Wrangled.rdata')

#Read SOC index
SOCIndex <- read_xls('Resources/soc_2010_alphabetical_index.xls',
    col_names=c('Title','Code'),col_types=c('text'),skip=6)

#Save as named binary object
save(SOCIndex,file='SOCIndex.rdata')

    



    

 