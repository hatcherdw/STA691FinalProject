library(plyr)
library(tidyverse)
library(readxl)
library(magrittr)
library(stringr)

rm(list=ls())

pre2010DataFiles <- dir("PermDisclosureData",pattern="[2][0][0][8-9].xlsx",
    full.names=TRUE)
pre2015DataFiles <- append(pre2010DataFiles,dir("PermDisclosureData",
    pattern="[2][0][1][0-4].xlsx",full.names=TRUE))

pre2015Missing = c("UNCLASSIFIED")

pre2015Import <- function(dataFile) {
    raw <- read_xlsx(dataFile,col_names=TRUE,guess_max=10000,na=pre2015Missing)
    names(raw) <- str_replace_all(toupper(names(raw)),fixed(" "),"_")
    raw <- mutate_all(raw,c("toupper"))
    return(raw)
}

PERM2008 <- pre2015Import(pre2015DataFiles[1])
PERM2009 <- pre2015Import(pre2015DataFiles[2])
PERM2010 <- pre2015Import(pre2015DataFiles[3])
PERM2011 <- pre2015Import(pre2015DataFiles[4])
PERM2012 <- pre2015Import(pre2015DataFiles[5])
PERM2013 <- pre2015Import(pre2015DataFiles[6])
PERM2014 <- pre2015Import(pre2015DataFiles[7])

pre2015 <- rbind.fill(PERM2008,PERM2009,PERM2010,PERM2011,PERM2012,PERM2013,
    PERM2014)

post2015Meta <- read_xlsx("PermDisclosureData/meta.xlsx",
    col_names=c("Names","Type"),col_types="text",skip=1)

post2015DataFiles <- dir("PermDisclosureData",pattern="[2][0][1][5-8].xlsx"
    ,full.names=TRUE)

post2015Missing <- c("#############","NULL")

post2015Import <- function(dataFile) {
    raw <- read_xlsx(dataFile,col_names=post2015Meta$Names,col_types=post2015Meta$Type
        ,skip=1,na=post2015Missing)    
    names(raw) <- toupper(names(raw))
    return(raw)
}

PERM2015 <- post2015Import(post2015DataFiles[1])
PERM2016 <- post2015Import(post2015DataFiles[2])
PERM2017 <- post2015Import(post2015DataFiles[3])
PERM2018 <- post2015Import(post2015DataFiles[4])

post2015 <- bind_rows(PERM2015,PERM2016,PERM2017,PERM2018)

PERMAllVars <- rbind.fill(pre2015,post2015)

varsInBoth <- c("CASE_NUMBER","DECISION_DATE","CASE_STATUS","EMPLOYER_NAME",
    "EMPLOYER_ADDRESS_1","EMPLOYER_ADDRESS_2","EMPLOYER_CITY","EMPLOYER_STATE",
    "EMPLOYER_POSTAL_CODE","2007_NAICS_US_CODE","2007_NAICS_US_TITLE",
    "PW_SOC_CODE","PW_JOB_TITLE_9089","PW_LEVEL_9089","PW_AMOUNT_9089",
    "PW_UNIT_OF_PAY_9089","WAGE_OFFER_FROM_9089","WAGE_OFFER_TO_9089",
    "WAGE_OFFER_UNIT_OF_PAY_9089","JOB_INFO_WORK_CITY","JOB_INFO_WORK_STATE",
    "COUNTRY_OF_CITIZENSHIP","CLASS_OF_ADMISSION","NAICS_US_CODE",
    "NAICS_US_TITLE")

PERMReduced <- PERMAllVars[,varsInBoth]

hourlyPay <- function(amount,unit) {
    if (!is.na(amount) && !is.na(unit)) {
        amount <- as.numeric(amount)
        if (identical(unit,'YEAR') || identical(unit,'YR')) {
            hourly <- amount / 2080
        }
        else if (identical(unit,'HOUR') || identical(unit,'HR')) {
            hourly <- amount
        }
        else if (identical(unit,'WEEK') || identical(unit,'WK')) {
            hourly <- amount / 40
        }
        else if (identical(unit,'MONTH') || identical(unit,'MTH')) {
            hourly <- amount / 160
        }
        else if (identical(unit,'BI-WEEKLY') || identical(unit,'BI')) {
            hourly <- amount / 80
        }
    }
    else {
        hourly <- NA
    }
    return(hourly)
}


PWHourly <- vector('numeric',length(PERMReduced))
for (i in 1:nrow(PERMReduced)) {
    PWHourly[[i]] <- hourlyPay(PERMReduced$PW_AMOUNT_9089[[i]],
        PERMReduced$PW_UNIT_OF_PAY_9089[[i]])
}   
    
    
    
    
    
 