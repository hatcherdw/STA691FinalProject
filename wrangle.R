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
    raw <- mutate_all(raw,c("toupper"))
}

post2015List <- vector("list",length(post2015DataFiles))
for (i in seq_along(post2015DataFiles)) {
    post2015List[[i]] <- post2015Import(post2015DataFiles[[i]])
}

post2015 <- bind_rows(post2015List)

PERMALL <- rbind.fill(pre2015,post2015)

PERMALLtrans1 <- PERMALL %$%
    mutate(.,EmployerState = mapvalues(EMPLOYER_STATE,toupper(state.name),
        state.abb)) %$%
    mutate(.,JobState = mapvalues(JOB_INFO_WORK_STATE,toupper(state.name),
        state.abb)) %$%
    mutate(.,SOCMajor = str_sub(PW_SOC_CODE,1,2))

PERMALLDF <- as.data.frame(unclass(PERMALLtrans1),stringsAsFactors = TRUE)

numLevels <- lapply(PERMALLDF, function(x) nlevels(x))
pctMissing <- lapply(PERMALLDF, function(x) sum(is.na(x))/nrow(PERMALLDF))

PERMALLtrans1 <- PERMALLDF %>%
    select(which(pctMissing < 0.5 & numLevels < 300)) %>%
    filter(CASE_STATUS %in% c("CERTIFIED","CERTIFIED-EXPIRED","DENIED")) %$%
    mutate(.,Denied = revalue(CASE_STATUS, c("CERTIFIED"=0,
        "CERTIFIED-EXPIRED"=0,"DENIED"=1)))
    
    
    
    
    
    
           