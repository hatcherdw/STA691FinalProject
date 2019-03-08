library(plyr)
library(tidyverse)
library(stringr)

rm(list=ls())

#Load wrangeled binary data (object name is post2015)
load('Wrangled.rdata')

#Load SOCIndex
load('SOCIndex.rdata')

#Function for calculating hourly pay
hourlyPay <- function(amount,unit) {
    if (!is.na(amount) && !is.na(unit)) {
        amount <- as.numeric(amount)
        switch(unit,
            'Year' = amount / 2080,
            'Hour' = amount,
            'Week' = amount / 40,
            'Month' = amount / 160,
            'Bi-Weekly' = amount / 80)
    }
    else {
        return(NA)
    }
}

#Calculate hourly PWD and hourly offer
PWHourly <- vector("numeric",nrow(post2015))   
offerHourly <- vector("numeric",nrow(post2015))
for (i in seq_along(PWHourly)) {
    PWHourly[[i]] <- hourlyPay(post2015$PW_AMOUNT_9089[[i]],
        post2015$PW_UNIT_OF_PAY_9089[[i]])
    offerHourly[[i]] <- hourlyPay(post2015$WAGE_OFFER_FROM_9089[[i]],
        post2015$WAGE_OFFER_UNIT_OF_PAY_9089[[i]])
}

#Extract valid SOC codes
validSOC <- vector('character',nrow(post2015))
for (i in 1:nrow(post2015)){
    rawCode <- str_detect(post2015$PW_SOC_CODE[[i]],'[0-9]{2}-[0-9]{4}') 
    #If raw code is valid, keep
    if(!is.na(rawCode) & rawCode == TRUE){
        validSOC[[i]] <- post2015$PW_SOC_CODE[[i]]     
    }
    else{
        rawTitle <- post2015$PW_SOC_TITLE[[i]]
        #If raw code is not valid, extract raw title
        if(!is.na(rawTitle)){
            #Match raw title with code in idex
            index <- as.integer(which(SOCIndex$Title == rawTitle))
            ifelse(length(index) > 1,index <- index[-1],index <- index)
            validSOC[[i]] <- SOCIndex$Code[index]
        }
        else{
            #If both raw code and title missing
            validSOC[[i]] <- NA    
        }
    }
}

moreStateNames <- c(toupper(state.name),'BRITISH COLUMBIA',
    'DISTRICT OF COLUMBIA','GUAM','NORTHERN MARIANA ISLANDS','PUERTO RICO',
    'VIRGIN ISLANDS','MARSHALL ISLANDS','FEDERATED STATES OF MICRONESIA')

moreStatsAbbs <- c(state.abb,'BC','DC','GU','MP','PR','VI','MH','FM')

proRecruitmentCount <- apply(post2015[,80:99], 1, function(x) 
    ceiling(sum(!is.na(x))/2)) 

#Transformations
trans <- post2015 %>%
    mutate(.,Status=revalue(CASE_STATUS,c("Certified-Expired"="Certified"))) %>%
    mutate(.,PWHourly) %>%
    mutate(.,offerHourly) %>%
    mutate(.,SOCMajor = str_sub(validSOC,1,2)) %>%
    mutate(.,NAICSMajor = str_sub(NAICS_US_CODE,1,2)) %>%
    mutate(.,PWAge = as.numeric(difftime(DECISION_DATE,PW_DETERM_DATE,
        units=c('days')))) %>%
    mutate(.,EmployerEstab = ifelse(post2015$EMPLOYER_YR_ESTAB>1600,
        post2015$EMPLOYER_YR_ESTAB,NA)) %>%
    mutate(.,EmployerAge = FY-EmployerEstab) %>%
    mutate(.,EmployerState = mapvalues(EMPLOYER_STATE,moreStateNames,
        moreStatsAbbs)) %>%
    mutate(.,AgentState = mapvalues(AGENT_STATE,moreStateNames,
        moreStatsAbbs)) %>%
    mutate(.,JobState = mapvalues(JOB_INFO_WORK_STATE,moreStateNames,
        moreStatsAbbs)) %>%
    mutate(.,JobEducation = factor(JOB_INFO_EDUCATION,ordered=TRUE,
        levels=c('None','High School',"Associate's","Bachelor's","Master's",
            'Other',"Doctorate"))) %>%
    mutate(.,WorkerEducation = factor(FOREIGN_WORKER_INFO_EDUCATION,ordered=TRUE,
        levels=c('None','High School',"Associate's","Bachelor's","Master's",
            'Other',"Doctorate"))) %>% 
    mutate(.,lag2015 = 
            (as.POSIXlt(post2015$CASE_RECEIVED_DATE)$year+1900)-2015) %>%
    mutate(.,ProRecruitment = proRecruitmentCount) %>%

    #Drop variables
    select(.,-c(80:99)) %>%
    select(.,-c(CASE_STATUS)) %>%
    select(.,-c(PW_AMOUNT_9089)) %>%
    select(.,-c(WAGE_OFFER_FROM_9089)) %>%
    select(.,-c(WAGE_OFFER_TO_9089)) %>%
    select(.,-c(NAICS_US_CODE)) %>%
    select(.,-c(DECISION_DATE,CASE_RECEIVED_DATE,ORIG_FILE_DATE,PW_DETERM_DATE,
        PW_EXPIRE_DATE,ORIG_CASE_NO)) %>%
    select(.,-c(EMPLOYER_YR_ESTAB,EmployerEstab)) %>%
    select(.,-c(EMPLOYER_COUNTRY)) %>%
    select(.,-c(EMPLOYER_STATE,AGENT_STATE,JOB_INFO_WORK_STATE)) %>%
    select(.,-c(JOB_INFO_EDUCATION)) %>%
    select(.,-c(FOREIGN_WORKER_INFO_EDUCATION)) %>%
    select(.,-c(FY)) %>%
    
    #Remove withdrawn cases
    filter(Status %in% c('Certified','Denied')) %>%

    #Remove cases with unrealistic PW ages
    filter(PWAge > 0)
    
#Convert to data frame
DF <- as.data.frame(unclass(trans),stringsAsFactors = TRUE)

#Count number of levels
numLevels <- lapply(DF,function(x) nlevels(x))

#Remove variables with unstructured data
selectByLevels <- DF %>%
    select(which(numLevels < 50 & numLevels != 1))

#Calculate percent missing for each variable
pctMissing <- colSums(is.na(selectByLevels)) / nrow(selectByLevels)

#Remove variable with "high" percent missing
selectByMissing <- selectByLevels %>%
    select(which(pctMissing < 0.05))

#Keep only complete records
nonMissing <- na.omit(selectByMissing)

#Save as unnamed binary R object
saveRDS(nonMissing, file='Transformed.rds')
