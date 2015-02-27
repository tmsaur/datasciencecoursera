##
## Ranks hospitals on given outcome. I.e: Rank the fifth hospital in all states 
## in US and return the hospital name and state short name (2 letters)
## Tor Martin Saur 26.02.2015
## 
rankall <- function(outcome, num = "best"){
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    failure <-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    attack <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    pneumonia <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia' 
    Hospitalname <- 'Hospital.Name'
    if(outcome == "heart attack"){
        attack <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack' 
    }
    else if(outcome == "heart failure"){
        attack <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    }
    else if(outcome == 'pneumonia'){
        attack <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    }
    ## num <- 4 ## number 4
    ## Collect states in the data set
    states <- unique(outcomedata[,7])
    states <- states[order(states)]
    ##str(states)
    ?rbind
    hospitalRank <- data.frame(hospital= character(0), state= character(0), stringsAsFactors = FALSE)
    ## get the hospital for given rank for each state
    hospitalNameV <- c()
    stateNameV <- c()
    for(state in states){
        rankedHospital <- data.frame(hospital= character(0), state= character(0), stringsAsFactors = FALSE)
        heartAttackNA <- is.na(outcomedata[[attack]])
        heartAttackClean <- subset(outcomedata, outcomedata[[attack]] != heartAttackNA)
        ##Transform the column from character to numeric in order to sorting
        heartAttackClean[[attack]] <- as.numeric(as.character(heartAttackClean[[attack]]))
        stateDataAttack <- subset(heartAttackClean, heartAttackClean$State == state)
        ##Sort the Hospitals:
        stateDataSorted <- stateDataAttack[order(stateDataAttack[[attack]], stateDataAttack[[Hospitalname]]),]
        
        ## Check the num value
        if(num == "best"){
            hospitalNameV <-  c(hospitalNameV,stateDataSorted[1,2])
            stateNameV <- c(stateNameV, state)
        }
        else if(num == "worst" && nrow(stateDataSorted) > 0 ){
            lengthData <- nrow(stateDataSorted)
            hospitalNameV <-  c(hospitalNameV,stateDataSorted[lengthData,2])
            stateNameV <- c(stateNameV, state)
        }
        else if(as.numeric(num)>0){
            num <- as.numeric(num)
            if(num <5000 ){
                nameHospital <- stateDataSorted[num,2]
                hospitalNameV <-  c(hospitalNameV,stateDataSorted[num,2])
                stateNameV <- c(stateNameV, state)
            } else{  
                return (NA)
            }
        }
        else{
            stop("invalid num2")
        }  
    }
    ## add the two vectors to the data frame
    hospitalRank<- data.frame(hospitalNameV, stateNameV, stringsAsFactors = FALSE)
    colnames(hospitalRank) <- c('hospital', 'state')
    return (hospitalRank)
}




