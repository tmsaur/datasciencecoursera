##
##Function that find the hospital in a state for a given rank and outcome (heart attack, heart failure, heart pneumonia)
## Tor Martin Saur 25.02.2015
## 
##
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    failure <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    attack <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    pneumonia <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia' 
    Hospitalname <- 'Hospital.Name'
    
    ## Check that state and outcome are valid
    if(state %in% unique(outcomedata[,7])){
        #test if the outcome is valid
        if(outcome == "heart attack"){
            heartAttackNA <- is.na(outcomedata[[attack]])
            heartAttackClean <- subset(outcomedata, outcomedata[[attack]] != heartAttackNA)
            ##Transform the column from character to numeric in order to sorting
            heartAttackClean[[attack]] <- as.numeric(as.character(heartAttackClean[[attack]]))
            stateDataAttack <- subset(heartAttackClean, heartAttackClean$State == state)
            ##Sort the Hospitals:
            stateDataSorted <- stateDataAttack[order(stateDataAttack[[attack]], stateDataAttack[[Hospitalname]]),]
            ## Check the num value
            if(num == "best"){
                rankedHospital <- stateDataSorted[1,2]
            }
            else if(num == "worst" && nrow(stateDataSorted) > 0 ){
                lengthData <- nrow(stateDataSorted)
                rankedHospital <- stateDataSorted[lengthData, 2]
            }
            else if(as.numeric(num)>0){
                num <- as.numeric(num)
                if(num <= nrow(stateDataSorted) ){
                    rankedHospital <- stateDataSorted[num,2]
                } else{  
                    return (NA)
                }
            }
            else{
                stop("invalid num2")
            }
            return (rankedHospital) 
        }
        
        else if(outcome == "heart failure"){
            ## Get the best Hospital for Heart Failure
            hearFailureNA <- is.na(outcomedata[[failure]]) ##logic vector for na's
            heartFailureClean <- subset(outcomedata, outcomedata[[failure]] !=hearFailureNA)
            
            ##Transform the column from character to numeric in order to find the minimum value
            heartFailureClean[[failure]] = as.numeric(as.character(heartFailureClean[[failure]]))
            stateData <- subset(heartFailureClean, heartFailureClean$State == "TX")
            
            ##Sort the Hospitals:
            stateDataSortedHF <- stateData[order(stateData[[failure]], stateData[[Hospitalname]]),]
            ## Check the num value
            if(num == "best"){
                rankedHospital <- stateDataSortedHF[1,2]
            }
            else if(num == "worst" && nrow(stateDataSortedHF) > 0 ){
                lengthData <- nrow(stateDataSortedHF)
                rankedHospital <- stateDataSortedHF[lengthData, 2]
            }
            else if(as.numeric(num)>0){
                num <- as.numeric(num)
                if(num <= nrow(stateDataSortedHF) ){
                    rankedHospital <- stateDataSortedHF[num,2]
                } else{  
                    return (NA)
                }
            }
            else{
                stop("invalid num2")
            }
            return (rankedHospital) 
        }
        else if(outcome == "pneumonia"){
            hearPneumoniaNA <- is.na(outcomedata[[pneumonia]]) ##logic vector for na's
            heartPneumoniaClean <- subset(outcomedata, outcomedata[[pneumonia]] !=hearPneumoniaNA)
            
            ##Transform the column from character to numeric in order to find the minimum value
            heartPneumoniaClean[[pneumonia]] = as.numeric(as.character(heartPneumoniaClean[[pneumonia]]))
            stateDataPneumonia <- subset(heartPneumoniaClean, heartPneumoniaClean$State == state)
            
            ## Sort the states based on 
            stateDataSortedHP <- stateDataPneumonia[order(stateDataPneumonia[[pneumonia]], stateDataPneumonia[[Hospitalname]]),]
            
            ## Check the num value
            if(num == "best"){
                rankedHospital <- stateDataSortedHP[1,2]
            }
            else if(num == "worst" && nrow(stateDataSortedHP) > 0 ){
                lengthData <- nrow(stateDataSortedHP)
                rankedHospital <- stateDataSortedHP[lengthData, 2]
            }
            else if(as.numeric(num)>0){
                num <- as.numeric(num)
                if(num <= nrow(stateDataSortedHP) ){
                    rankedHospital <- stateDataSortedHP[num,2]
                } else{  
                    return (NA)
                }
            }
            else{
                stop("invalid num")
            }
            return (rankedHospital) 
        }
        else{
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
}

## For test purpose:
## rankhospital("TX", "pneumonia", "worst")
## rankhospital("TX", "pneumonia", "10")
## rankhospital("TX", "heart failure", "5000")
## rankhospital("TX", "heart attack", "best")