##
##Function that find the best Hospital given state and outcome (heart attack, heart failure, heart pneumonia)
## Tor Martin Saur 25.02.2015
## Future improvments: inside the is-statements one can create at inner function that finds the 
## best hospital for given outcome (less code)
##

best <- function(state, outcome) {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    failure <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    attack <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    pneumonia <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'  
    
    ## Check that state and outcome are valid
    if(state %in% unique(outcomedata[,7])){
        #test if the outcome is valid
        if(outcome == "heart attack"){
            heartAttackNA <- is.na(outcomedata[[attack]])
            heartAttackClean <- subset(outcomedata, outcomedata[[attack]] != heartAttackNA)
            
            ##Transform the column we are interested in from character to numeric in order to find the minimum value
            heartAttackClean[[attack]] <- as.numeric(as.character(heartAttackClean[[attack]]))
            stateDataAttack <- subset(heartAttackClean, heartAttackClean$State == state)
            bestHospitalHA <- subset(stateDataAttack, stateDataAttack[[attack]] == min(stateDataAttack[[attack]]))
            
            if(length(bestHospitalHA) > 1){
                first <- sort(bestHospitalHA$Hospital.Name)
                bestHospital <- first[1]
            } else{  
                first <- bestHospitalHA$Hospital.Name
                bestHospital<- first[1]
            }
            return (bestHospital) 
        }
        else if(outcome == "heart failure"){
            ## Get the best Hospital for Heart Failure
            hearFailureNA <- is.na(outcomedata[[failure]]) ##logic vector for na's
            heartFailureClean <- subset(outcomedata, outcomedata[[failure]] !=hearFailureNA)
            
            ##Transform the column we are interested in from character to numeric in order to find the minimum value
            heartFailureClean[[failure]] = as.numeric(as.character(heartFailureClean[[failure]]))
            stateData <- subset(heartFailureClean, heartFailureClean$State == state)
            bestHosptialHF <- subset(stateData, stateData[[failure]]==min(stateData[[failure]]) )
            
            if(length(bestHosptialHF) > 1){
                first <- sort(bestHosptialHF$Hospital.Name)
                bestHospital <- first[1]
            } else{  
                first <- bestHosptialHF$Hospital.Name
                bestHospital[1]
            }
            return (bestHospital) 
        }
        else if(outcome == "pneumonia"){
            hearPneumoniaNA <- is.na(outcomedata[[pneumonia]]) ##logic vector for na's
            heartPneumoniaClean <- subset(outcomedata, outcomedata[[pneumonia]] !=hearPneumoniaNA)
            
            ##Transform the column we are interested in from character to numeric in order to find the minimum value
            heartPneumoniaClean[[pneumonia]] = as.numeric(as.character(heartPneumoniaClean[[pneumonia]]))
            stateDataPneumonia <- subset(heartPneumoniaClean, heartPneumoniaClean$State == state)
            bestHosptialHP <- subset(stateDataPneumonia, stateDataPneumonia[[pneumonia]]==min(stateDataPneumonia[[pneumonia]]) )
            bestHosptialHP$Hospital.Name
            
            if(length(bestHosptialHP) > 1){
                first <- sort(bestHosptialHP$Hospital.Name)
                bestHospital <- first[1]
            }
            else{  
                first <- bestHosptialHP$Hospital.Name
                bestHospital<- first[1]
            }
            return (bestHospital) 
        }
        else{
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
}

##Testing the function
##best("TX", "heart attack")
##best("TX", "heart failure")
##best("MD", "heart attack")
##best("MD", "pneumonia")

