## Output : Hospital Name with lowest 30-day death for input outcome.
## Input : State 2digit code, Outcome

## [7] "State" 
## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 



best <- function(state, outcome="heart attack") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    intOutcome<-0
    
    if (outcome=="heart attack")
        intOutcome<-11
    else if (outcome=="heart failure")
        intOutcome<-17
    else if (outcome=="pneumonia")
        intOutcome<-23
    else{
        ## Throw Error
        stop("invalid outcome")
        ##return("Error: Use one of the following inputs : heart attack, heart failure, pneumonia")
    }
    
    
    myfiledata <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=",", colClasses = "character")
    
    ## Filter data to required Outcome Columns
    outcome_data<-myfiledata[,c(2, 7, intOutcome)]
    
    ## Filter data for required State
    outcome_data<-outcome_data[outcome_data[,2]==state,]

    ## Filter data for NA
    outcome_data<-outcome_data[outcome_data[,3]!="Not Available",]
    
    ## Convert data to Numeric
    outcome_data[,3]<-as.numeric(outcome_data[,3])
    ##outcome_data<-outcome_data[complete.cases(outcome_data),] 
    
    ## Output the Hospital Name
    outcome_data[outcome_data[,3]==min(outcome_data[,3],na.rm=TRUE),1]
}