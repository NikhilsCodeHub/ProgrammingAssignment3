rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
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
    
    iOrder<-FALSE
    iRank<-1
    if (num=="best")
        iOrder<-FALSE
    else if(num=="worst")
        iOrder<- TRUE
    else {        
        iRank<-as.numeric(num)
        if (is.na(iRank)) {stop("invalid rank.")}
    }
    
    myfiledata <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=",", colClasses = "character")
    
    ## Filter data to required Outcome Columns
    outcome_data<-myfiledata[,c(2, 7, intOutcome)]
    
    ## Check if State Exists
    state<-toupper(state)
    if (any(outcome_data[,2]==state)==FALSE){
        stop("invalid state")
    }
    
    ## Filter data for required State
    outcome_data<-outcome_data[outcome_data[,2]==state,]
    
    ## Filter data for NA
    outcome_data<-outcome_data[outcome_data[,3]!="Not Available",]
    
    ## Convert data to Numeric
    outcome_data[,3]<-as.numeric(outcome_data[,3])
    
    outcome_data<-outcome_data[order(outcome_data[,3], outcome_data[,1] ,decreasing=iOrder),]
    
    outcome_data[iRank,1]

}