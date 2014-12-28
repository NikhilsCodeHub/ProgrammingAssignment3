rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    
    ## Initialize outcome values
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
    
    ## Initialize and set sort order and Rank Number
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
    
    ## Read data from file
    myfiledata <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=",", colClasses = "character")
    
    ## Filter data to required Outcome Columns
    outcome_data<-myfiledata[,c(2, 7, intOutcome)]
    
    ## Filter data for NA
    outcome_data<-outcome_data[outcome_data[,3]!="Not Available",]
    
    ## Convert data to Numeric
    outcome_data[,3]<-as.numeric(outcome_data[,3])
    
    ## Order the data to Rank each Hospital in the dataset.
    outcome_data<-outcome_data[order(outcome_data[,2],outcome_data[,3], outcome_data[,1] ,decreasing=iOrder),]
    
    ## Split data for required State
    outcome_data<-split(outcome_data, outcome_data$State)
    
    ## lapply over each state's data
    result<-lapply(outcome_data,function(x) {
        x<-x[order(x[,2],x[,3], x[,1] ,decreasing=iOrder),]
        c(hospital=x[iRank,1], state=x[iRank,2])
        })
    
    ## recombine result to dataframe
    result<-data.frame(t(sapply(result,c)))
    
    ## Copy RowNames to fill missing state values
    result[,2]<-rownames(result)
    
    result

}