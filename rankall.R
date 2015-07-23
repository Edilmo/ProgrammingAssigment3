rank <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return  adata frame  with hospital name 
        ## in that state, the rate
        
        measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        states <- unique(measures$State)
        if(!(state %in% states)){
                stop("invalid state")
        }
        
        outcomelevels <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcomelevels)){
                stop("invalid outcome")
        }
        outcomecol <- factor(outcome, outcomelevels, labels=c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        outcomecol <- as.character(outcomecol)
        
        suppressWarnings(measures[, outcomecol] <- as.numeric(measures[, outcomecol]))
        
        rescol <- "Hospital.Name"
        
        observations <- subset(measures,(measures$State==state & !is.na(measures[outcomecol])), c(rescol,outcomecol))
        observations <- observations[order(observations[outcomecol],observations[rescol]),]
        observations
}

rankall <- function(outcome, num = "best"){
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        states <- unique(measures$State)
        
        outcomelevels <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcomelevels)){
                stop("invalid outcome")
        }
        outcomecol <- factor(outcome, outcomelevels, labels=c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        outcomecol <- as.character(outcomecol)
        
        suppressWarnings(measures[, outcomecol] <- as.numeric(measures[, outcomecol]))
        
        hnamecol <- "Hospital.Name"
        statecol <- "State"
        
        observations <- subset(measures,(!is.na(measures[outcomecol])), c(hnamecol,statecol,outcomecol))
        observationsOrder <- order(observations[statecol],observations[outcomecol],observations[hnamecol])
        observations <- observations[observationsOrder,]
        observationsParts <- split(observations,observations[statecol])
        
        
        selfunc <- function(obs){
                i <- NULL
                dftemp <- as.data.frame(obs)
                rowcount <- nrow(dftemp)
                
                if(is.numeric(num) && num > 0){
                        if(num <= rowcount)
                                i <- num
                        else
                                i <- NA
                }
                else if(is.character(num) && num == "best"){
                        i <- 1
                }
                else if(is.character(num) && num == "worst"){
                        i <- rowcount
                }
                else{
                        stop("invalid number")
                }
                
                if(!is.na(i))
                        dftemp[i,c(hnamecol,statecol)]
                else{
                        dftemp <- dftemp[1,c(hnamecol,statecol)]
                        dftemp[1,hnamecol] <- NA
                        dftemp
                }
        }
        observations <- sapply(observationsParts, selfunc)
        observations <- t(observations)
        colnames(observations) <- c("hospital","state")
        
        as.data.frame(observations)
}

rankhospital <- function(state, outcome, num = "best") {
        observations <- rank(state, outcome)
        
        i <- NULL
        rowcount <- nrow(observations)
        if(is.numeric(num) && num > 0){
                if(num <= rowcount)
                        i <- num
                else
                        i <- NA
        }
        else if(is.character(num) && num == "best"){
                i <- 1
        }
        else if(is.character(num) && num == "worst"){
                i <- rowcount
        }
        else{
                stop("invalid number")
        }
        
        if(is.na(i))
                NA
        else
                observations[i,1]
}

best <- function(state, outcome){
        ## Return hospital name in that state with lowest 30-day death
        ## rateout
        observations <- rank(state, outcome)
        observations[1,1]
}

test <- function(){
        r <- c()
        test <- best("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER"
        r <- c(r, best1 = test)
        test <- best("TX", "heart failure") == "FORT DUNCAN MEDICAL CENTER"
        r <- c(r, best2 = test)
        test <- best("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE"
        r <- c(r, best3 = test)
        test <- best("MD", "pneumonia") == "GREATER BALTIMORE MEDICAL CENTER"
        r <- c(r, best4 = test)
        test <- tryCatch(best("BB", "heart attack"), error = function(err) err$message == 'invalid state')
        r <- c(r, best5 = test)
        test <- tryCatch(best("NY", "hert attack"), error = function(err) err$message == 'invalid outcome')
        r <- c(r, best6 = test)
        
        test <- rankhospital("TX", "heart failure", 4) == "DETAR HOSPITAL NAVARRO"
        r <- c(r, rankhospital1 = test)
        test <- rankhospital("MD", "heart attack", "worst") == "HARFORD MEMORIAL HOSPITAL"
        r <- c(r, rankhospital2 = test)
        test <- is.na(rankhospital("MN", "heart attack", 5000))
        r <- c(r, rankhospital3 = test)
        
        r        
}