## "rankhospital" finds the name of the hospital with a particular rank
## for the specified outcome in that state
## this R script will be making use of the dplyr package
library(dplyr)

rankhospital <- function(state, outcome, num = "best"){
        ## arguments:
        ## state = abbreviated name of a state to query from
        ## outcome = a specified condition to query outcomes from
        ## note:        [,11] = heart attack
        ##              [,17] = heart failure
        ##              [,23] = pneumonia
        ## num = ranking of a hospital in that state for a given outcome
        
        ## read outcome data
        caremeasures <- read.csv("outcome-of-care-measures.csv",
                                 header = TRUE,
                                 na.strings = "Not Available",
                                 stringsAsFactors = FALSE)
        caremeasures <- tbl_df(caremeasures)
        
        ## check that outcome are valid
        validoutcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        if(outcome %in% names(validoutcomes)){
                outcomecol <- validoutcomes[outcome]
        }
        else{
                print("invalid outcome")
                stop()
        }
        
        ## check that state is valid
        if(state %in% caremeasures$State == FALSE){
                print("invalid state")
                stop()
        }
        
        ## subset and rank relevant parameters, filtering out NA values
        queryfields <- caremeasures %>%
                select(2, 7, outcomecol) %>%
                filter(State == state)
        
        colnames(queryfields) <- c("Name", "State", "Rate")
        queryfields <- filter(queryfields, Rate != "NA")
        is.numeric(queryfields$Rate)
        ranked <- arrange(queryfields, Rate, Name)
        
        ## return hospital name with specified rank
        if(num == "best"){
                print(as.character(ranked[1,1]))
        }
        if(num == "worst"){
                print(as.character(ranked[nrow(ranked),1]))
        }
        else{
                print(as.character(ranked[num,1]))
        }
        
}