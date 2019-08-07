## "rankall" returns a data frame containing the hospital in each state
## that has the ranking specified in num
## this R script will be making use of the dplyr package
library(dplyr)

rankall <- function(outcome, num = "best"){
        ## arguments:
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
        
        ## subset relevant parameters, filtering out NA values
        queryfields <- select(caremeasures, 2, 7, outcomecol)
        colnames(queryfields) <- c("Name", "State", "Rate")
        queryfields <- filter(queryfields, Rate != "NA")
        is.numeric(queryfields$Rate)
        
        ## split and rank data frame
        grouped <- queryfields %>%
                group_by(State) %>%
                arrange(Rate, Name)
        
        ## return hospital name with specified rank
        if(num == "best"){
                ranking <- 1L
                slice(grouped, ranking)
        }
        if(num == "worst"){
                slice(grouped, n())
        }
        
        else{
                slice(grouped, num)
        }
}