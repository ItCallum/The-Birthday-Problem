## title: 'Formative Assignment: The Birthday Problem'
## author: "Callum Simpson"
## date: "19/10/2020"

## This originally was an r markdown document that I switch to pure R just incase

##------ Set up ----------

##Load in ggplot
library(ggplot2)
library("dplyr") 

#Disable exponential format
options(scipen=999)


##----- Functions ------

## In this section is the functions that will be used to check for duplicates 




##Part 1 (updated for part 3 - added a variable for Roomsize)

## A function for checking birthday duplicates give a certain RoomSize (room size being the number of people who are comparing birthdays)
contians_duplicates <- function(RoomSize) {
  
  ##creates a sample of birthdays 
  ## 1:365 used to repestent each day of the year
  bdaysample <- sample(1:365, RoomSize, replace=TRUE)
  
  ## In case you want to check that it works correctly the following will print out the vector of "brithdays" 
 ## print(bdaysample)
  
  ##If there was any duplicates return true, else return false
  return(any(duplicated(bdaysample)))
}


## Part 2 (updated for part 3 - added a variable for Roomsize)

## A function for estimating the probability of two people having the same birthday given how many people are in the room 
## Also takes in repetitions which the the number of time that the experiment gets ran
calculate_probability <- function(repetitions,RoomSize) {
  
  ##replicate the contains_duplicates function for the given room size for the inputted number of repetitions, store the results in results
  result <- replicate(n = repetitions, contians_duplicates(RoomSize))
  
 ## print(result)
  
  ##Calculate the probability by working out the sum of TRUE in result (so duplicates) then div by number of repetitions
  probality <- (sum(result, na.rm = TRUE)) / repetitions
  
  ##print(probality)
  
  return(probality)
}


## ----- Part 1 test ----

## A test of the contians_duplicates function for a room size of 10 

contians_duplicates(10)
contians_duplicates(10)
contians_duplicates(10)


## ---- Part 2 test ----

##calculate_probability(repetitions,RoomSize)
calculate_probability(10,10) 



## ---- Generate the graph for part 2 ----
## This is all the code need to generate my results for part 2


##Generate a list of powers of 10
N <- 10^(1:7)

##Generate a result table for the following experiment
##These results will be used to generate the plot 
create_table_Q2 <- function(repetitions,RoomSize){
  
  ##Create empty vectors that will be used for rows in a data table
  a = character(length(repetitions))
  b = numeric(length(repetitions))
  
  inc = 1
  # for each value in the repetitions function , record the calculated probability 
  
  for (i in repetitions){
    a[inc] <- i
    b[inc] <- calculate_probability(i,RoomSize)
 
    ## i was having trouble doing this inc in the for loop so i have have do it like this (not great but it works)   
    inc <- inc + 1
  }
 
  ##create a table of results
  table <- data.frame("N" = a , "Probability" = b)
  
  return(table)
  
}

##produce a table of probability for the given values of N
proabiltyTablep2<- create_table_Q2(N,10)
proabiltyTablep2

##Create a graph line
##ggplot(data=proabiltyTablep2, aes(x= N , y=Probability,group = 1)) +
##geom_line() + geom_point()+
##ggtitle("The Birthday Problem: Probability of duplicates for room size 10 by repetion") +
##xlab("The number of repetitions") + ylab("Probability")+
##geom_text(aes(label=Probability),hjust=0, vjust=-0.5)+
##theme(plot.title = element_text(hjust = 0.5))


##Create a graph bar
ggplot(data=proabiltyTablep2, aes(x= N , y=Probability)) +
  geom_bar(stat="identity") + 
  ggtitle("The Birthday Problem: Probability of duplicates for room size 10 by repetion") +
  xlab("The number of repetitions") + ylab("Probability")+
  geom_text(aes(label=Probability),hjust=0.5, vjust= -0.5)+
  theme(plot.title = element_text(hjust = 0.5))


## ---- Generate the graph for part 3 ----
## This is all the code need to generate my results for part 3

##create a vector of all numbers between 1 and 367
Room <- (1:366)

## used to create a table for part 3
## returns a table of the probabilities given a range of room sizes
create_table_Q3 <- function(N,RoomSize){
  
  a = numeric(length(RoomSize))
  b = numeric(length(RoomSize))
  
  inc = 1
  
  # loop through all roomsizes and record the probability of two people having the same birthday 
  for (i in RoomSize){
    a[inc] <- i
    b[inc] <- calculate_probability(N,i)
    
    inc <- inc + 1
  }
  
  table <- data.frame("N" = a , "Probability" = b)
  
  return(table)
  
}

##generate the dataframe used in the graph for part 3
proabiltyTablep3<- create_table_Q3(10000,Room)
proabiltyTablep3

##Make the graph
ggplot(data=proabiltyTablep3, aes(x= N , y=Probability,group = 1)) +
  geom_line() + 
  ggtitle("The Birthday Problem: Probability of duplicates by room size") +
  xlab("The room size") + ylab("Probability")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_segment(data=proabiltyTablep3, mapping=aes(x=0, y=0.5, xend=23, yend=0.5),  color="red") + 
  geom_segment(data=proabiltyTablep3, mapping=aes(x=23, y=0, xend=23, yend=0.5),  color="red") + annotate("text", x = 45 , y = 0.50, label = "X = 23",  color="red")

