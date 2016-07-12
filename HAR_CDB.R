
ActPred <- function() {
  
  setwd("C://R//MachineLearning//Assignment")
  
  tryCatch(
    
    {
      #Check whether you have already dowloaded the data into the working director, and if not, download and unzip it 
      
      message("Checking local drive for data file. If it's not there then trying URL for new file.")
      
      if (!file.exists("pml-training.csv")) {
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        
        fileName <- "pml-training.csv"
        
        download.file(fileURL, fileName, mode = "wb")
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        
        fileName <- "pml-testing.csv"
        
        download.file(fileURL, fileName, mode = "wb")
        
        dateDownloaded <- date()
        
      }
      
    },
    error=function(cond) {
      message(paste("Error getting data:", fileURL))
      message("Original Error Message:")
      message(cond)
      # Return NA
      stop("Check connection and try again.")
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning getting data", fileURL))
      message("Original warning message:")
      message(cond)
    },
    
    finally={
      
      ## Read tracking data and remove NA
      training <- read.csv("pml-training.csv", header = TRUE,na.strings = c("NA","#DIV/0!",""))
      
      testing <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA","#DIV/0!",""))
      
    }
    
  )
  
}



# Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in ???ve di???erent fashions: exactly according to the speci???cation (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the speci???ed execution of the exercise, while the other 4 classes correspond to common mistakes.
#Remove not needed columns
#training <- training[,-c(1:7)]

dim(training)

#Remove columns that are entirely NA
training <- training[, colSums(is.na(training)) != nrow(training)]


# for(i in 8:153){
#   training[is.na(training[,i]), i] <- mean(training[,i], na.rm = TRUE)
# }


############################################################################################################################

# Preserving the timestamp data 

############################################################################################################################

#Make milliseconds display
options("digits.secs"=7)

#Convert Milliseconds
training$raw_timestamp_part_2 <- training$raw_timestamp_part_2/1000000

#Convert timestamps 
training$cvtd_timestamp <-  
  
  as.POSIXct(
    
    strftime(
      
      strptime(
        
        format(as.POSIXlt(training$raw_timestamp_part_1, origin="1970-01-01")
               , tz="GMT",usetz = TRUE),
        
        format="%Y-%m-%d%H:%M:%S")
      
      + (training$raw_timestamp_part_2),format="%Y-%m-%d %H:%M:%OS6"),
    
    "GMT"
  )


library(mlbench)
library(caret)

#Break the data up into time-series chunks (30 element list, 6 users X 5 Classes..each element a set of 10 repetitions)
#The splits are in order of user_name and classe (A-E)
#Doing this, rather than just random sampling from the data, in order to keep related data together since 'classe' is assigned at the set level (and not rep).

set.seed(1000)

TrainChunks <- split( training , f = paste(training$user_name,training$classe ))


#Set the numeric columns to a mean value when they are NA in each chunk
avgNAs <- function(TrainChunks)
{
  for(i in 8:153){
    
    TrainChunks[is.na(TrainChunks[,i]), i] <- mean(TrainChunks[,i], na.rm = TRUE)
    
    
    
  }
  
  return(TrainChunks)
}



TrainNew <- lapply(TrainChunks,avgNAs)

# 
# for(i in 8:159){
#   training[is.na(training[,i]), i] <- mean(training[,i], na.rm = TRUE)
# }


# Mix up the chunks randomly so they are not in order of user_name and Classe
#    TrainChunks <- sample(TrainChunks)

# Create groups for training and validating    
groups = sample(c("train", "validate"),
                size = length(TrainChunks), replace = TRUE)

# Split up the chunked data into groups, one for training and one for validation        
t_split <- split(TrainChunks,f=groups)

# Unsplit the groups assign the data back to a data frame        
training <- do.call("rbind",t_split$train)

validate <- do.call("rbind",t_split$validate)




