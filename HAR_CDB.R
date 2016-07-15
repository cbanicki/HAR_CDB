
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
      
      ## Read tracking data 
      training <- read.csv("pml-training.csv", header = TRUE,na.strings = c("NA","#DIV/0!",""))
      
      testing <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA","#DIV/0!",""))
      
    }
    
  )
  
}

# Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in ???ve di???erent fashions: exactly according to the speci???cation (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the speci???ed execution of the exercise, while the other 4 classes correspond to common mistakes.
#Remove not needed columns


#Replace all zeroes with NA to make sure they are not evaluated and to help remove columns that don't add value
#training[training == 0] <- NA

#Remove columns that are entirely NA since that adds no value
#training <- training[, colSums(is.na(training)) != nrow(training)]

#Replace NA with zero 
#training[is.na(training)] <- 0

############################################################################################################################

# Preserving the timestamp data.  
# Had to go through all of these steps because for some reason I couldn't just combine the two timestamps to get the valid date info

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

#Not sure what else to do here besides convert the test data timestamps as well
testing$raw_timestamp_part_2 <- testing$raw_timestamp_part_2/1000000

#Convert timestamps 
testing$cvtd_timestamp <-  
  
  as.POSIXct(
    
    strftime(
      
      strptime(
        
        format(as.POSIXlt(testing$raw_timestamp_part_1, origin="1970-01-01")
               , tz="GMT",usetz = TRUE),
        
        format="%Y-%m-%d%H:%M:%S")
      
      + (testing$raw_timestamp_part_2),format="%Y-%m-%d %H:%M:%OS6"),
    
    "GMT"
  )


############################################################################################################################

#Splitting the data into chunks based on user_name and classe.  The goal is to have a separate data object for each set. 
#I want this because the classe is assigned at the set level and not the row of the data.
#Also, for the rows that are NA because they are summary amounts, taking the average for that set and applying it to each row.

############################################################################################################################

library(mlbench)
library(caret)

#Break the data up into time-series chunks (30 element list, 6 users X 5 Classes..each element a set of 10 repetitions)
#The splits are in order of user_name and classe (A-E)
#Doing this, rather than just random sampling from the data, in order to keep related data together since 'classe' is assigned at the set level (and not rep).

set.seed(1000)

TrainChunks <- split( training , f = paste(training$user_name,training$classe ))


startCol <- which( colnames(training)=="num_window") + 1

endCol <- ncol(training)-1

#Set the numeric columns to a mean value when they are NA in each chunk
avgNAs <- function(Chunk){


  
  for(i in startCol: endCol){
    
    Chunk[is.na(Chunk[,i]), i] <- mean(Chunk[,i], na.rm = TRUE)
 
    }
  
  return(Chunk)
  

}

TrainNew <- lapply(TrainChunks,avgNAs)



# Mix up the chunks randomly so they are not in order of user_name and Classe
TrainNew <- sample(TrainNew)


# Create groups for training and validating    
groups = sample(c("train", "train1","validate"),
                size = length(TrainNew), replace = TRUE)
 

# Split up the chunked data into groups, one for training and one for validation        
t_split <- split(TrainNew,f=groups)


##################################################################################################################

#Should I train a model on each of the groups now and then combine them?  Maybe that way it can be a ts analysis?

##################################################################################################################


# Unsplit the groups assign the data back to a data frame 
# Maybe a better way to do this but I don't know it

training <- do.call("rbind",t_split$train)

training1 <- do.call("rbind",t_split$train1)

training <- rbind(training,training1)

rm(training1)

validate <- do.call("rbind",t_split$validate)



###################################################################################################################

#Pick attributes to use for model (Principle Component Analysis)

###################################################################################################################

#Week 2 - Preprocessing with principle components analysis

#Remove columns that are entirely NA since that adds no value
# training <- training[, colSums(is.na(training)) != nrow(training)]

#Remove columns that are entirely 0 since that adds no value
#training <- training[, colSums(is.na(training)) != nrow(training)]

startCol <- which( colnames(training)=="num_window") + 1

endCol <- ncol(training)-1

#function to look for nan in data.frame, http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#Replace NaN with something really small  
training[is.nan(training)] <- 0.00001




# Create model with default paramters  
# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(training))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(training$classe~., data=training, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

#The NaN values will throw an error if you don't handle them
validate[is.nan(validate)] <- 0.00001


confusionMatrix(validate$classe,predict(rf_default,validate))



# otherwise you get an error that 'all arguments must have the same length'
#testing <- testing[, colSums(is.na(testing)) != nrow(testing)]
testing[is.na(testing)] <- 0


#There are WAY more variables in the training data set then there are in the testing one.  
predict(rf_default,testing)

#THIS IS CAUSING AN ERROR 'all arguments must have the same length'
confusionMatrix(training$classe,predict(rf_default,testing))

#setdiff(names(training),names(testing))



# 
# # Have to use the code below to get around the NaN and NA values 
# preProc <- prcomp(~ ., data=training[,startCol:endCol], center = TRUE, scale=TRUE, na.action = na.omit)
# 
# #preProc <- prcomp(na.omit(training), center = TRUE, scale = TRUE)
# trainingPC <- predict(preProc, training[,startCol:endCol])
# 
#  
# #plot(preProc$x[,1],preProc$x[,2],xlab="PC1",ylab="PC2", col=training$classe)
# 
# #library(e1071)
# 
# modelFit <- train(training$classe ~ ., method = "rf",data = trainingPC)
# 
# finMod <- modelFit$finalModel
# 
# 
# testPC <- predict(preProc,validate[,startCol:endCol])
# 
# confusionMatrix(validate$classe,predict(rf_default,validate))
# 
# 
# names(validate[,startCol:endCol])
# names(training[,startCol:endCol])
