
#Summary

#Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in ???ve di???erent fashions: exactly according to the speci???cation (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the speci???ed execution of the exercise, while the other 4 classes correspond to common mistakes.
#Remove not needed columns


############################################################################################################################
#Reading the data
############################################################################################################################

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

############################################################################################################################
#Preprocessing
############################################################################################################################

library(caret)

#Only removing ones that are NA in training to avoid over fitting concerns
testing <- testing[, colSums(is.na(training)) != nrow(training)]

#Remove columns that are entirely NA since that adds no value
training <- training[, colSums(is.na(training)) != nrow(training)]


#Remove columns that are mainly 0. 
training <- training[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 

testing <- testing[, which(as.numeric(colSums(testing != 0)) > nrow(testing)/2)] 


#Create the data partitions 
inTrain <- createDataPartition(y=training$user_name,
                               p=0.75, list=FALSE)

training <- training[inTrain,]
validate <- training[-inTrain,]


###################################################################################################################
#Identify Principle Components
###################################################################################################################

prComp <- preProcess(training[,8:ncol(training)-1],method="pca", thresh = 0.99 )

trainPC <- predict(prComp,training[,8:ncol(training)-1])

validatePC <- predict(prComp,validate[,8:ncol(validate)-1])

testPC <- predict(prComp,testing[,8:ncol(testing)-1])


###################################################################################################################
#Create Random Forest model (with default parameters)  
###################################################################################################################

rf_default <- train(training$classe ~ ., method = "rf", data = trainPC, trControl = trainControl(method = "CV", number = 4), importance = TRUE)

predVal <- predict(rf_default,validatePC)

confusionMatrix(validate$classe,predVal)


###################################################################################################################
#Final Prediction
###################################################################################################################
 
predTestFinal <- predict(rf_default,testPC) 

predTestFinal
# [1] B A B A A E D B A A B C B A E E A B B B
# Levels: A B C D E

 

