
#Summary

#Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in ???ve di???erent fashions: exactly according to the speci???cation (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the speci???ed execution of the exercise, while the other 4 classes correspond to common mistakes.
#Remove not needed columns


# You should create a report describing how you built your model, 
#how you used cross validation, 
#what you think the expected out of sample error is

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

#Remove columns in testing and training that are entirely NA (in training)  
testing <- testing[, colSums(is.na(training)) != nrow(training)]

training <- training[, colSums(is.na(training)) != nrow(training)]


#Remove columns in testing and training that are mainly 0 (in training)  
testing <- testing[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 

training <- training[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 


#Create the data partitions for training and validation based on the orginal training data set
inTrain <- createDataPartition(y=training$user_name,
                               p=0.75, list=FALSE)

training <- training[inTrain,]
validate <- training[-inTrain,]


#Graph correlation matrix for attributes in training data set
library(qtlcharts)

iplotCorr(training[,8:37], reorder=TRUE)



###################################################################################################################
#Identify Principle Components
###################################################################################################################

#Estimate Principle Components (PCA) based on training data
prComp <- preProcess(training[,8:ncol(training)-1],method="pca")

#Calculate Principle Compoent values for training data
trainPC <- predict(prComp,training[,8:ncol(training)-1])

#Calculate Principle Component values for validate data, based on training PCA's
validatePC <- predict(prComp,validate[,8:ncol(validate)-1])

#Calculate Principle Component values for test data, based on training PCA's
testPC <- predict(prComp,testing[,8:ncol(testing)-1])


###################################################################################################################
#Create Random Forest model 
###################################################################################################################

#Create Random Forest Model with Cross Validation method with 4 folds.
#Normally use something like (method="repeatedcv", number=10, repeats=3) for repeated
#But it takes a long time  to process and doesn't improve accuracy that much for this model
rf_default <- train(training$classe ~ ., method = "rf", data = trainPC, trControl = trainControl(method = "CV", number = 4), importance = T)


#The more predictors the less accuracy?
plot(rf_default, metric = "Accuracy")

#97.1% accuracy, compared to 98.2% in the actual study
rf_default

#Predict values in validation data based on Random Forest Model Created
predVal <- predict(rf_default,validatePC)

#Show confusion matrix for the predictions to check accuracy and error rates
predValConf <- confusionMatrix(predVal, validate$classe)

#Show accuracy for the validation predictions
predValConf$overall["Accuracy"]

# Graph Importance of principle components 
varImpPlot(rf_default$finalModel, sort = TRUE, type = 1, pch = 19, col = 1, cex = 1,
           main = "Variable Importance Plot")



#https://dinsdalelab.sdsu.edu/metag.stats/code/trees.html
library(tree)
rf.tree <- tree(training$classe~., trainPC[,1:25])
rf.tree.best <- prune.tree(rf.tree, best=10)
plot(rf.tree.best)
text(rf.tree.best, cex=.5)
#dev.off()


###################################################################################################################
#Final Prediction
###################################################################################################################
 
predTestFinal <- predict(rf_default,testPC) 

predTestFinal
# [1] B A B A A E D B A A B C B A E E A B B B
# Levels: A B C D E
 

