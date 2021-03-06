
#Summary

#Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in ???ve di???erent fashions: exactly according to the speci???cation (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the speci???ed execution of the exercise, while the other 4 classes correspond to common mistakes.
#Remove not needed columns


# You should create a report describing how you built your model, 
#how you used cross validation, 
#what you think the expected out of sample error is

############################################################################################################################
#Reading the data
############################################################################################################################
ActPred <- function(i) {
  
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
      
      ifelse(i==1, 
             ## Read tracking data 
             testTrain <- read.csv("pml-training.csv", header = TRUE,na.strings = c("NA","#DIV/0!","")),
             
             testTrain <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA","#DIV/0!",""))
      )
      
      
      return(testTrain)
      
    }
    
  )
  
}

training <- ActPred(1)

testing <- ActPred(0)


library(knitr)

Desc <- c("Row Number","Athlete","Date", "Time (in milliseconds)", "Combined Date/Time", "Start of New Repetition",
          "Start of New Set", "Quality of Repetition")

Field <- c(names(training[1:7]),names(training[160]))

FieldNames <- names(training[names(training) %in% Field])

Field_name <- "Field Name"

Desc_name <- "Field Description"

df <- data.frame(FieldNames,Desc)

names(df) <- c(Field_name,Desc_name )

kable(df, digits=2)

############################################################################################################################
#Preprocessing
############################################################################################################################

library(caret)


metaData <- c("Row Number","Athlete","Date", "Time (milliseconds)", "Combined Date/Time", "Start of New Repitition",
              "Start of New Set")


#Remove columns in testing and training that are entirely NA (in training)  
testing <- testing[, colSums(is.na(training)) != nrow(training)]

training <- training[, colSums(is.na(training)) != nrow(training)]


#Remove columns in testing and training that are mainly 0 (in training)  
testing <- testing[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 

training <- training[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 



#Quick look at the training data classes by density
qplot(training$classe,color=training$classe,data=training,geom="density")

#Might be interesting to see if the predictions reflect the relative distributions of this plot
#such that the counts of A > B > C > D > E.

#Create the data partitions for training and validation based on the orginal training data set
inTrain <- createDataPartition(y=training$user_name,
                               p=0.75, list=FALSE)

training <- training[inTrain,]
validate <- training[-inTrain,]


#Graph correlation matrix for attributes in training data set
#Lots of strong correlations between roll belt and acceleration belts
library(qtlcharts)

iplotCorr(training[,8:37], reorder=TRUE)

#Look at fields with at least .9 correlation
corFields <- findCorrelation(cor(training[,8:59]), names = TRUE, cutoff = .90, verbose = FALSE)
corFields
removeCor <- c("accel_belt_z","accel_belt_y","gyros_dumbbell_x")

# [1] "accel_belt_z"     "roll_belt"        "accel_belt_y"     "accel_belt_x"     "gyros_dumbbell_x"
# [6] "gyros_dumbbell_z" "gyros_arm_x"

#Keep one of each of the correlated pairs of fields and remove the others
training <- training[,!(names(training) %in% removeCor)]
testing <- testing[,!(names(testing) %in% removeCor)]

###################################################################################################################
#Identify Principle Components
###################################################################################################################

#Estimate Principle Components (PCA) based on training data, as well as centering/scaling the data
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

#7 A 
#8 B 
#1 C
#1 D
#3 E
 

# #Density Plot
# 
  qplot(training$classe,color=training$classe,data=training,geom="density")

