


############################################################################################################################


# Summary

# Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in ???ve di???erent fashions: exactly according to the speci???cation (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the speci???ed execution of the exercise, while the other 4 classes correspond to common mistakes.
#Remove not needed columns

############################################################################################################################



############################################################################################################################

# Reading the data

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
# Plotting Predictors
############################################################################################################################
# library(caret)
# library(ggplot2)
# 
# #What does the training data look like?
# summary(training)
# 
# dim(training)
# 
# #How many in each classe?
# library(Hmisc)
# 
# cutTrain <- cut2(training$classe, g = 5)
# 
# table(cutTrain)
# 
# p1 <- qplot(cutTrain,user_name, data=training, fill=cutTrain, geom=c("boxplot"))
# 
# p2 <- qplot(cutTrain,user_name, data=training, fill=cutTrain, geom=c("boxplot","jitter"))
# 
# t1 <- table(cutTrain,training$classe)
# 
# t1
# 
# # Get proportions
# prop.table(t1,1)
# grid.arrange(p1,p2,ncol=2)
# 
# 
# #Density Plot
# 
# qplot(training$classe,color=training$classe,data=training,geom="density")

############################################################################################################################
#Preprocessing
############################################################################################################################
#Examples

#preOb <- preProcess(training[,-58],method=c("center","scale"))
#trainCapAveS <- predict(predObj,training[,-58])$capitalAve
#mean(trainCapAveS)
#sd(trainCapAves)

#testCapAveS <- predict(preObj,testing[,-58])$captialAve
#mean(testCapAveS)

# OR 

# set.seed(124)
# modelFit <- train(classe ~ ., data = training, 
#                   preProcess=c("center","scale"), metho="glm")


############################################################################################################################
#Imputing Data
#Center and Scale
#Remove zero variance columns
############################################################################################################################

#training[is.na(training)] <- NA

#Remove columns that are entirely NA since that adds no value
training <- training[, colSums(is.na(training)) != nrow(training)]

#ncol(training)

#Impute NA values with nearest value, and automatically centers and scales the data as well
preObj <- preProcess(training[,8:153],method=c("knnImpute"))  

library(RANN)

#add pre processed data to data frame
trainingImp <- predict(preObj,newdata = training[,8:153])

#join it back together to get metadata
trainingNew <-  cbind(training[,1:7],trainingImp,training[154])

# quantile(trainingNew[,8:153] - na.omit(training)[,8:153])


#These variables have zero variances so remove them from the data
training <- trainingNew[,-c(which(colnames(training)%in%c('amplitude_yaw_belt', 'amplitude_yaw_dumbbell', 'amplitude_yaw_forearm')))]



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

#Remove columns that are mainly 0.  I didn't have this included at first but decided to add it to speed up the processing 
#and see if the accuracy is still acceptable

############################################################################################################################

trainRows <- nrow(training)

#training[is.na(training)] <- 0

#Remove columns where more than half the values are zero
training <- training[, colSums(training != 0) > trainRows/2] 


# 
# ############################################################################################################################
# # Not currently using this section - would like to be able to, however.
# #Splitting the data into chunks based on user_name and classe.  The goal is to have a separate data object for each set. 
# #I want this because the classe is assigned at the set level and not the row of the data.
# #Also, for the rows that are NA because they are summary amounts, taking the average for that set and applying it to each row.
# 
# ############################################################################################################################
# 
# library(mlbench)
# 
# 
# #Break the data up into time-series chunks (30 element list, 6 users X 5 Classes..each element a set of 10 repetitions)
# #The splits are in order of user_name and classe (A-E)
# #Doing this, rather than just random sampling from the data, in order to keep related data together since 'classe' is assigned at the set level (and not rep).
# 
# set.seed(1000)
# 
# TrainChunks <- split( training , f = paste(training$user_name,training$classe ))
# 
# 
# startCol <- which( colnames(training)=="num_window") + 1
# 
# endCol <- ncol(training)-1
# 
# #Set the numeric columns to a mean value when they are NA in each chunk
# avgNAs <- function(Chunk){
# 
# 
#   
#   for(i in startCol: endCol){
#     
#     Chunk[is.na(Chunk[,i]), i] <- mean(Chunk[,i], na.rm = TRUE)
#  
#     }
#   
#   return(Chunk)
#   
# 
# }
# 
# TrainNew <- lapply(TrainChunks,avgNAs)
# 
# 
# 
# # Mix up the chunks randomly so they are not in order of user_name and Classe
# TrainNew <- sample(TrainNew)
# 
# 
# # Create groups for training and validating    
# groups = sample(c("train", "train1","validate"),
#                 size = length(TrainNew), replace = TRUE)
#  
# 
# # Split up the chunked data into groups, one for training and one for validation        
# t_split <- split(TrainNew,f=groups)
# 
# 
# 
# # Unsplit the groups assign the data back to a data frame 
# # Maybe a better way to do this but I don't know it
# 
# training <- do.call("rbind",t_split$train)
# 
# training1 <- do.call("rbind",t_split$train1)
# 
# training <- rbind(training,training1)
# 
# rm(training1)
# 
# validate <- do.call("rbind",t_split$validate)
# 



###################################################################################################################
#Create the data partitions 
###################################################################################################################

inTrain <- createDataPartition(y=training$user_name,
                               p=0.75, list=FALSE)

training <- training[inTrain,]
validate <- training[-inTrain,]

# Identify highly correlated variables
# M <- abs(cor(training[,8:150]))
# diag(M) <- 0
# which(M > .8, arr.ind = T)
# 
# prComp <- prcomp(training[,8:150])

# prComp$rotation
 

prComp <- preProcess(training[,8:150],method="pca", pcaComp = 12)

trainPC <- predict(prComp,training)

plot(trainPC[,1],trainPC[,2],col=trainPC$classe)



###################################################################################################################

#Pick attributes to use for model (Principle Component Analysis)

###################################################################################################################

#There are too many columns so need to improve this section before 
#running the model

startCol <- which( colnames(training)=="num_window") + 1 

endCol <- ncol(training)-1


###################################################################################################################

# Uncomment this section if you do not pair down the columns to remove mostly NA's

###################################################################################################################
#function to look for nan in data.frame, http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# 
# #Replace NaN with something really small  
# training[is.nan(training)] <- 0.00001

#The NaN values will throw an error if you don't handle them
# validate[is.nan(validate)] <- 0.00001
# 
# validate[is.na(validate)] <- 0

# otherwise you get an error that 'all arguments must have the same length'
#testing <- testing[, colSums(is.na(testing)) != nrow(testing)]
#testing[is.na(testing)] <- 0


###################################################################################################################
# Create Random Forest model (with default parameters)  
# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
###################################################################################################################

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"  #or RMSE for continuous?
set.seed(seed)
mtry <- sqrt(ncol(training))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(training$classe~., data=trainPC, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

print(rf_default$finalModel)

# An attempt at a random forest without altering any parameters (same as above since I am using the defaults)
# Building in the PCA to this training model
# Might need to add the transform to the data first (log 10 or boxcox)
rf_default <- train(training$classe~.,data=training, method="rf",preProcess="pca", prox = TRUE)

 


#predVal <- predict(rf_default,validate)

#table(validate$classe, predVal) 

confusionMatrix(validate$classe,predict(rf_default,validate))

#predVal

# Accuracy : 0.0771 


###################################################################################################################
# Trying Model Based Predictions 
###################################################################################################################

###########################################
# Boosting MOdels
#GBM
##########################################

#This model returns a very nice accuracy (97%)
modgbm <- train(training$classe ~ ., data = trainPC, method = "gbm", verbose=FALSE)

#predGBM <- predict(modgbm,validate)

#table(validate$classe, predGBM) 

confusionMatrix(validate$classe,predict(modgbm,validate))
# 
# Accuracy : 0.9792  

predGBM <- predict(modgbm,validate)

predGBM
 
# [1] A A A A A A A A A A A A A A A A A A A A
# Levels: A B C D E

##############################################
# Naive Bayes
#############################################

# modnb <- train(training$classe ~ ., data = training, method = "nb")
# 
# confusionMatrix(validate$classe,predict(modnb,validate))
# 
# # 
# # This model has a very poor Accuracy : 0.0314   


table(predVal,predGBM)



###################################################################################################################
#Can't run a confusion matrix against the testing data because the 'Classe' is not in that data set
###################################################################################################################
predict(rf_default,testing)
# [1] B A A A A A A A A D A A B A A A B A B B

predict(modgbm,testing)
# [1] A A A A A A A A A A A A A A A A A A A A
 

##Expected
##  [1] B A B A A E D B A A B C B A E E A B B B

