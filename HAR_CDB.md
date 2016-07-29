# <center>Practical Machine Learning Assignment</center>
<center><h3>**Qualitative Prediction of Dumbell Curl Exercises**<h3></center>
<center> <h4>*CB*</h4> </center>
<center> <h5>July 28, 2016</h5> </center>
 
### Overview



A group of 6 participants each performed a set of 10 repetitions using a small dumbell weight.  The repetitions were performed according to different specifications and assigned a 'classe' such that a participant in: 

* Classe A - Correctly performed the repetition
* Classe B - Threw the elbow to the front 
* Classe C - Lifted the dumbell only halfway
* Classe D - Lowered the dumbell only halfway
* Classe E - Threw the hips to the front
 
Using on-body sensing devices, time-series data were collected for 17 features including accelerometer, gyroscopic, and magnetometer.  The devices were used to measure acceleration, pitch, yaw, and roll of the belt, arm, and wrist during each dumbell repetition.

The exercise data are divided into two sets:

* > Training data - in which the 'classe' has been provided.
* > Testing data, - in which the 'classe' has been ommited. 


**The goal of this assignment is to constuct a model that will correctly predict the classe of a each dumbell repitition in the *Testing* data.** 

*** 




### Reading the data





The training data set contains **19,622** rows and **160** columns.  Upon a quick look at the distribution of *'classe'* in *(Appendix, Figure-1)*, it appears that the  counts of classe A > classe B > classe C > classe D > classe E.  If the testing data is a random sample (and large enough), we might expect a similar distribution.

*** 

#### Metadata Descriptions for Dimensions in the Training Data

The dimension found in the data are described in the table, below.


Field Name             Field Description       
---------------------  ------------------------
X                      Row Number              
user_name              Participant Name        
raw_timestamp_part_1   Date                    
raw_timestamp_part_2   Time (in milliseconds)  
cvtd_timestamp         Combined Date/Time      
new_window             Start of New Repetition 
num_window             Start of New Set        
classe                 Quality of Repetition   


####Preprocess the data

It often improves accuracy and reduces processing time to remove data that are:

* Completely 'NA'
* More than half of the rows are zero
* The data are correlated at a rate greater than 0.90


####Create Data Parition

The existing training data set was partitioned to simulate a new testing data set. A randomly selected 30% subset of original training set was allocated to create a *'validation'*  data set.  The validation set was used to test the model predictions before being applied to the testing data set, helping to prevent over-fitting of the model. 





After preprocessing and partitioning the data sets, the training data set now has '**50**' columns and '**13,737**' rows and the new validation data set has '**50**' columns and '**5,885**' rows.  The testing data set has '**50**' columns, with the rows count remaining unchanged at '**20**'.  It is worth mentioning that the columns removed from the testing data set were based upon conditions in the training data set (i.e. correlated, 'NA, and zero values). 

A correlation matrix *(Appendix, Figure-2)* shows the relative correlations between the attributes in the training data.


### Principle Component Analysis

An analysis of the most influential data in the training data set helped to identify important components, and reduce the model complexity. The principle component analyis was used to:

1. Identify subsets of the remaining columns that contribute most to the outcome ('classe') of a dumbbell repetition
2. Center and Scale the data to prevent differences in column data types from askewing the measurement contributions to the model
3. Reduce the complexity of the prediction model and improve processing time



The principle component analysis was able to capture '**0.99**' percent of the variance with just '**36**' components. A Variable Importance Plot *(Appendix, Figure-3)* shows the relative importance of these components, as well as the accuracy that would be lost in the model if that component was ommitted. A Classification Tree Plot  *(Appendix, Figure-4)* helps vizualize the rules for predicting outcomes based on the principle compoents. 

###Random Forest Prediction Model

Random Forest model was chosen for it's accuracy and ease of use in classification predictions.  In this model, there were 4 folds created and used in a cross-validation method.  


```
## Random Forest 
## 
## 13737 samples
##    35 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (4 fold) 
## Summary of sample sizes: 10304, 10302, 10302, 10303 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##    2    0.972     0.965  0.00203      0.00258 
##   19    0.966     0.957  0.00164      0.00207 
##   36    0.955     0.943  0.00501      0.00635 
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1671    2    1    0    0
##          B   26 1097   14    1    1
##          C    4   14 1001    7    0
##          D    0    0   43  920    1
##          E    0    1    4    3 1074
```

      
The overall accuracy of this simple model was '**0.97**' which is pretty good considering the original study produced 98.2% accuracy. See [Qualitative Activity Recognition of Weight Lifting Exercise](http://groupware.les.inf.puc-rio.br/public/papers/2012.Ugulino.WearableComputing.HAR.Classifier.RIBBON.pdf "Activity Recognition Study")
Remarkably, the accuracy of the prediction against the validation data was '**0.98**' with  '**0.02**' percent out of sample error.



```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
      
      
The final prediction against the testing data was '**BABAAEDBAABCBAEEABBB**'


###Executive Summary

Even a relatively simple Random Forest model appeared to be very effective at predicting the classe of the repetitions. Increasing the number of repeats and folds in the random forest showed some improvement but dramatically increased the processing time. Removing the highly correlated fields did not have a significant impact on the model accuracy in this case.  Another GBM (boosted model) was tried with similar results *(Addendum, R Code)*.  

Perhaps an interesting note is that despite the fact that the dumbell repetitions are measured in a time-series; the 'classe' is assigned to the entire repetition.  For example, it may be possible that a number of rows of the time-series data (measured in milliseconds) are preformed correctly (classe A), but because of a mistake later in the repetition, those rows received a lower classe.  Even with this potential issue, the prediction model worked well and didn't require a time-series analysis.





## Appendix


![](Figs/chunk6-1.png)


<!--html_preserve--><div id="htmlwidget-668" style="width:1152px;height:768px;" class="iplotCorr html-widget"></div>

![](Figs/chunk8-1.png)![](Figs/chunk8-2.png)









### Addenedum
####R Code 


```r
############################################################################################################################
#Reading the data
############################################################################################################################

ActPred <- function(i) {
  
  setwd("C://R//MachineLearning//Assignment")
  
  tryCatch(
    
    {
      #Check whether you have already dowloaded the data into the working director, and if not, download and unzip it 
      
      #message("Checking local drive for data file. If it's not there then trying URL for new file.")
      
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
```



```r
############################################################################################################################
#Preprocessing
############################################################################################################################

library(caret)

#Remove the first 7 columns as they are not used in this analysis
training[1:7] <- list(NULL) 
testing[1:7] <- list(NULL)


#Remove columns in testing and training that are entirely NA (in training)  
testing <- testing[, colSums(is.na(training)) != nrow(training)]

training <- training[, colSums(is.na(training)) != nrow(training)]


#Remove columns in testing and training that are mainly 0 (in training)  
testing <- testing[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 

training <- training[, which(as.numeric(colSums(training != 0)) > nrow(training)/2)] 



#Look at fields with at least .9 correlation
corFields <- findCorrelation(cor(training[,1:ncol(training)-1]), names = TRUE, cutoff = .90, verbose = FALSE)
#corFields
removeCor <- c("accel_belt_z","accel_belt_y","gyros_dumbbell_x")

# [1] "accel_belt_z"     "roll_belt"        "accel_belt_y"     "accel_belt_x"     "gyros_dumbbell_x"
# [6] "gyros_dumbbell_z" "gyros_arm_x"

#Keep one of each of the correlated pairs of fields and remove the others
training <- training[,!(names(training) %in% removeCor)]
testing <- testing[,!(names(testing) %in% removeCor)]


#Create the data partitions for training and validation based on the orginal training data set
inTrain <- createDataPartition(y=training$classe,
                               p=0.70, list=FALSE)

trainNew <- training[inTrain,]
valNew <- training[-inTrain,]
```
  
  
  


```r
###################################################################################################################
#Identify Principle Components
###################################################################################################################

#Estimate Principle Components (PCA) based on training data, as well as centering/scaling the data
prComp <- preProcess(trainNew[,1:ncol(trainNew)-1],method="pca", thresh = 0.99)

#Calculate Principle Compoent values for training data
trainPC <- predict(prComp,trainNew[,1:ncol(trainNew)-1])

#Calculate Principle Component values for validate data, based on training PCA's
validatePC <- predict(prComp,valNew[,1:ncol(valNew)-1])

#Calculate Principle Component values for test data, based on training PCA's
testPC <- predict(prComp,testing[,1:ncol(testing)-1])
```



```r
###################################################################################################################
#Create Random Forest model 
###################################################################################################################

#Create Random Forest Model with Cross Validation method with 4 folds.
#Normally use something like (method="repeatedcv", number=10, repeats=3) for repeated
#But it takes a long time  to process and doesn't improve accuracy that much for this model

rf_default <- train(trainNew$classe ~ ., method = "rf", data = trainPC, trControl = trainControl(method = "CV", number = 4), importance = T)


#97.1% accuracy, compared to 98.2% in the actual study
rf_default

#Predict values in validation data based on Random Forest Model Created
predVal <- predict(rf_default,validatePC)

#Show confusion matrix for the predictions to check accuracy and error rates
predValConf <- confusionMatrix(valNew$classe,predVal)

predValConf$table
#Show accuracy for the validation predictions
#predValConf$overall["Accuracy"]
```
  
  
  


```r
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
```




```r
#Quick look at the training data classes by density
qplot(training$classe,color=training$classe,data=training,geom="density", main = "Distribution of Classe (Figure-1)")
  

#Might be interesting to see if the predictions reflect the relative distributions of this plot
#such that the counts of A > B > C > D > E.
```



```r
#Graph correlation matrix for attributes in training data set
#Lots of strong correlations between roll belt and acceleration belts
library(qtlcharts)

iplotCorr(trainNew[,1:ncol(trainNew)-1], reorder=TRUE,chartOpts=list(cortitle="Correlation matrix (Figure-2)",
                         scattitle="Scatterplot"))
```



```r
#The more predictors the less accuracy?
#plot(rf_default, metric = "Accuracy")


# Graph Importance of principle components 
varImpPlot(rf_default$finalModel, sort = TRUE, type = 1, pch = 19, col = 1, cex = 1,
           main = "Variable Importance Plot (Figure -3)")

#https://dinsdalelab.sdsu.edu/metag.stats/code/trees.html
library(tree)
rf.tree <- tree(trainNew$classe~., trainPC[,1:35])
rf.tree.best <- prune.tree(rf.tree, best=10)
plot(rf.tree.best)
title(main="Decision Tree - (Figure - 4)")
text(rf.tree.best, cex=.5)
#dev.off()
```



```r
 ###########################################
 # Boosting MOdels - GBM
 ##########################################
 
 #This model returns a very nice accuracy (97%)

 # modgbm <- train(training$classe ~ ., data = training, method = "gbm", verbose=FALSE)
 
 #confusionMatrix(validate$classe,predict(modgbm,validate))
 
 # Accuracy : 0.9792  
```




```r
############################################################################################################################
# Not currently used but in case you want to use the timestamp data
# Preserving the timestamp data.  
# Had to go through all of these steps because for some reason I couldn't just combine the two timestamps to get the valid date info

############################################################################################################################

#Make milliseconds display
# options("digits.secs"=7)
# 
# #Convert Milliseconds
# training$raw_timestamp_part_2 <- training$raw_timestamp_part_2/1000000
# 
# 
# #Convert timestamps 
# training$cvtd_timestamp <-  
#   
#   as.POSIXct(
#     
#     strftime(
#       
#       strptime(
#         
#         format(as.POSIXlt(training$raw_timestamp_part_1, origin="1970-01-01")
#                , tz="GMT",usetz = TRUE),
#         
#         format="%Y-%m-%d%H:%M:%S")
#       
#       + (training$raw_timestamp_part_2),format="%Y-%m-%d %H:%M:%OS6"),
#     
#     "GMT"
#   )
# 
# #Not sure what else to do here besides convert the test data timestamps as well
# testing$raw_timestamp_part_2 <- testing$raw_timestamp_part_2/1000000
# 
# #Convert timestamps 
# testing$cvtd_timestamp <-  
#   
#   as.POSIXct(
#     
#     strftime(
#       
#       strptime(
#         
#         format(as.POSIXlt(testing$raw_timestamp_part_1, origin="1970-01-01")
#                , tz="GMT",usetz = TRUE),
#         
#         format="%Y-%m-%d%H:%M:%S")
#       
#       + (testing$raw_timestamp_part_2),format="%Y-%m-%d %H:%M:%OS6"),
#     
#     "GMT"
#   )
```



```r
# ############################################################################################################################
# # Not currently using this section - would like to be able to, however.
# #Splitting the data into chunks based on user_name and classe.  The goal is to have a separate data object for each set. 
# #I want this because the classe is assigned at the set level and not the row of the data.
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
```
