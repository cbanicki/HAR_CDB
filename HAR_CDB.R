
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
#training <- training[,-c(1:7)]

#dim(training)



#Replace all zeroes with NA to make sure they are not evaluated and to help remove columns that don't add value
#Remove columns that are entirely NA since that adds no value

#training[training == 0] <- NA

#Remove columns that are entirely NA since that adds no value
training <- training[, colSums(is.na(training)) != nrow(training)]


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

#There is a problem here that if all of the values for a chunk are NA this is converted them to NaN.  
#If I set them to 0 then it will throw off the calcs by biasing them towards zero

#training[training == NaN] <- 0



rm(training1)

validate <- do.call("rbind",t_split$validate)

#validate[validate == NaN] <- 0

#Remove columns with constant variances (zero) because they will stop you from doing PCA

#training <- training[,apply(training[,startCol:endCol], 2, var, na.rm=TRUE) != 0]

###################################################################################################################

#Pick attributes to use for model (Principle Component Analysis)

###################################################################################################################

training[training == 0] <- NA

#Remove columns that are entirely NA since that adds no value
training <- training[, colSums(is.na(training)) != nrow(training)]

#Have to rerun this
startCol <- which( colnames(training)=="num_window") + 1

endCol <- ncol(training)-1

# Having issues getting the principle components from this data.  x must be numeric, probably the NA's 


# training.2 <- data.frame(t(na.omit(t(training))))
# 
# pca.training.2 <- prcomp(training.2[,3:35], retx=TRUE)
# 
# prComp <-prcomp(training[,startCol:endCol])
# 
# preProc <- preProcess(training[,startCol:endCol], method="pca",pcaComp=2)
# 
# trainingPC <- predict(preProc, log10(training[,startCol:endCol]))
# 
# plot(preProc$x[,1],preProc$x[,2],col=typeColor,xlab="PC1",ylab="PC2")
