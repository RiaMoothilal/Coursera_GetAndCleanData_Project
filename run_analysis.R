#read in data files
subject_test <- read.table ("./UCI HAR Dataset/test/subject_test.txt", sep = "", header = FALSE)
X_test <- read.table ("./UCI HAR Dataset/test/X_test.txt", sep ="", header = FALSE)
y_test <- read.table ("./UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE)
subject_train <- read.table ("./UCI HAR Dataset/train/subject_train.txt", sep = "", header = FALSE)
X_train <- read.table ("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
y_train <- read.table ("./UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE)

#merge the test data into one df
testdata <- cbind(subject_test,y_test,X_test)

#merge the training data into one df
traindata <- cbind(subject_train,y_train,X_train)

#merge training and test data into one df
data <- rbind(testdata,traindata)

#read in the features text file to get the header names
featuresnames <- read.table ("./UCI HAR Dataset/features.txt")

#create vector with list of feature names
#remove parethesis, and - from string names
fnames <- gsub("()","_",featuresnames[,2], fixed = TRUE)
fnames <- gsub("-","_",fnames, fixed = TRUE)
fnames <- gsub("(","_",fnames, fixed = TRUE)
fnames <- gsub(")","_",fnames, fixed = TRUE)
fnames <- gsub(",","_",fnames, fixed = TRUE)

fnames <- featuresnames[,2]

#create vector with heading names
hnames <- cbind("subject","activity")
for (i in 1:561) {
              hnames <- cbind(hnames,fnames[i])  
}

#Set the heading names for the data frame
colnames(data) <- hnames

#find columns where heading contains the words "mean" or "std"
z<- ""
for (i in 1:563) {
              x <- ""
              y <- ""
              Want <- 0
              x <- grep("mean",hnames[i], fixed = TRUE) 
              y <- grep("std",hnames[i], fixed = TRUE)
              j <- length(x)+length(y)
              if (j > 0) {
                  Want <- 1
              }
             
              if (Want == 1) {
                  z <- c(z,i)
              }
}

#create subset of data
for (i in 2:80) {
      j[i-1] <- z[i]
}

#coerce j into numeric vector
k <- as.numeric(j)

#Make new dataframe with only means or std dev
Data2 <- data[,c(1,2,k)]
