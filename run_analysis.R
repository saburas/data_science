### Establish initial environment setup
setwd("C:/Temp/R/Data/UCI HAR Dataset")
library(dplyr)


### 1. Merges the training and the test sets to create one data set.

# Load features and activity data
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")

# Load training data
subject_train <- read.table("train/subject_train.txt")
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

# Load testing data
subject_test <- read.table("test/subject_test.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

# Append training and testing data
subject_combine <- rbind(subject_train,subject_test)
x_combine <- rbind(x_train,x_test)
y_combine <- rbind(y_train,y_test)

# Name and label columns of the 3 datasets
colnames(x_combine) <- t(features[2])
colnames(y_combine) <- "Activity"
colnames(subject_combine) <- "Subject"

# Merge all data sets into one data set
all_combine <- cbind(subject_combine,y_combine,x_combine)

### 2.Extracts only the measurements on the mean and standard deviation for each measurement.

# Extract mean columns, Use \\ to include special characters like '(' and ')'
mean_cols <- grep("mean\\(", colnames(all_combine))
# Extract standard deviation columns
std_cols  <- grep("std\\(", colnames(all_combine))
# Combine mean and standard deviation columns
mean_std_cols <- c(mean_cols, std_cols)
# Combine column list with Subject (1) and Activity (2) Columns
required_cols <- c(1,2,mean_std_cols)

# Extract only required columns into new data set

analysis_data <- all_combine[,required_cols]

### 3. Uses descriptive activity names to name the activities in the data set

# Convert Activity column from a numeric field to a character field so it can hold string values
analysis_data$Activity <- as.character(analysis_data$Activity)
# Retrieve activity descriptions from the activity_labels data set and
# loop through substituting numeric values with the descriptions

for (i in 1:6){
  analysis_data$Activity[analysis_data$Activity == i] <- as.character(activities[i,2])
}
# Coerce Activity column to be a factor
analysis_data$Activity <- as.factor(analysis_data$Activity)

### 4. Appropriately labels the data set with descriptive variable names.

# Remove parentheses
names(analysis_data) <- gsub('\\(|\\)',"",names(analysis_data))
# Remove dashes
names(analysis_data) <- gsub('\\-'," ",names(analysis_data))
# Replace t and f with Time and Frequency
names(analysis_data) <- gsub('^t',"Time ",names(analysis_data))
names(analysis_data) <- gsub('^f',"Frequency ",names(analysis_data))
# Replace Acc with Accelerometer
names(analysis_data) <- gsub('Acc'," Accelerometer ",names(analysis_data))
# Replace Mag with Magnitude
names(analysis_data) <- gsub('Mag'," Magnitude ",names(analysis_data))
# Replace Gyro with Gyroscope
names(analysis_data) <- gsub('Gyro'," Gyroscope ",names(analysis_data))
# Replace BodyBody with Body
names(analysis_data) <- gsub('BodyBody',"Body",names(analysis_data))
# Replace mean,std with Mean and Standard Deviation
names(analysis_data) <- gsub('mean',"Mean",names(analysis_data))
names(analysis_data) <- gsub('std',"Standard Deviation",names(analysis_data))
# Replace double spaces with a single space
names(analysis_data) <- gsub('  '," ",names(analysis_data))

### 5. From the data set in step 4, creates a second, independent tidy data set with the average 
###    of each variable for each activity and each subject.

#Use ddply function to apply averages across all numeric columns
tidy_data <- ddply(analysis_data, c("Subject","Activity"), numcolwise(mean))
write.table(tidy_data, file = "tidy_data.txt", row.names=FALSE)
