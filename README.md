SmartphoneAnalysis
==================
The program is run_analysis.R. It performs the following steps:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement. 

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive variable names. 

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The details of run_analysis.R are as following. Bold fonts are comments for the explanation about what run_analysis.R did 

## 1. Merges the training and the test sets to create one data set.

### Merge train and test features by rows
Xtrain <- read.table("train/X_train.txt")
#### dim(Xtrain)
#### [1] 7352  561
Xtest <- read.table("test/X_test.txt")
#### dim(Xtest)
#### [1] 2947  561
X <- rbind(Xtrain, Xtest)
#### dim(X)
#### [1] 10299   561

### Merge train and test activities by rows
ytrain <- read.table("train/y_train.txt")
#### dim(ytrain)
#### [1] 7352    1
ytest <- read.table("test/y_test.txt")
#### dim(ytest)
#### [1] 2947    1
y <- rbind(ytrain, ytest)
#### dim(y)
#### [1] 10299     1

### Merge train and test subjects by rows
subjecttrain <- read.table("train/subject_train.txt")
#### dim(subjecttrain)
#### [1] 7352    1
subjecttest <- read.table("test/subject_test.txt")
#### dim(subjecttest)
#### [1] 2947    1
subject <- rbind(subjecttrain, subjecttest)
#### set subject column name
names(subject) <- c("subject")
#### dim(subject)
#### [1] 10299     1

### Merge features, activities and subjects by columns
Xys <- cbind(X, y, subject)
#### dim(Xys)
#### [1] 10299   563

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#### Load feature definition from features.txt
feature_def <- read.table("features.txt")
#### dim(feature_names)
#### [1] 561   2

#### Set column names: featureId, featureName
names(feature_def) <- c("featureId", "featureName")

#### feature_def$V1: feature id, feature_def$V2: feature name
#### Extract feature name with mean() or std()
#### fixed=TRUE in grepl for exact matching
mean_std_def <- feature_def[grepl("mean()", feature_def$featureName, fixed=TRUE) | grepl("std()", feature_def$featureName, fixed=TRUE),]
#### head(mean_std_def)
####  featureId  featureName
#### 1  1 tBodyAcc-mean()-X
#### 2  2 tBodyAcc-mean()-Y
#### 3  3 tBodyAcc-mean()-Z
#### 4  4  tBodyAcc-std()-X
#### 5  5  tBodyAcc-std()-Y
#### 6  6  tBodyAcc-std()-Z

#### Extract only the measurements on the mean and standard deviation on X by column id
mean_std_X <- X[, mean_std_def$featureId]

#### Merge extracted features(mean, std), activities and subjects by columns
Xys <- cbind(mean_std_X, y, subject)

## 3. Uses descriptive activity names to name the activities in the data set

activity_label <- read.table("activity_labels.txt")
#### set column names
colnames(activity_label) <- c("activityId", "activityName")
#### head(activity_label)
####   activityId    activityName
#### 1  1            WALKING
#### 2  2   WALKING_UPSTAIRS
#### 3  3 WALKING_DOWNSTAIRS
#### 4  4            SITTING
#### 5  5           STANDING
#### 6  6             LAYING

#### Look up activity id in activity_label$V1 and get the descriptive activity name from activity_lable$V2 
#### Assing to activity
activity <- apply(y, 1, function(id) {activity_label[which(id == activity_label$activityId),]$activityName})

#### Merge extracted features(mean,std), activities with descriptive names and subjects by columns
Xys <- cbind(mean_std_X, activity, subject)

#### Peeking the result
#### head(Xys[,65:68], n=3)
####  V542        V543       activity     subject
#### 1 -0.9919904 -0.9906975 STANDING         1
#### 2 -0.9958539 -0.9963995 STANDING         1
#### 3 -0.9950305 -0.9951274 STANDING         1

## 4. Appropriately labels the data set with descriptive variable names.
#### head(mean_std_def)
#### featureId   featureName 
#### 1  1 tBodyAcc-mean()-X
#### 2  2 tBodyAcc-mean()-Y
#### 3  3 tBodyAcc-mean()-Z
#### 4  4  tBodyAcc-std()-X
#### 5  5  tBodyAcc-std()-Y
#### 6  6  tBodyAcc-std()-Z

### mean_std_def $featureId $featureName: 66 feature names
#### dim(mean_std_def)
#### [1] 66  2

colnames(mean_std_X) <- unlist(mean_std_def$featureName)
colnames(y) <- c("activity")
colnames(subject) <- c("subject")

### Xys: 68 columns: 66 features + 1 activity + 1 subject
Xys <- cbind(mean_std_X, activity, subject)

#### dim(Xys)
#### [1] 10299    68

#### colnames(Xys)
#### [1] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"           "tBodyAcc-mean()-Z"          
#### [4] "tBodyAcc-std()-X"            "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
#### [7] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"        "tGravityAcc-mean()-Z"       
#### ...
#### [61] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"   "fBodyBodyGyroMag-mean()"    
#### [64] "fBodyBodyGyroMag-std()"      "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()" 
#### [67] "activity"                    "subject"          

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

group_by(Xys, subject, activity) %>% summarise_each(funs(mean)) %>% write.table(file="result.txt", row.name=FALSE)



