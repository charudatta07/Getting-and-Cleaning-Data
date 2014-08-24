# Getting and Cleaning Data
# 1. Merges the training and the test sets to create one data set.
setwd("./UCI HAR Dataset")
# Acquire header columns for the data set
feature.names = read.table('features.txt')
#head(feature.names)

# Read in files and confirm input
testx = read.table('test/X_test.txt')
#head(testx)
#dim(testx)

testy = scan('test/Y_test.txt')
#head(testy)
#length(testy)

test_subjects = scan('test/subject_test.txt')
#head(test_subjects)
#length(test_subjects)

if ( length(testy) != length(test_subjects) || length(testy) != dim(testx)[1] ) {
  stop('dimensions of test data set inputs do not match')
}

trainx = read.table('train/X_train.txt')
#head(trainx)
#dim(trainx)

trainy = scan('train/Y_train.txt')
#head(trainy)
#length(trainy)
train_subjects = scan('train/subject_train.txt')
#head(train_subjects)
#length(train_subjects)

if ( length(trainy) != length(train_subjects) || length(trainy) != dim(trainx)[1] ) {
  stop('dimensions of training data set inputs do not match')
}
# Set headers on training and test data
names(trainx) = feature.names$V2
names(testx) = feature.names$V2
# Merge predictors and subject variable for each data set
trainx$subject = train_subjects
testx$subject = test_subjects

# Merge predictors and outcome variable for each data set

trainx$activity = trainy
testx$activity = testy

# Merge data sets

data_set = rbind(trainx, testx)
#head(data_set)
#dim(data_set)

# 2. Only the measurements on the mean and standard deviation for each measurement.

mean_and_std_columns = grepl( '(-mean\\(\\)|-std\\(\\))', feature.names$V2 )
mean_and_std_columns = append(mean_and_std_columns, TRUE) # keep the subject and activity
mean_and_std_columns = append(mean_and_std_columns, TRUE)

means_and_stds = data_set[, mean_and_std_columns]
names(means_and_stds)
dim(means_and_stds)


# 3.Uses descriptive activity names to name the activities in the data set

activity_labels = read.table('activity_labels.txt')
means_and_stds$activity_label = factor(means_and_stds$activity, levels=c(1,2,3,4,5,6), 
                                       labels=activity_labels$V2)


 
# 4.Appropriately labels the data set with descriptive activity names. 
# Understood to mean: Appropriately labels the data set with descriptive variable or 
# column names
# This was accomplished in Step 1 using the feature.names assigned to names(data.frame)

# 5.Creates a second, independent tidy data set with the average of each variable for each activity 
# and each subject. 
# Clarification: column for subject, a column for activity, and columns for each summary variable 
# (so 6*30 as each row represents a unique combination of subject and activity)
# probably there is a more concise way to do this

tidy.frame = data.frame()

subjects = sort( unique(means_and_stds$subject) )
activities = sort( unique(means_and_stds$activity) )

for (subj in subjects) {
  for (act in activities) {
    # subset by subject and activity
    subset = means_and_stds[ means_and_stds$subject==subj & means_and_stds$activity == act, ]
    # get mean values for subject-activity pair, coerce to data.frame
    by_subject_and_activity = as.data.frame( lapply( subset[,1:66], FUN=mean ) )
    # resupply subject, activity and activity label
    by_subject_and_activity$subject = subj
    by_subject_and_activity$activity = act
    by_subject_and_activity$activity_label = activity_labels[act,2]
    # build up tidy dataset
    tidy.frame = rbind(tidy.frame, by_subject_and_activity)
  }
}

write.table( tidy.frame, file="tidydata.txt",row.name=FALSE )