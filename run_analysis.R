#load required packages

library(dplyr)
library(data.table)

# load test X, Y and Subject data as well as the variable names codebook (features.txt) from the Test directory

test_x = read.table("X_test.txt")
test_y = read.table("y_test.txt")
subject_test = read.table("subject_test.txt")
features = read.table("features.txt")


#Rename variables in the three parts above:
  
  #test_x with corresponding 561 variable names from features txt

colnames(test_x) = features[,2]
  
  #test_y only column as "Activity"

colnames(test_y) = "Activity"

 
  #subject_test as "Subject

colnames(subject_test) = "Subject"


#Change test_y row values  (1 to 6) into corresponding activities as shown in activity_labels.txt file

test_y$Activity[test_y$Activity == 5] = "Standing"
test_y$Activity[test_y$Activity == 6] = "Laying"
test_y$Activity[test_y$Activity == 4] = "Sitting"
test_y$Activity[test_y$Activity == 3] = "Walking_downstairs"
ttest_y$Activity[test_y$Activity == 2] = "Walking_upstairs"
test_y$Activity[test_y$Activity == 1] = "Walking"


#Combine three parts into one dataset called test_data

test_data = cbind(subject_test, test_y, test_x)

#Do all of the above again for the training dataset


#load training X, Y and Subject data from Train directory

train_x = read.table("X_train.txt")
train_y = read.table("y_train.txt")
subject_train = read.table("subject_train.txt")
features = read.table("features.txt")


#Rename variables:
  #train_x with corresponding 561 variable names from features txt

colnames(train_x) = features[,2]

  #train_y as "Activity"

colnames(train_y) = "Activity"

  #subject_train as "Subject

colnames(subject_train) = "Subject"


  #train_y rows into corresponding activity from activity_labels.txt

train_y$Activity[train_y$Activity == 5] = "Standing"
train_y$Activity[train_y$Activity == 6] = "Laying"
train_y$Activity[train_y$Activity == 4] = "Sitting"
train_y$Activity[train_y$Activity == 3] = "Walking_downstairs"
train_y$Activity[train_y$Activity == 2] = "Walking_upstairs"
train_y$Activity[train_y$Activity == 1] = "Walking"


#Combine three parts into one dataset called test_data
train_data = cbind(subject_train, train_y, train_x)

#Merge both datasets to bind the training and test rows together

mega_dataset = rbind(train_data, test_data)

#Subsetting

#load the dataset into Dplyr
dataset = tbl_df(mega_dataset)

#subset columns which contain "std" or "mean"  

subset = select(dataset, contains("mean|std|Mean", ignore.case=TRUE))


#subsest for $Subject and $Activity

subset2 = dataset[,1:2]

#Combine two subsets

labelled_subset = cbind(subset2, subset)

#because of duplicate error, delete duplicate columns

dataset = dataset[,!duplicated(colnames(dataset))]

#Label variables
#Load new variable names from newVariableNames.csv file (prepared manually) where they are in column #3

variable_names = read.csv("newVariableNames.csv")

#Change variable names
colnames(subset) = variable_names[,3]

#Combine two subsets

labelled_subset = cbind(subset2, subset)



#Subset independent tidy data dataset which contains means for every measurements using Dplyr chaining 
#(divided below into 3 steps for clarity)

#Choose dataset then group by subject and activity then summarirse each variable by Mean for every value of Activity and Subject (30 subjects x 6 activities each = 180 rows)


tidy_data =labelled_subset %>% group_by(Subject, Activity)%>% summarise_each(funs(mean))

#save tidy_data to txt file
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)

