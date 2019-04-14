####
library(dplyr)
library(tidyr)
# Test data
setwd("~/Desktop/Clean Data/UCI HAR Dataset/test")
sub_test <- readLines("subject_test.txt")
set_test <- strsplit(readLines("X_test.txt")," ")
label_test <- readLines("y_test.txt")
test_set <- vector('list', length(set_test))
for (i in 1:length(set_test)) {
  demo <- set_test[[i]]
  test_set[[i]] <- subset(demo, demo!="")
}

# Training data
setwd("~/Desktop/Clean Data/UCI HAR Dataset/train")
sub_train <- readLines("subject_train.txt")
set_train <- strsplit(readLines("X_train.txt")," ")
label_train <- readLines("y_train.txt")
train_set <- vector('list', length(set_train))
for (i in 1:length(set_train)) {
  demo <- set_train[[i]]
  train_set[[i]] <- subset(demo, demo!="")
}

####
setwd("~/Desktop/Clean Data/UCI HAR Dataset")
activity_labels <- readLines("activity_labels.txt")
for (i in 1:length(activity_labels)) {
  activity_labels[i] <- strsplit(activity_labels[i], " ")[[1]][2]
}
# Relabeling label numbers with corresponding activity labels
for (i in 1:length(label_test)) {
  num <- as.numeric(label_test[i])
  label_test[i] <- activity_labels[num]
}
for (i in 1:length(label_train)) {
  num <- as.numeric(label_train[i])
  label_train[i] <- activity_labels[num]
}
# Cleaning feature labels from number in front
features <- readLines("features.txt")
for (i in 1:length(features)) {
  features[i] <- strsplit(features[i], " ")[[1]][2]
}
test_df <- data.frame(matrix(NA, nrow=length(test_set), ncol=length(features)))
train_df <- data.frame(matrix(NA, nrow=length(train_set), ncol=length(features)))
# Naming each column as a feature
names(test_df) <- features
names(train_df) <- features
# Loading feature data into data frame
for (i in 1:length(test_set)) {
  test_df[i,] <- as.numeric(test_set[[i]])
}
for (i in 1:length(train_set)) {
  train_df[i,] <- as.numeric(train_set[[i]])
}
# Adding subject identifiers and label names
test_df <- bind_cols(test_df, "Subject"=sub_test)
train_df <-bind_cols(train_df, "Subject"=sub_train)
test_df <- bind_cols(test_df, "Label"=label_test)
train_df <- bind_cols(train_df, "Label"=label_train)
# Extracting only the measurements on the mean and standard deviation for each
# measurement
selected <- c(features[grepl("mean", features)], 
              features[grepl("std", features)], 
              "Subject", "Label")
test_df <- test_df[,selected]
train_df <- train_df[,selected]
df <- bind_rows(test_df, train_df[1:length(train_df)])
df$Subject <- as.numeric(df$Subject)
dataset_1 <- df %>% select(-contains("BodyBody"))

####
## This is to make the new data set.
mutant <- dataset_1 %>% group_by(Subject, Label) %>% mutate_each(funs(mean = mean(.)))
mutant <- mutant[,71:length(mutant)]
dataset_2 <- mutant %>% distinct(.keep_all = TRUE)