# Plyr package
require(plyr)

#################################################3

# Setting Dir variable values
uci_dir <- "UCI\ HAR\ Dataset"


#Setting File variable values

activity_labels_file <- paste(uci_dir, "/activity_labels.txt", sep = "")
feature_file <- paste(uci_dir, "/features.txt", sep = "")

subject_train_file <- paste(uci_dir, "/train/subject_train.txt", sep = "")
x_train_file <- paste(uci_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_dir, "/train/y_train.txt", sep = "")

subject_test_file <- paste(uci_dir, "/test/subject_test.txt", sep = "")
x_test_file  <- paste(uci_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_dir, "/test/y_test.txt", sep = "")


# Loading data in datasets
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
features <- read.table(feature_file, colClasses = c("character"))

subject_train <- read.table(subject_train_file)
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)

subject_test <- read.table(subject_test_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)


#################################################################################################

# STEP 1 - Merge the training and the test sets to create one data set

train_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
bound_sensor_data <- rbind(train_sensor_data, test_sensor_data)

sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(bound_sensor_data) <- sensor_labels


# STEP 2 - Extract the measurements on the mean and standard deviation for each measurement

mean_std_sensor_data <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]


# STEP 3 - Use descriptive activity names to name the activities in the data set

mean_std_sensor_data <- join(mean_std_sensor_data, activity_labels, by = "ActivityId", match = "first")
mean_std_sensor_data <- mean_std_sensor_data[,-1]


# STEP 4 - Labels the data set with descriptive names

names(mean_std_sensor_data) <- gsub('\\(|\\)',"",names(mean_std_sensor_data), perl = TRUE)

names(mean_std_sensor_data) <- make.names(names(mean_std_sensor_data))

names(mean_std_sensor_data) <- gsub('Acc',"Acceleration",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('GyroJerk',"AngularAcceleration",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('Gyro',"AngularSpeed",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('Mag',"Magnitude",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('^t',"TimeDomain.",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('^f',"FrequencyDomain.",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('\\.mean',".Mean",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('\\.std',".StandardDeviation",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('Freq\\.',"Frequency.",names(mean_std_sensor_data))
names(mean_std_sensor_data) <- gsub('Freq$',"Frequency",names(mean_std_sensor_data))


# STEP 5 - New independent tidy data set with the average of each variable for each activity and each subject

sensor_average = ddply(mean_std_sensor_data, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_average, file = "sensor_average.txt")