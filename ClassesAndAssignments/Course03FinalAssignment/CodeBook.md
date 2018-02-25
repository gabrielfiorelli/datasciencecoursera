## Code Book for Course Project

### Overview

[Source](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) of the original data:

	https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

[Full Description](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) at the site where the data was obtained:

	http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
	

### Script Variables

- test = table contents of `test/X_test.txt`.
- train = table contents of `train/X_train.txt`.
- trainTest = Combined data set of the two above variables.
- subjectTest = table contents of `test/subject_test.txt`.
- subjectTrain = table contents of `test/subject_train.txt`.
- subjectUnion = Subjects. Combined data set of the two above variables.
- activityTest = table contents of `test/y_test.txt`.
- activityTrain = table contents of `train/y_train.txt`.
- activityUnion = Combined data set of the two above variables. 
- features = table contents of `features.txt`.
- features$FeatureNames = Names of features.
- activityLabels = table contents of `activity_labels.txt`. Human readable
- dataset = the raw organized data.
- dataSetSummarized = the summarized data.

### Process

The script `run_analysis.R` performs the following process to clean up the data
and create the output data sets:

1. Merge the test and training sets to create one data set.

2. Use the `features.txt` to identify the activity names and makes the data easir to human reading.

3. Use the `activity_labels.txt` to identify and select just the measurements that represents mean or standart deviation.

4. Merge the features, activity and subject in one raw dataset, with features names and activity labels. That content is saved at `dataset.txt`.

5. Grouped the data using subject and activity allowing group computations.

6. Used the grouped information to calculate the average for each pair of dimensions (Activity-Subject). That content is saved at `dataset_summarized.txt`.

### Output

#### dataset.txt

`dataset.txt` is a 10299x69 data frame.
- The first 2 columns there's Activity information (Id and Name).
- The third column contains subject information.
- The last 66 columns are measurements.
- Subject IDs are integers between 1 and 30.

#### dataset_summarized.txt

`dataset_summarized.txt` is a 180x68 data frame.

- The first column contains the activity names.
- The second column contains the subjects.
- The averages for each of the 66 attributes are in columns 3-68.