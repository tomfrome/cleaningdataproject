The data used in this analysis come from Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

As a first step, we merged data from the training set and the test set to create one data set with 10299 observations of 563 variables, the first variable “subject” (a factor variable from 1 to 30, corresponding to the 30 subjects who participated in the experiment), the second “activity” (a factor variable from 1 to 6, corresponding to the different activities participants engaged in while wearing their devices, e.g. walking, sitting, etc.), and the rest measurements recorded by the phone (either its accelerometer or its gyroscope) or transformations of those measurements.

After arranging this large data set according to “subject” and “activity”, we found those variables that were either means or standard deviations of measurements (excluding meanFreq(), since these were weighted averages of the frequency components, and thus not true means), and reduced our data of interest to only those variables, along with “subject” and “activity”. 

We also labeled the factors of “activity” so that they would read “WALKING”, “WALKING_UPSTAIRS” and so on instead of 1-6.

After this we renamed the remaining variables to make them tidier. We removed periods and changed all letters to lower-case. Furthermore, the leading “t” at the beginning of some variable names was changed to “time”. Likewise, the leading “f” at the beginning of some variable names was changed to “fourier”. Any appearances of “mag” were changed to “magnitude”. We opted not to change “gyro” (for gyrometer) or “acc” (for accelerometer), because these abbreviations save the data analyst a lot of time typing, and are hard to confuse with other words. All variables can now be represented as [time/fourier][body/gravity][¿body][acc/gyro][¿jerk][¿magnitude][mean/std][¿x/y/z], where ¿ indicates an optional block. 

We also made “subject” into a factor variable.

Finally, we created a second, independent tidy data set with the average of each column for each subject and each activity. This final tidy data set has 180 observations of 68 variables:

"subject" "activity" "timebodyaccmeanx" "timebodyaccmeany" "timebodyaccmeanz" "timebodyaccstdx" "timebodyaccstdy" "timebodyaccstdz" "timegravityaccmeanx" "timegravityaccmeany" "timegravityaccmeanz" "timegravityaccstdx" "timegravityaccstdy" "timegravityaccstdz" "timebodyaccjerkmeanx" "timebodyaccjerkmeany" "timebodyaccjerkmeanz" "timebodyaccjerkstdx" "timebodyaccjerkstdy" "timebodyaccjerkstdz" "timebodygyromeanx" "timebodygyromeany" "timebodygyromeanz" "timebodygyrostdx" "timebodygyrostdy" "timebodygyrostdz" "timebodygyrojerkmeanx" "timebodygyrojerkmeany" "timebodygyrojerkmeanz" "timebodygyrojerkstdx" "timebodygyrojerkstdy" "timebodygyrojerkstdz" "timebodyaccmagnitudemean" "timebodyaccmagnitudestd" "timegravityaccmagnitudemean" "timegravityaccmagnitudestd" "timebodyaccjerkmagnitudemean" "timebodyaccjerkmagnitudestd" "timebodygyromagnitudemean" "timebodygyromagnitudestd" "timebodygyrojerkmagnitudemean" "timebodygyrojerkmagnitudestd" "fourierbodyaccmeanx" "fourierbodyaccmeany" "fourierbodyaccmeanz" "fourierbodyaccstdx" "fourierbodyaccstdy" "fourierbodyaccstdz" "fourierbodyaccjerkmeanx" "fourierbodyaccjerkmeany" "fourierbodyaccjerkmeanz" "fourierbodyaccjerkstdx" "fourierbodyaccjerkstdy" "fourierbodyaccjerkstdz" "fourierbodygyromeanx" "fourierbodygyromeany" "fourierbodygyromeanz" "fourierbodygyrostdx" "fourierbodygyrostdy" "fourierbodygyrostdz" "fourierbodyaccmagnitudemean" "fourierbodyaccmagnitudestd" "fourierbodybodyaccjerkmagnitudemean" "fourierbodybodyaccjerkmagnitudestd" "fourierbodybodygyromagnitudemean" "fourierbodybodygyromagnitudestd" "fourierbodybodygyrojerkmagnitudemean" "fourierbodybodygyrojerkmagnitudestd"

To read the data, simply use this code (after putting tidy_data.txt into your working directory):

data <- read.table(“tidy_data.txt”, header = TRUE)
View(data)
