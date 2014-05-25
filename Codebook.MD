## STUDY DESIGN

Experiments were carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 

Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments were video-recorded to label the data manually. The datasets were separated into two parts: 1) a training set, and 2) test set.

To obtain the tidyset
* the subject, activity and feature measurements files were merged,
* the next step was then to subset only the measurements relating to the mean and standard deviation of the participants, 
* the final step was to only to summarise each the data by averaging each measurement recorded by the subject (of which there were 30) for each of the activities they performed

In naming the variables, capitals were used to separate the multiple parts of the variable names. The reason for this that longer full names would have been too long and not useful for an end user.

Example:
- tBodyAccMeanX = time series data of the Body Accelertion Mean in the X direction. 

A summary of the parts that describe a variable:
* Acc: the body acceleration component of the acceleration signal
* Body: physical body of subject
* f: data that had a Fast Fourier Transform (FFT) applied
* Gravity: the gravity acceleration component of the acceleration signal
* Gyro: signals from the Gryoscope
* Jerk: measures the signal of body linear acceleration and angular velocity
* t: time domain signals; data recorded by averaging signals in a signal window sample
* X, Y or Z: direction of movement of subject

# VARIABLES

Below are the final variables used:

"Subject"
"activity"
"tBodyAccMeanX"           
"tBodyAccMeanY"            
"tBodyAccMeanZ"            
"tBodyAccStdX"            
"tBodyAccStdY"
"tBodyAccStdZ"             
"tGravityAccMeanX"        
"tGravityAccMeanY"         
"tGravityAccMeanZ"         
"tGravityAccStdX"         
["tGravityAccStdY"
"tGravityAccStdZ"          
"tBodyAccJerkMeanX"       
"tBodyAccJerkMeanY"        
"tBodyAccJerkMeanZ"        
"tBodyAccJerkStdX"        
"tBodyAccJerkStdY"         
"tBodyAccJerkStdZ"         
"tBodyGyroMeanX"          
"tBodyGyroMeanY"           
"tBodyGyroMeanZ"           
"tBodyGyroStdX"           
"tBodyGyroStdY
"tBodyGyroStdZ"
"tBodyGyroJerkMeanX"      
"tBodyGyroJerkMeanY"
"tBodyGyroJerkMeanZ"
"tBodyGyroJerkStdX"       
"tBodyGyroJerkStdY"        
"tBodyGyroJerkStdZ"        
"tBodyAccMagMean"         
"tBodyAccMagStd"
"tGravityAccMagMean"       
"tGravityAccMagStd"       
"tBodyAccJerkMagMean"
"tBodyAccJerkMagStd"       
"tBodyGyroMagMean"        
"tBodyGyroMagStd"
"tBodyGyroJerkMagMean"
"tBodyGyroJerkMagStd"     
"fBodyAccMeanX"            
"fBodyAccMeanY"
"fBodyAccMeanZ"           
"fBodyAccStdX"
"fBodyAccStdY"             
"fBodyAccStdZ"            
"fBodyAccJerkMeanX"
"fBodyAccJerkMeanY"
"fBodyAccJerkMeanZ"       
"fBodyAccJerkStdX"
"fBodyAccJerkStdY"
"fBodyAccJerkStdZ"        
"fBodyGyroMeanX"           
"fBodyGyroMeanY"           
"fBodyGyroMeanZ"          
"fBodyGyroStdX"
"fBodyGyroStdY"
"fBodyGyroStdZ"           
"fBodyAccMagMean"
"fBodyAccMagStd"
"fBodyBodyAccJerkMagMean" 
"fBodyBodyAccJerkMagStd"   
"fBodyBodyGyroMagMean"
"fBodyBodyGyroMagStd"     
"fBodyBodyGyroJerkMagMean" 
"fBodyBodyGyroJerkMagStd" 

