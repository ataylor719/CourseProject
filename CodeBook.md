## Code Book for "Human Activity Recognition Using Smartphones Dataset" Analysis

### Description
The file "analysis.txt" has 180 rows and 68 variables. Each row identifies the subject and the activity the subject performed while wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, the researchers captured 3-axial linear acceleration and 3-axial angular velocity.

In order to derive the file, the R Script merges the observations researchers derived for subjects that had been separated into "test" and "training" groups. It adds the variables for subject and activity to that combined file as the original dataset kept these as separate files. The script also employs descriptive names for the variables and activities as identified in separate files in the original dataset. These descriptive names were made more uniform and readable for the "analysis.txt" file.

The file "analysis.txt" contains the average of all dataset observations for that subject and activity over the mean and standard deviation of the variables the researchers estimated from their measurements. This does not include the "meanFreq" weighted average of the frequency components that is included in the original dataset.

### License:
The file "analysis.txt" was derived from the "Human Activity Recognition Using Smartphones Dataset":

Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

## Subject and Activity Identifiers

### subjectid
Identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

### activity
Identifies the activity performed by the subject. There are six values for each subject:

* walking - Walking
* walkingupstairs - Walking upstairs
* walkingdownstairs - Walking downstairs
* sitting - Sitting
* standing - Standing
* laying- Laying

## Time Domain Signals
The prefix "time" denotes the time domain signals captured at a constant rate of 50 Hz. Researchers normalized and bounded the results within [-1,1].

### timebodyacc
Body acceleration signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* timebodyaccmeanx
* timebodyaccmeany
* timebodyaccmeanz
* timebodyaccstddevx
* timebodyaccstddevy
* timebodyaccstddevz

### timegravityacc
Gravity acceleration signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* timegravityaccmeanx
* timegravityaccmeany
* timegravityaccmeanz
* timegravityaccstddevx
* timegravityaccstddevy
* timegravityaccstddevz

* timebodyaccjerkmeanx
* timebodyaccjerkmeany
* timebodyaccjerkmeanz
* timebodyaccjerkstddevx
* timebodyaccjerkstddevy
* timebodyaccjerkstddevz

### timebodygyro
Body gyroscope signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* timebodygyromeanx
* timebodygyromeany
* timebodygyromeanz
* timebodygyrostddevx
* timebodygyrostddevy
* timebodygyrostddevz

### timebodygyrojerk
Body gyroscope jerk signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* timebodygyrojerkmeanx
* timebodygyrojerkmeany
* timebodygyrojerkmeanz
* timebodygyrojerkstddevx
* timebodygyrojerkstddevy
* timebodygyrojerkstddevz

### timebodyaccmag
Body acceleration magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* timebodyaccmagmean
* timebodyaccmagstddev

### timegravityaccmag
Gravity acceleration magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* timegravityaccmagmean
* timegravityaccmagstddev

### timebodyaccjerkmag
Body acceleration jerk magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* timebodyaccjerkmagmean
* timebodyaccjerkmagstddev

### timebodygyromag
Body gyroscope magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* timebodygyromagmean
* timebodygyromagstddev

### timebodygyrojerkmag
Body gyroscope jerk magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* timebodygyrojerkmagmean
* timebodygyrojerkmagstddev

## Frequency Domain Signals
The prefix "freq" denotes the frequency domain signals researchers derived using a Fast Fourier Transform (FFT). Researchers normalized and bounded the results within [-1,1].

### freqbodyacc
Body acceleration signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* freqbodyaccmeanx
* freqbodyaccmeany
* freqbodyaccmeanz
* freqbodyaccstddevx
* freqbodyaccstddevy
* freqbodyaccstddevz

### freqgravityacc
Gravity acceleration signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* freqgravityaccmeanx
* freqgravityaccmeany
* freqgravityaccmeanz
* freqgravityaccstddevx
* freqgravityaccstddevy
* freqgravityaccstddevz

* freqbodyaccjerkmeanx
* freqbodyaccjerkmeany
* freqbodyaccjerkmeanz
* freqbodyaccjerkstddevx
* freqbodyaccjerkstddevy
* freqbodyaccjerkstddevz

### freqbodygyro
Body gyroscope signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* freqbodygyromeanx
* freqbodygyromeany
* freqbodygyromeanz
* freqbodygyrostddevx
* freqbodygyrostddevy
* freqbodygyrostddevz

### freqbodygyrojerk
Body gyroscope jerk signals. Includes 3-axial (x, y, z) mean (mean) and standard deviation (stddev):

* freqbodygyrojerkmeanx
* freqbodygyrojerkmeany
* freqbodygyrojerkmeanz
* freqbodygyrojerkstddevx
* freqbodygyrojerkstddevy
* freqbodygyrojerkstddevz

### freqbodyaccmag
Body acceleration magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* freqbodyaccmagmean
* freqbodyaccmagstddev

### freqgravityaccmag
Gravity acceleration magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* freqgravityaccmagmean
* freqgravityaccmagstddev

### freqbodyaccjerkmag
Body acceleration jerk magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* freqbodyaccjerkmagmean
* freqbodyaccjerkmagstddev

### freqbodygyromag
Body gyroscope magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* freqbodygyromagmean
* freqbodygyromagstddev

### freqbodygyrojerkmag
Body gyroscope jerk magnitude signals calculated from three-dimensional signals using the Euclidean norm. Includes mean (mean) and standard deviation (stddev):

* freqbodygyrojerkmagmean
* freqbodygyrojerkmagstddev