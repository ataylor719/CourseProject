## Read Me

"run_analysis.R" requires the "plyr" and "util" packages. It will warn you if these packages are not installed.

Upon execution in R, "run_analysis.R" will check to see if the "UCI HAR Dataset" is in your working directory. It will download and unzip the files if necessary. This file is over 60 MB. Download time will vary depending on your connection.

By default, the resulting output file is named "analysis.csv" and is saved in your working directory. This can be altered by setting the parameter "outputfile" to a different name and location when calling "run_analysis()" in R.

The resulting file is a comma-separated file that can be opened in a text editor or spreadsheet program.