#!/usr/bin/env Rscript



### Welcome to R! ###

# First things first: this is a SCRIPT. Anything you write here  will be
#  executed if you RUN this - EXCEPT that the '#' character indicates a
#  COMMENT: anything after the # on any line will be ignored completely (except
#  for the very first line of this file).

# You should perform your analyses for Coursework 4 in R:
#  - create a new script;
#  - copy essential features (e.g. the first line) from this script to yours;
#  - write your R commands, using this file as a guide/help;
#  - run the script, as described below;
#  - the results (e.g. p-values) will be calculated by R
#  - you can then use these numbers to build your report

# To RUN an R script:
#  - download this to your laptop / School machine;
#  - make sure you are running LINUX (e.g. School computers) or Mac OSX;
#  - open an Explorer/Finder window;
#  - navigate to the folder where you put this file;
#  - right-click on the blank white space to open the menu; and
#  - click "Open in Terminal".
# A black-background command prompt should open.
#  - Simply type the name of the script preceded by: ./
#  - e.g. to run this script 'intro.r', run: ./intro.r






### SETTING UP YOUR SCRIPT ###

# You will need to load the dataset provided for the assignment.
# First, ensure you have downloaded the dataset to the same directory as your
#  script. It should be called 'datafile.csv'.
data <- read.csv("datafile.csv")
# The above creates a VARIABLE called 'data' which stores the information
#  in the file.

# You can always OUTPUT any piece of information by typing its name:
data
# When you RUN the script, the contents of 'data' will be shown.






### RETRIEVING SUBSETS OF YOUR DATA ###

# You can access just a few rows in the dataset using 'head':
head(data)
# This is useful when you just want to see what kind of data you're working
#  with, but don't care about working with the whole thing. Good when starting.


# You can refer to individual columns/variables in the dataset using a $, thus:
data$lat
# this will retrieve (and output) all latitude values.


# You can also impose restrictions on your data, such as retrieving only rows
#  (observations) which fulfil certain criteria. This is done by using:
#  subset(data, YOUR_CONDITIONS)
# These criteria can be anything you like. For example:

#  - retrieve all Scottish people's observations
# NOTE that there are TWO '=' symbols, not just one. This is important!
subset(data, nationality == "Scottish")

#  - retrieve all observations which are NOT about fallen trees
subset(data, text != "fallen tree")

#  - retrieve all observations made by anyone whose age is at least 30.
subset(data, age >= 30)

#  - retrieve all observations by anyone over-30s AND/OR Scottish.
# NOTE the double || which combines two conditions using 'OR'
subset(data, age >= 30 || nationality == 'Scottish')

#  - retrieve all observations by male participants with ID less than 10.
# NOTE the double && which combines two conditions using 'AND'
subset(data, pid < 10 && gender == 'm')


# Finally, you can combine these: to extract only one column from a specific
#  subset of your data, use: subset(data, YOUR_CONDITIONS)$YOUR_COLUMN
# For example:

#  - retrieve all latitudes entered by Scottish people
subset(data, nationality == 'Scottish')$lat

#  - retrieve the participant IDs for all rows relating to over-30s
subset(data, age >= 30)$pid





### STATISTICAL FEATURES ###

# You may wish to calculate basic data features in R. For example:

#  - calculate the standard deviation of all latitudes in the dataset
sd( data$lat )
#  - find the mean latitude entered for fault #1:
mean( subset(data, fid == 1)$lat )
#  - find the total length of time spent working with faults using MapA
sum( subset(data, map=="MapA")$f.time )






### GENERATING GRAPHS ###

# Many possibilities exist for producing graphs using R. By default,
#  running any of these will cause a file called 'Rplots.pdf' to be
#  created in the same directory as the script, which will contain all
#  the graphs you have requested.

#  - simple plots of coordinates, e.g. latitude vs. longitude:
plot( data$lat, data$long )
#  - bar charts, e.g. ages:
barplot( data$age )
#  - many more, which you are encouraged to find on your own

# To add a title to your graph, add some text as the PARAMETER 'main':
plot( data$lat, data$long, main="Latitude against Longitude" )

# To add labels for your axes, use "xlab" and/or "ylab" parameters.
barplot( data$age, xlab="Age", ylab="Frequency" )






### RUNNING STATISTICAL TESTS ###

# When running a test, you will be comparing two subsets calculated as above.
#  - write the line to generate one of your two subsets
#  - write the line to generate the other
#  - put them together, with a comma between them, in the brackets within the
#   test function you want:
# For a  T-TEST, the function is called t.test(...)
# For an F-TEST, the function is called var.test(...)
# For a  CHI-SQUARED TEST, the function is called chisq.test(...)


# For example (bear in mind these may not be sensible tests to run!):

#  - t-test comparing latitudes between first 10 and last 10 participants
t.test( subset(data, pid<=10)$lat, subset(data, pid>10)$lat )

#  - F-test comparing longitudes between participants 3 and 4
var.test( subset(data, pid==3)$long, subset(data, pid==4)$long )

#  - chi-squared comparing 'nationality' and 'map' on all participants
chisq.test( data$nationality, data$map )





### FURTHER POSSIBILITIES ###
# The above instructions should allow you to do the vast majority of what you
#  need for this assignment. However, R allows you to perform many, many more
#  complex interactions with your data - far too many to list here. Various
#  resources exist to help you take advantage of these features IF you need:
#  - the Internet, unreliable as its advice often is
#  - R's own help: run 'R' in your terminal then enter the name of a function
#    preceded by ?, e.g. ?apply
#    => note that you can quit R in the terminal by typing: quit()
#  - on the 27th March there will be a drop-in session when you can ask your
#   lecturers more detailed questions.
#  - to carry out advanced operations on your dataset, you MAY find the
#    following functions useful:
#    => unique() - remove duplicates
#    => apply()  - perform calculations on every row in a dataset
#    => cat()    - output some text of your own to the terminal
#    => simple arithmetic operators, which can be applied to entire
#       data columns if those columns are the same length.
