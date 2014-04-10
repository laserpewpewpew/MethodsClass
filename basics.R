# POLI 5003						# The number sign indicates a comment: R ignores anything after one.
# Frederick Solt, Department of Political Science, University of Iowa
# R Code Examples File			


# Importing data 
anes <- read.table("ANES_2008.csv", header=T, sep=",", row.names="id") 
								# The read.table() command reads in data of various simple formats, 
								# like csv ("comma separated values").  The arrow ("<-") assigns  
								# whatever is on its right into the whatever is on its left; equals
								# ("=") is a synonym, but many users disfavor it as confusing (and, 
								# in rare circumstances, it causes problems).  This  
								# particular line of code reads in the 2008 ANES data and puts it  
								# into an object called "anes."

anes <- read.csv("ANES_2008.csv", row.names="id") 
                                # The read.csv() command is an alternative version of read.table() 
                                # with the appropriate options already specified
 
# Built-in help
?read.csv                       # Open the help file for a command 
??import                        # Search help files

# Making new variables
table(anes$V081101)				# The table() command gives the frequency of each value of a variable.
								# The dollar sign: aa$bb means "the element bb of object aa." If 
								# aa is a dataset, bb is a variable in that dataset.  In this case,
								# V081101 is the gender variable in the 2008 ANES dataset, which is
								# held in the object "anes." (Remember how we did that in the last line?)
								# The codebook <http://bit.ly/1noqRkY> says that it has two values: 1. male 
                                # and 2. female. We want a *binary* dichotomous variable, which we'll call 
anes$female <- anes$V081101-1	# "female," with 0. male and 1. female, so we just subtract one from V081101.


table(anes$V083005)				# V083005 is the reported frequency of voting.  It has two problems
							 	# for us.  First, it has two values for nonresponses (-9 and -8),
								# which don't tell us anything.  Second, high values of the variable  
								# correspond to low frequencies of voting, which makes no sense.  							
anes$votefreq[anes$V083005==4] <- 1	# We create a new variable, "votefreq", and assign it a value of 
anes$votefreq[anes$V083005==3] <- 2	# 1 when V083005 equals 4, and so on until we assign it a value 
anes$votefreq[anes$V083005==2] <- 3	# of 4 when V083005 equals 1: that is, we flip the values so 
anes$votefreq[anes$V083005==1] <- 4	# they make sense. We don't assign any values to votefreq if 
								# V083005 is -9 or -8, so that R will treat those as missing data 
								# (which appear as "NA" in the dataset) rather than as actual numbers.
								# Double-equals ("==") means "if equal to," and brackets after a
								# a variable name restrict the observations involved to only those 
								# for which the bracketed statement is true (an "index").  So the code
								# "anes$votefreq[anes$V083005==4] <- 1" means "Assign the value 1 to
								# the variable votefreq in the dataset anes for those observations
								# that have a value of 4 in the variable V083005 in the dataset anes."

anes$votefreq <- replace(anes$V083005, anes$V083005==4, 1) # An alternative to the forgoing is to use the
anes$votefreq <- replace(anes$V083005, anes$V083005==3, 2) # replace command.  Its first argument is an
anes$votefreq <- replace(anes$V083005, anes$V083005==2, 3) # object, the second is an index, and the third
anes$votefreq <- replace(anes$V083005, anes$V083005==1, 4) # is the value you want in the indexed position.

anes$votefreq <- ifelse(anes$V083005<0, NA, 5-anes$V083005) # A more convenient alternative in this case:  
								# we can use the ifelse command to both get rid of the uninformative negative 
								# responses and reverse the others' values in one step.  The ifelse command 
								# takes three arguments: a test, what to do if the test is true, and what 
								# to do if the test is false. Here, we ask whether the value of V083005 
								# is less than zero, if so, we set the it to missing (NA) in votefreq
								# and otherwise we reverse it by subtracting it from 5 and putting that 
								# value into votefreq.

# Calculating univariate statistics								
table(anes$votefreq)			# What is the mode of votefreq?  How do you know?
								# We can also assign the output of a command to an object, which  
mytable <- table(anes$votefreq) # we'll call "mytable," so that we can use it again later.
mytable							# Just naming an object lists its contents.  Don't do this with a 
								# big dataset like the ANES!

prop.table(mytable)				# The prop.table() command converts a table into proportions.
								# What is the median of votefreq?  How do you know?

mean(anes$votefreq, na.rm=T)	# The mean() command, um, gives the mean of a variable.  But if that
								# variable has any missing values, by default it will tell us that 
								# the mean is missing!  We avoid this problem by setting the na.rm   
								# option ("remove NAs") to TRUE.

mean(anes$votefreq[anes$female==0], na.rm=T) # What do you think this code does?
mean(anes$votefreq[anes$female==1], na.rm=T)


# Cross-tabs
library(gmodels)				# This loads the gmodels library.  You'll have to download it
								# to your machine the first time you want to use it with the command   
                                # install.packages("gmodels").  Then it can be loaded for a session using 
								# the library() or require() commands.  We load it now so that we can 
                                # use the CrossTable command.

CrossTable(anes$votefreq, anes$female, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T)
								# CrossTable makes a cross-tab.  The first variable will appear in
								# the rows, and the second in the columns.  The options specified say
								# that we don't want the proportions of the rows to appear (prop.r=FALSE)
								# but that we do want the proportions of the columns (prop.c=TRUE).
								# What hypothesis does this cross-tab test?  Is it supported?
								
								# To make a three-way crosstab, make a series of crosstabs
								# between your IV and DV, each restricted to observations with
								# a particular value on your control variable.  Let's examine whether
								# the difference between men and women in voting frequency holds up
table(anes$V083097)				# once their party is taken into account.  V083097 is party
								# identification: 1. Dem 2. Rep 3. Ind 4. Other 5. None.
anes$pid[anes$V083097==1] <- 1	# For convenience, let's make a new variable that's 1. Dem 2. Rep 
anes$pid[anes$V083097==2] <- 2	# 3. everyone else--what's the level of measurement for this 
anes$pid[anes$V083097>2] <- 3	# variable?--and see what we find.
CrossTable(anes$votefreq[anes$pid==1], anes$female[anes$pid==1], prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(anes$votefreq[anes$pid==2], anes$female[anes$pid==2], prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(anes$votefreq[anes$pid==3], anes$female[anes$pid==3], prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T)
								# Are women still more likely to vote more frequently than men, once 
								# party identification is taken into account?
