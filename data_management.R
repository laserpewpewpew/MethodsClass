### Data Management
# Contents: 
#   Scraping Web Tables
#   Renaming Variables
#   Reordering Observations
#   Subsetting
#   Binding
#   Reshaping
#   Aggregating
#   Merging

### Load packages, installing if necessary
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages <- c("XML", "RCurl", "doBy")
ipak(packages)


### Scraping Web Tables
# Importing local files is straightforward:
#   use load() to read .RData files;
#   read.table(), read.csv(), or read.delim() to read text files;
#   read.dta() from the foreign package to read Stata files;
#   and the spss file commands of the memisc package to read SPSS files
#   (see http://stackoverflow.com/questions/3136293/read-spss-file-into-r ).
# 
# Getting data from a table on a webpage is (often) only a bit trickier.

library(XML)
gsi.tables <- readHTMLTable("http://www.globalslaveryindex.org/findings/",
                        stringsAsFactors=F) # Creates a list of tables
gsi <- gsi.tables[[4]]  # The fourth table on the page is the one we want;
                        # use double square brackets, [[ and ]], to specify
                        # an element in a list

str(gsi)            # Note that the values are stored as character, 
                    # not numeric, because of the commas to mark 000s

# To fix this, we use gsub() to replace the commas with nothing and then
# as.numeric() to switch the type to numeric, looping over the third
# through sixth columns of the dataframe. We use single square brackets,
# [ and ], to specify a dataframe's elements (by row and column) or a
# vector's element by position
for (i in 3:6) {
    gsi[ , i] <- as.numeric(gsub(",", "", gsi[ , i])) 
}


### Renaming Variables
names(gsi)  # The variable names in the scraped dataframe are unwieldy
# We can replace them with a new list of names
names(gsi) <- c("rank", "country", "pop", "slaves.est", 
                "slaves.lb", "slaves.ub")
# We can also change just one variable name at a time
names(gsi)[4] <- "slaves"   # Fourth element of the vector!
names(gsi)[which(names(gsi)=="slaves")] <- "slaves.est"     # Too many to count?
names(gsi)[4] <- "slaves"   # Change it back


# Making a new variable
gsi$slaves.10k <- round((with(gsi, (slaves/pop) * 10000 )), digits=1)


### Reordering Observations
# Maybe we'd prefer the countries in alphabetical rather than rank order
gsi <- gsi[order(gsi$country), ]
# We can reorder with multiple variables: the second breaks ties among
# observations with the same value on the first, etc.  
gsi <- gsi[with(gsi, order(country, pop)), ] # Not interesting in these data


### Subsetting
# Suppose we wanted a list of countries in Europe
europe <- readHTMLTable("http://en.wikipedia.org/wiki/List_of_European_countries_by_population",
                        stringsAsFactors=F)
europe <- europe[[1]]   # Extract the first (and only) table

europe <- europe[ , 2]  # We subset the dataframe by referring to the rows 
                        # and columns we want: here we just want the second variable
europe                  # Note the bracketed footnotes 6 and 7
europe <- gsub("\\[[67]\\]", "", europe)    # gsub() accepts regular expressions--
                                            # see http://en.wikipedia.org/wiki/Regular_expression
                                            # http://www.regexr.com and of course
                                            # https://xkcd.com/208/ 

# We can use this to extract from the GSI table just the European countries
gsi.europe <- gsi[gsi$country %in% europe, ]    # "Rows of gsi for which the variable
                                                # country (in gsi) is matched by
                                                # an element in the vector europe"
                                            
# Do it again for Asian countries
asia <- readHTMLTable("http://en.wikipedia.org/wiki/List_of_Asian_countries_by_population")
asia <- asia[[1]]
asia <- asia[ , 2]
asia
asia <- gsub("\\[[567]\\]", "", asia)                       # darn footnotes
asia[asia=="Hong Kong (China)"] <- "Hong Kong, SAR China"   # To match gsi format
gsi.asia <- gsi[gsi$country %in% asia, ]


### Binding
# Binding is the reverse of subsetting: if we have all of the same variables
# in two dataframes, we can just bind the observations (rows) together.  
gsi.eurasia <- rbind(gsi.europe, gsi.asia)


### Reshaping
# Sometimes the rows and columns in a spreadsheet aren't arranged how we'd prefer.
# Freedom House's data are in a messy excel file with lots of issues, but notably 
# they have countries in rows and three year-variables in the columns ("wide" format).  
# Suppose we want country-year observations and three variables ("long" format).  
# R doesn't read excel files very well (thanks, M$), so first save it as a csv and  
# we'll start from there.

fh <- read.csv("Country Ratings and Status, 1973-2014 (FINAL).csv", 
                            as.is=T, skip=7, header=F)

# Fix variable names
fh.vars <- c("PR", "CL", "Status")
fh.years <- c(1972:1981, 1983:2013)
fh.names <- c("country", paste(rep(fh.vars, times=length(fh.years)),
                                rep(fh.years, each=length(fh.vars)), sep=".")) # Nifty, huh?
names(fh) <- fh.names

fh <- fh[!fh$country=="", !is.na(names(fh))]   # Drop notes and blank columns

# Reshape the data
fh1 <- reshape(fh, varying=2:dim(fh)[2], idvar="country", direction="long")

# Convert PR & CL to numeric (assigning NA in place of FH's ".." symbol)
for (i in 3:4) {
    fh1[ , i] <- as.numeric(fh1[ , i])
}

### Aggregating
# There are many different ways to aggregate data--to get averages over some group.  
# See http://mathewanalytics.wordpress.com/2014/03/26/r-101-summarizing-data/
# for a comprehensive list of the options. We'll use summarizeBy() now to get
# country averages of the Freedom House PR and CL scores. 
library(doBy)

fh2 <- summaryBy(PR + CL ~ country, data=fh1, FUN=c(mean), na.rm=T)

### Merging
# Merging lets you match up observations in different datasets
gsi.fh2 <- merge(x=gsi, y=fh2, all.x=T)
gsi.fh2[is.na(gsi.fh2$PR), "country"]   # List the countries that didn't match up

# Fix the unmatched countries
fh2$country[fh2$country=="Bosnia-Herzegovina"] <- "Bosnia and Herzegovina"
fh2$country[fh2$country=="Cote d'Ivoire"] <- "Côte d'Ivoire"
fh2$country[fh2$country=="Congo (Brazzaville)"] <- "Republic of Congo"
fh2$country[fh2$country=="Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
fh2$country[fh2$country=="Gambia, The"] <- "Gambia"
fh2$country[fh2$country=="Burma"] <- "Myanmar"
fh2$country[fh2$country=="East Timor"] <- "Timor-Leste"
fh2$country[fh2$country=="Trinidad & Tobago"] <- "Trinidad and Tobago"

# Try again
gsi.fh2 <- merge(x=gsi, y=fh2, all.x=T)
gsi.fh2[is.na(gsi.fh2$PR), "country"]       # Just Hong Kong


### A Toy Model

m1 <- lm(slaves.10k ~ CL.mean, data=gsi.fh2)

