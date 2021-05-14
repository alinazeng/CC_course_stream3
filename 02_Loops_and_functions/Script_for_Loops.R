# May-13, 2021

# Set working directory ----
setwd("C:/Users/alina/Documents/git/Git_Tutorials/CC_course_stream3/02_Loops_and_functions")

# Import data ----
trees_bicuar <- read.csv("trees_bicuar.csv")
trees_mlunguya <- read.csv("trees_mlunguya.csv")

head(trees_bicuar)
str(trees_bicuar)

# The basic syntax for creating a function looks like this: ----
  example.fn <- function(x, y){
    # Perform an action using x and y
    x + y
  }
  
# The function() command is used to tell R that we are creating a function, and 
# we are assigning the function to an object called example.fn. x and y are 
# “arguments” for the function, i.e. things that the user provides when running 
# the function, then in the curly brackets are the actions performed by the 
# function, using the parameters defined by the user earlier in the function 
# call, and any other objects in the working environment in this case adding x 
# and y together.
  

# define a function that calculates the basal area of each stem in m^2 from the 
# diameter, which is in cm. The basal area is the cross-sectional area of the 
# tree trunk if it was cut parallel to the ground.  
  basal.area <- function(x){
    (pi*(x)^2)/40000
  }
  
  
# test the function
  basal.area(x = trees_bicuar$diam)

# Function arguments don’t need to be called x and y, they can be any character ----
# string, for example, the function below works identically to the one above,
# only x is now referred to as dbh:   
  
  basal.area <- function(dbh){
    (pi*(dbh)^2)/40000
  }
  

# Additionally, you can add a indeterminate number of extra arguments 
# using the ... operator. Imagine that we want to extend our basal.area() 
# function so that it can compute the combined basal area of multiple vectors of 
# diameter measurements, e.g. from multiple sites:
  basal.area <- function(...){
    (pi*c(...)^2)/40000
  }
  
  basal.area(trees_bicuar$diam, trees_mlunguya$diam)
  
  

# for()
  # A for() loop iterates through a number of items, most commonly stored as a 
  # list, and performs some action equally on each item. It can drastically 
  # reduce the amount of copying and pasting.
  
  for(i in list){
    # PERFORM SOME ACTION
  }
  
#  what if we had 100 field sites instead of just two? In that case, you can use
# a for() loop. First, we have to create a list of dataframes to perform the 
# loop on. There are many ways of doing this, but the simplest way is:
  
  trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)
  
# This makes a list called trees, where each element in the list is a dataframe. 
# List items within a list can be accessed using double square brackets, e.g. 
# trees[[1]] selects the first list item, the dataframe for trees_bicuar. We can 
# take advantage of this method of list indexing using square brackets when we 
# construct our for() loop:  
  
  for( i in 1:length(trees) ){
    trees[[i]]$ba <- basal.area(trees[[i]]$diam)
  }
  
  
# The first line sets up the loop, similar to how the function() definition 
# worked earlier. 1:length(trees) creates a sequence of integers from 1 to the 
# length of the list (trees), so in this case the sequence will be 1, 2 as there 
# are two list items. i will take each value of 1:length(trees) in turn, then 
# run the actions in the curly brackets once. For example, the first time the 
# loop runs, i will have a value of 1, and the second time i will have a value 
# of 2. Once the loop has run for the second time, the loop will end, as there 
# are no further values in 1:length(trees).
  
  
# First, separate trees_mlunguya into a list of dataframes, each based on the 
# contents of the year column:
  trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year)
  
# Create an empty list
  mean_ba_list <- list()
  
  for( i in 1:length(trees_mlunguya_list) ){
    ba <- basal.area(trees_mlunguya_list[[i]]$diam)
    mean_ba <- mean(ba)
    year <- mean(trees_mlunguya_list[[i]]$year)
    dat <- data.frame(year, mean_ba)
    mean_ba_list[[i]] <- dat
  }
  

# During each iteration, this loop creates a number of intermediate data objects 
# (ba, mean_ba, year), and eventually returns a dataframe (dat) with a single row 
# and two columns, one for year and one for mean basal area. Each of these 
# dataframes are then stored as a list item in the new list mean_ba_list.
  
#Of course, this intermediate calculation could be stored in it’s own custom function:

   
  ba.mean.year <- function(dbh, year){
    data.frame(
      mean_ba = mean(basal.area(dbh)),
      year = mean(year)
    )    
  }
  
  ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)
  
# And this new function can be used in the for loop:
  for( i in 1:length(trees_mlunguya_list) ){
    mean_ba_list[[i]] <- ba.mean.year(
      trees_mlunguya_list[[i]]$diam,
      trees_mlunguya_list[[i]]$year)
  }
  

# Note that this for() loop now contains a custom function (ba.mean.year()), 
# which itself contains a custom function (basal.area()), demonstrating that 
# there is really no limit to the complexity you can create with functional 
# programming tools like loops and function calls. You can even have loops 
# within loops, and loops in functions!
  

# To replicate the previous for() loop, where we calculated the mean basal area per year in trees_mlunguya, you can run:
lapply(trees_mlunguya_list, function(x){ba.mean.year(dbh = x$diam, year = x$year)})

# imagine we wanted to find the mean height of trees in trees_bicuar for each 
# taxonomic family.

# First, create a list of vectors of height (rather than dataframes) where each 
# list is a different family of species.
bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family)

lapply(bicuar_height_list, mean, na.rm = TRUE)

# write this into a data frame

table_test <- data.frame(lapply(bicuar_height_list, mean, na.rm=TRUE))

# I could use sapply() to get a more readable output from this loop. sapply() 
# simplifies the output of lapply() to a vector, with elements in the vector 
# named according to the name of the items in the original list:

sapply(bicuar_height_list, mean, na.rm = TRUE)


# The code below constructs a function with an ifelse() statement to calculate 
# Lorey’s mean height for the Bicuar plots.
stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  
  lorey_height <- sum(height_adj * ba, na.rm = TRUE) / sum(ba, na.rm = TRUE)
  
  return(lorey_height)
}

# Then we can test the function on each plot using lapply() like we did before:

  trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)

lapply(trees_bicuar_list, function(x){stick.adj.lorey(height = x$height, method = x$height_method, ba = x$ba)})

# ifelse() statements can also be used in conjunction with logical TRUE/FALSE 
# function arguments to determine whether certain actions are taken. For example, 
# we can write a function that calculates summary statistics on the trunk diameter 
# measurements for a given fieldsite, and we can use TRUE/FALSE arguments to let 
# the user decide whether certain statistics are calculated:


diam.summ <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE, 
                     mean(dbh), 
                     NA)
  median_dbh <- ifelse(median == TRUE, 
                       median(dbh), 
                       NA)
  mean_ba <- ifelse(ba == TRUE, 
                    mean(basal.area(dbh)), 
                    NA)
  
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

diam.summ(dbh = bicuar_trees$diam, mean = TRUE, median = FALSE)

# Also note that in this function definition the extra arguments have default 
# values, e.g. mean = TRUE. This means that even if the user doesn’t specify 
# what the value of mean should be, e.g. diam.summ(dbh = trees_bicuar$diam, 
# median = TRUE, mean_ba = FALSE), R will default to the value of mean = TRUE, 
# thus calculating the mean trunk diameter.
