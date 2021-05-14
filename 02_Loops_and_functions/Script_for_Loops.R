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