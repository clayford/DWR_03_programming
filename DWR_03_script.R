# Data Wrangling in R, Part 3 of 3
# Programming and Automation
# Fall 2018
# UVa Library - Research Data Services
# Clay Ford

library(tidyverse)
library(haven)   # so we can import an SPSS file


# Data for the workshop ---------------------------------------------------


# In this workshop we'll work with the 2013 Virginia Youth Survey data for high
# schools, downloaded from
# http://www.vdh.virginia.gov/virginia-youth-survey/data-files/.

# The Virginia Youth Survey (VYS) has been developed to monitor priority health
# risk behaviors that contribute markedly to the leading causes of death,
# disability, and social problems among youth and adults within the Commonwealth
# of Virginia. The survey is administered every odd year in randomly selected
# Virginia public schools.

# Questionnaire:
# http://www.vdh.virginia.gov/content/uploads/sites/69/2016/12/2013VAH-Questionnaire.pdf

# The file is SPSS format (SAV file), so we use the read_sav() function from the
# haven package.

vys <- read_sav("http://people.virginia.edu/~jcf2d/data/yrbs2013.sav")
dim(vys)

# The first 113 columns, Q1 - Q113, are survey responses. Other than Q5, Q6 and
# Q7, all of these responses are categorical variables that we might want to
# tabulate and plot.


# One thing we want to do is to convert "labelled" variables to "factors". For
# example look at the structure of the Q1 column in vys
str(vys$Q1)

# Notice the numbers are actually codes, and that their labels are in the labels
# attribute. haven provides the as_factor() function that converts a labelled
# variable to factor.

# Notice the before and after difference:
table(vys$Q1)
table(as_factor(vys$Q1))

# So we want to use as_factor on all the labelled variables. How to easily do that?

# The is.labelled() function tells us if a variable is labelled
is.labelled(vys$Q1)

# How many labelled columns? "Apply" is.labelled to vys. sapply simplifies the
# output to a vector.
sum(sapply(vys, is.labelled))

# We can iterate through vys and apply as_factor() to all the labelled columns
# using the mutate_if() function from dplyr. If is.labelled() returns TRUE, then
# it mutates the column so it is class factor.

vys <- vys %>% mutate_if(is.labelled, as_factor)
str(vys$Q1)
str(vys$Q2)


# Writing functions -------------------------------------------------------

# Use the function function to write functions. Not unlike the f(x) functions
# you may have learning about in algebra.

# f(x) = 2x + 6

f <- function(x) 2*x + 6
f(x = 2)

# The function is in our global environment and will be deleted when we close
# RStudio.

# don't have to use argument name since there's only one argument:
f(2)
f(100)

# Because the * and + operators are vectorized, so is our function.
f(c(2,4,6))
f(-10:10)

# Functions can have multiple arguments. For example, formula for BMI:
# BMI = weight/(height^2) * 703
bmi <- function(w, h) w/(h^2) * 703
bmi(w = 150, h = 68)

# don't have to use argument names, but need to put argument values in correct
# order!
bmi(150, 68)

# Again, since the ^, \, and * operators are vectorized, so is our bmi function:
bmi(w = c(150, 175, 200), h = c(68, 69, 70))

# A function can return multiple outputs. The c() and list() functions are
# useful for this.

# Return original weight and height with bmi as a vector. Notice that when our
# function has more than one line of code we need to wrap the lines in {}
bmi <- function(w, h){
  b <- w/(h^2) * 703
  c(weight = w, height = h, bmi = b)
}
bmi(w = 150, h = 68)

# Notice all the decimals that were added to weight and height. That's because
# vectors only store one type of data. Since bmi had decimal precision, the
# height and weight variables were coerced to decimal precision as well. 


# Return original weight and height with bmi as a list
bmi <- function(w, h){
  b <- w/(h^2) * 703
  list(weight = w, height = h, bmi = b)
}
bmi(w = 150, h = 68)

# Data Frames may be used as well, which are actually lists:
bmi <- function(w, h){
  b <- w/(h^2) * 703
  data.frame(weight = w, height = h, bmi = b)
}
bmi(w = 150, h = 68)

# And we can save the results
bmi.out <- bmi(w = 150, h = 68)
bmi.out

# Try entering bmi at the console with no parentheses or arguments;
# Try the same with the cor function


# YOUR TURN #1 ------------------------------------------------------------

# 1) Write a function that calculates the volume of a sphere, given a radius.
# Name it whatever you want. Don't use spaces or special character.
# Formula: A = 4/3*pi*r^3
# Note: "pi" is a built-in constant in R.


# 2) Try it out. Calculate the volume for sphere with radius of 3. Calculate the
# volume for spheres with radii ranging from 1:10



# Debugging functions -----------------------------------------------------

# There is much we can do to debug functions. The most basic thing to do, and
# only thing we'll cover, is to use the debug() function.

# Call debug on your function and then run your function. This opens the
# debugging window. Click the Next button to step through each line of code.
# Notice the environment window is specific to the function, so we can see any
# objects being created. We can also work with the objects at the console if
# needed.

# Let's introduce an error into a function and debug it.

# stackWords: a function that takes a sentence and stacks the words in a one
# column matrix.
stackWords <- function(x){
  words <- strsplit(x, split = " ")
  as.matrix(words)
}
stackWords("It's time to debug!")
# Not what we wanted!

# call debug on the function...
debug(stackWords)

# Now run the function.
stackWords("It's time to debug!")

# Click Next and investigate what's happening each step of the way.

# Fix: need to unlist the list
stackWords <- function(x){
  words <- unlist(strsplit(x, split = " "))
  as.matrix(words)
}
stackWords("It's time to debug!")

# Re-assigning the function turns off debugging. Can also turn off debugging
# with undebug() on the function. Or re-assign the function.

# There is also debugonce() function for a single-use instance of debugging.



# Functions: beyond math --------------------------------------------------


# Functions don't have to involve explicit mathematical formulas. 

# EXAMPLE 1

# Let's generate some data and run a t-test
set.seed(1)
x1 <- rnorm(n = 30, mean = 10, sd = 1.2)
x2 <- rnorm(n = 30, mean = 11, sd = 1.2)
t.test(x = x1, y = x2)

# The output includes a CI for difference in means, but not the estimated
# difference itself. Let's write a function that returns the difference and the
# CI of the difference.

# If we save the output of a t-test we get a list, from which we can extract the
# CI:
t.out <- t.test(x1, x2)
str(t.out)
t.out$conf.int
ci <- t.out$conf.int
ci[1]
ci[2]

# We can also do this:
ci <- t.test(x1, x2)$conf.int

# difference in means
mean(x1) - mean(x2)
dif <- mean(x1) - mean(x2)

# place everything into a named vector
c("CI lower" = ci[1], "Difference" = dif, "CI upper" = ci[2])
round(c("CI lower" = ci[1], "Difference" = dif, "CI upper" = ci[2]),3)

# Remove ci and dif from memory. We don't have to do this for function to work.
# I want to demonstrate that objects created in a function do NOT get created in
# our global environment.
rm(ci, dif)

# Place the relevant code in a function; notice we can include comments if we
# like; replace x1 and x2 with var1 and var2
meanDiff <- function(var1, var2){
  # get the CI
  ci <- t.test(var1, var2)$conf.int
  # get the diff in means
  dif <- mean(var1) - mean(var2)
  # place everything in vector
  round(c("CI lower" = ci[1], "Difference" = dif, "CI upper" = ci[2]),3)
  }

meanDiff(var1 = x1, var2 = x2)

# Notice the ci and dif objects created in the function do not appear in our
# global environment. When a function is run, it gets its own temporary
# environment for any objects it creates.

# Compare sepal length of the virginica and setosa iris species
meanDiff(var1 = iris$Sepal.Length[iris$Species=="setosa"], 
         var2 = iris$Sepal.Length[iris$Species=="virginica"])

# See appendices below (the ... argument) for how we could re-write function to
# utilize formula notation with a data argument.


# EXAMPLE 2

# A histogram with a smooth density estimate

# First generate some data; quick way to get a 100 x 50 data frame of numbers.
set.seed(2)
m <- matrix(data = rnorm(n = 100 * 50), ncol = 50)
dat <- as.data.frame(m)
names(dat)

# The slides showed base R graphics. Here we'll use ggplot.

# The code to create a histogram with a smooth density estimate for V1.
x <- dat[["V1"]]
info <- paste0("mean = ", round(mean(x),3), 
               "; median = ", round(median(x),3), 
               "; sd = ", round(sd(x),3))
ggplot(dat, aes_string(x = "V1", y = "stat(density)")) +
  geom_histogram(binwidth = 0.5, alpha = 0.5) +
  geom_density(color = "blue") +
  labs(title = "V1", subtitle = info)


# Now create a function called histDensity. Notice we basically copy-and-paste
# the code between "function(v){" and "}", and then replace "V1" with v. Also
# notice we have to use aes_string() instead of the usual aes() in ggplot to
# allow quoted strings.
histDensity <- function(v){
  x <- dat[[v]]
  info <- paste0("mean = ", round(mean(x),3), 
                 "; median = ", round(median(x),3), 
                 "; sd = ", round(sd(x),3))
  ggplot(dat, aes_string(x = v, y = "stat(density)")) +
    geom_histogram(binwidth = 0.5, alpha = 0.5) +
    geom_density(color = "blue") +
    labs(title = v, subtitle = info)
}

# Try out the function
histDensity("V2")
histDensity("V14")
histDensity("V42")




# YOUR TURN #2 ------------------------------------------------------------

# The following code returns counts and proportions of the Q1 question as a
# matrix.
x <- vys[["Q1"]]
qs <- attr(x = x, which = "label")
cnt <- as.matrix(table(x, useNA = "ifany"))
prop <- round(cnt/sum(cnt),3)
m <- cbind(cnt, prop)
colnames(m) <- c("Count","Proportion")
names(dimnames(m)) <- c("responses",qs)
m

# Place this code in a function called Qtable that will work for any categorical
# question in vys. (Q1 - Q4, Q8 - Q113)





# Iteration ---------------------------------------------------------------


# Sometimes we want to repeatedly use a function on different data or parts of
# data.

# R provides the apply family of functions for this purpose. The purrr package
# provides the map family of functions. It's a matter of taste which you choose,
# though purrr provides more control over the output.

# To apply a function to a vector or list of data, use lapply, sapply or the map
# family of functions.

# lapply returns a list; 
# sapply tries to simplify the output to a vector or matrix.
# map returns a list;
# map_dbl returns a numeric vector
# map_chr returns a character vector
# etc

# The apply function can be use to apply a function to rows of a matrix or data
# frame. Use the second argument to specify rows (1) or columns (2)

# EXAMPLE 1

# apply histDensity function to the names of the dat data frame. names(dat)
# returns a character vector.
names(dat)

# In the interest of time, we just do the first 3. The first argument is the
# vector of data you want to iterate over, the second argument is your function.
names(dat)[1:3]
lapply(X = names(dat)[1:3], FUN = histDensity)

# Notice the empty list returned to the console.

# What if we try sapply? Not what we wanted!
sapply(X = names(dat)[1:3], FUN = histDensity)

# To make it work we have to set simplify = FALSE
sapply(X = names(dat)[1:3], FUN = histDensity, simplify = FALSE)

# Let's try map; notice the argument names are different
map(.x = names(dat)[1:3], .f = histDensity)

# We could also use a for loop, but notice we have to use the print() function
# with functions that use ggplot.
for(i in 1:3){
  print(histDensity(names(dat)[i]))
}



# EXAMPLE 2

# Let's apply the summary function to the columns of the vys dat frame.
summary(vys$Q1)
summary(vys$Q2)

# Recall that (1) data frames are lists, and (2) we can use lapply to apply
# functions to lists. Also, since we want to apply summary() to every column in
# vys, we don't need to create a vector of column names. We can just apply
# summary() directly to vys.

# Using lapply
lapply(vys, summary)

# That's too much output; Plus it doesn't show the question.

# Let's save the output, which will be a list, and then rename the list elements
# to be the original questions.

# first save the output
summ.out <- lapply(vys, summary)

# The list element names are the variable names 
names(summ.out)

# Let's write a function that gets the question names from the vys data frame
# and outputs a character vector.

attributes(vys[["Q1"]])
attributes(vys[["Q1"]])$label
getAttr <- function(x)attributes(x)$label

qs <- lapply(vys, getAttr)
# use unlist to create character vector
qs <- unlist(qs)

# or better yet use purrr's map_chr
qs <- map_chr(vys, getAttr)

# Now rename the list elements
names(summ.out) <- qs

# Now questions appear with each variable summary
summ.out

# We'll use R Markdown later to produce output that's easier to read

# YOUR TURN #3 ------------------------------------------------------------

# Apply the QTable function from YOUR TURN #2 to the categorical (factor)
# variables of the vys data frame. We can apply/map the is.factor function to
# get a logical vector of T/F so we can subset the names for just the factor
# variables. map_lgl returns a vector of logical values.

k <- map_lgl(vys, is.factor)
q.factor <- names(vys)[k]

# 1) apply/map the QTable function over the q.factor vector and save the output
# as qtable.out.


# 2) rename the elements of the qtable.out list using the q.factor vector, which has
# the variable names. 



# Anonymous functions -----------------------------------------------------

# R allows us to create functions on-the-fly as "anonymous" functions when using
# apply/map. The function code is essentially moved into the apply/map function.
# This is useful for simple functions.

# EXAMPLE 1

# get number of missings for each column in vys
sum(is.na(vys$Q1))
sum(is.na(vys$Q2))

# We could write a function for this, but probably to just do it anonymously:
sapply(vys, function(x)sum(is.na(x)))
map_int(vys, function(x)sum(is.na(x)))

# The purrr map functions can also create functions using tilde and .x, like so:
map_int(vys, ~ sum(is.na(.x)))

# EXAMPLE 2

# Above we created the getAttr and then applied to vys.
getAttr <- function(x)attributes(x)$label
qs <- map_chr(vys, getAttr)

# We could have done this anonymously:
qs <- map_chr(vys, ~ attributes(.x)$label)

# EXAMPLE 3

# Sum of missings for each subject. Need to apply to rows, so we use apply().
# The first argument is the data frame, the second argument specifies rows (1)
# or columns (2), and the third argument is the function. The output in this
# case is a vector.
subj.miss <- apply(vys, 1, function(x)sum(is.na(x)))
summary(subj.miss)




# YOUR TURN #4 ------------------------------------------------------------

# The vys data frame has 6935 observations. Let's save as N.
N <- nrow(vys)

# The following line of code tells us if a column has all missing data.
sum(is.na(vys$Q1)) == N
sum(is.na(vys$Q58)) == N

# Some columns have been complete redacted due to the sensitive nature of the
# question.

# 1) Apply/map the code as an anonyous function to vys to create a logical
# vector of T/F so we can know which columns have all missing data. Save it as
# an object. Name it something like redacted. TIP: map_lgl would be good for this.


# 2) How many columns have all missings? Which questions?




# Extended example: bar plots ---------------------------------------------

# In this example we generate bar plots for all the vys categorical variables.

# First a bar plot for Q1. Recall that qs is a named vector. The "names" are
# above the values.
head(qs)

# We can use the names to extract values from the vector
qs["Q1"]
qs["Q2"]

# So we can use that in the plot title.

ggplot(vys, aes_string(x = "Q1")) + 
  geom_bar() + 
  coord_flip() + 
  ggtitle(qs["Q1"]) +
  xlab("")

# Let's make this into a function.

vys_bar_plot <- function(var){
  ggplot(vys, aes_string(x = var)) + 
    geom_bar() + 
    coord_flip() + 
    ggtitle(qs[var]) +
    xlab("")
}

# Let's try it out:
vys_bar_plot("Q11")
vys_bar_plot("Q14")



# Now we need to grab the column names that we want to plot. We want categorical
# (factor) variables that are not all missing.

(sum(is.na(vys$Q1)) != N) & is.factor(vys$Q1)   # Yes, plot this variable
(sum(is.na(vys$Q58)) != N) & is.factor(vys$Q58) # No, don't plot

# Let's map that expression anonymously to vys and save result
keep <- map_lgl(vys, ~ (sum(is.na(.x)) != N) & is.factor(.x))
# subset names(vys)
bc.vars <- names(vys)[keep]
length(bc.vars)

# And now use map (just for first four):
map(bc.vars[1:4], vys_bar_plot)

# To produce all 97, we could either produce 97 separate image files, or run the
# iteration in an R Markdown file

# Here's a function that produces a PDF file; mapping it to the vector Qs will
# produce 97 pdf files.
vys_bar_plot_pdf <- function(var){
  ggplot(vys, aes_string(x = var)) + 
    geom_bar() + 
    coord_flip() + 
    ggtitle(qs[var]) +
    xlab("")
  suppressMessages(ggsave(paste0(var,"_barplot.pdf")))
}

# We could also create an R markdown file to produce all 97 plots in one file.

# Steps:
# 1. File...New File...R Markdown; select HTML document; give it a name
# 2. Delete the template material (Everthing from "## R Markdown" on down)
# 3. Insert a new code chunk: on toolbar click Insert...R
# 4. In the code chunk, copy-and-paste the following, which is everything we
#    need to produce the plots.

library(tidyverse)
library(haven)
vys <- read_sav("http://people.virginia.edu/~jcf2d/data/yrbs2013.sav")
vys <- vys %>% mutate_if(is.labelled, as_factor)
N <- nrow(vys)

qs <- map_chr(vys, ~ attributes(.x)$label)

keep <- map_lgl(vys, ~ (sum(is.na(.x)) != N) & is.factor(.x))
bc.vars <- names(vys)[keep]

vys_bar_plot <- function(var){
  ggplot(vys, aes_string(x = var)) + 
    geom_bar() + 
    coord_flip() + 
    ggtitle(qs[var]) +
    xlab("")
}

map(bc.vars, vys_bar_plot)

# Or use for loop to prevent empty list elements from being printed ([[1]],
# [[2]], [[3]], etc.)

# for(i in 1:length(bc.vars)){
#   print(vys_bar_plot(bc.vars[i]))
# }


# 5. Edit top of code chunk to ```{r echo=FALSE, message=FALSE} to supress R
#    code and messages
# 6. Save the Rmd file to where the "yrbs2013.sav" file is located
# 7. Click the Knit button to generate HTML file with all plots



# YOUR TURN #5 ------------------------------------------------------------

# Recall the QTable function
QTable("Q1")

# Map/apply the function to the bc.vars vector in an R Markdown HTML document.
# For convenience the required R code is below. Notice the QTable has been
# slightly modified to use the kable function from the knitr package to produce
# a nice looking markdown-formatted table.

# Use the following options in your code chunk:
# message=FALSE, echo=FALSE, results='asis'

# 'asis' = write raw results from R into the output document (the raw results in
# this case are markdown-formatted tables)


library(tidyverse)
library(haven)
library(knitr)
vys <- read_sav("http://people.virginia.edu/~jcf2d/data/yrbs2013.sav")
vys <- vys %>% mutate_if(is.labelled, as_factor)
N <- nrow(vys)

qs <- map_chr(vys, ~ attributes(.x)$label)

keep <- map_lgl(vys, ~ (sum(is.na(.x)) != N) & is.factor(.x))
bc.vars <- names(vys)[keep]

QTable <- function(v){
  x <- vys[[v]]
  qs <- attr(x = x, which = "label")
  cnt <- as.matrix(table(x, useNA = "ifany"))
  prop <- round(cnt/sum(cnt),3)
  m <- cbind(cnt, prop)
  colnames(m) <- c("Count","Proportion")
  kable(m, caption = qs)
}


map(bc.vars, QTable)




# End of workshop!


# Appendix: non-standard evaluation ---------------------------------------


# Non-standard evaluation means evaluating an expression without using the usual
# R rules of evaluation.

# The subset function is a classic example of non-standard evaluation
subset(iris, subset = Species == "virginica")

# If we try that with subsetting brackets it doesn't work
iris[Species == "virginica",]

# We have to do the following
iris[iris$Species == "virginica",]

# What's non-standard about the subset argument in the subset function is that
# we can simply refer to "Species" instead of "iris$Species"

# The ggplot and dplyr packages work the same way
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()

# However that fails to work in a function
splot <- function(x,y){
  ggplot(iris, aes(x = x, y = y)) + geom_point()
}

splot(Sepal.Length, Petal.Length)

# Earlier I showed how we can use aes_string() to get around this.
splot <- function(x,y){
  ggplot(iris, aes_string(x = x, y = y)) + geom_point()
}

splot("Sepal.Length", "Petal.Length")

# That works, but (a) we had to quote the arguments, and (b) the aes help page
# says that aes_string has been soft_deprecated, which means its use is
# discouraged.


# We can use the aes() function if we wrap the argument calls in the base R
# get() function. The get() function takes a character string and finds the
# object in the specified environment, which in this case is the environment
# where the function is running. The following works with quoted strings.

splot <- function(x,y){
  ggplot(iris, aes(x = get(x), y = get(y))) + geom_point()
}

splot("Sepal.Length", "Petal.Length")


# If we wanted to use unquote strings we need to do the following:

# - wrap the argument calls in enquo(); Turns the argument into a string
# - precede the arguments with !! when they're used (!! to say that you want to
#   unquote an input so that it's evaluated)

splot <- function(x,y){
  x <- enquo(x)
  y <- enquo(y)
  ggplot(iris, aes(x = !! x, y = !! y)) + geom_point()
}

splot(Sepal.Length, Petal.Length)

# For a deeper explanation of enquo and !! see the dplyr vignette "Programming
# with dplyr"


# Appendix: the dot dot dot argument --------------------------------------

# The dot dot dot argument allows us to pass through arguments for other
# functions. Below we use the ... argument to allow us to utilize the full
# formula functionality of the t.test function.


# We can use formula notation to run t-test
t.test(Sepal.Length ~ Species, data = subset(iris, Species != "versicolor"))

# To allow our meanDiff function to use formulas we use the ... argument as
# follows:
meanDiff2 <- function(...){
  # save t-test output
  t.out <- t.test(...)
  # get the CI
  ci <- t.out$conf.int
  # get the diff in means (stored in estimate); use rev() to reverse order
  dif <- diff(rev(t.out$estimate))
  names(dif) <- NULL 
  # place everything in vector
  round(c("CI lower" = ci[1], "Difference" = dif, "CI upper" = ci[2]),3)
}

# Now we can use our function with a formula
meanDiff2(Sepal.Length ~ Species, data = subset(iris, Species != "versicolor"))


# Appendix: more on programming with base R graphics ----------------------


# What if we didn't want to quote the variable names? We have to use the deparse
# and substitute functions to convert to character.

# substitute - "substitute" the name of the object for what it actually
#               represents

# deparse - turn an unevaluated expression (such as the name of object) into a
#           character string

histDensity2 <- function(v){
  v <- deparse(substitute(v))  # converts v to character string
  x <- dat[[v]]
  h <- hist(x, plot = FALSE)
  info <- paste0("mean = ", round(mean(x),3), 
                 "; median = ", round(median(x),3), 
                 "; sd = ", round(sd(x),3))
  plot(h, freq = FALSE, 
       ylim = c(0, 1.2*max(h$density)),
       sub = info, main = v, xlab = v)
  lines(density(x), col = "blue")
}

histDensity2(V2)
histDensity2(V14)
histDensity2(V42)

# As we'll see, quoted names are sometimes easier to work with then unquoted
# names.


# What if we wanted a general function that works for ANY numeric column in ANY
# data frame? We need to create an argument for the data frame. Below I added an
# argument called "data":

histDensity3 <- function(v, data){
  v <- deparse(substitute(v))
  x <- data[[v]]                 # Notice data is an argument
  h <- hist(x, plot = FALSE)
  info <- paste0("mean = ", round(mean(x),3), 
                 "; median = ", round(median(x),3), 
                 "; sd = ", round(sd(x),3))
  plot(h, freq = FALSE, 
       ylim = c(0, 1.2*max(h$density)),
       sub = info, main = v, xlab = v)
  lines(density(x), col = "blue")
}

histDensity3(v = Sepal.Length, data = iris)
histDensity3(v = Petal.Width, data = iris)
histDensity3(v = Fertility, data = swiss)





# Appendix: mapply/pmap - applying functions to multiple vectors ----------

# The mapply (base) and pmap (purrr) functions allow us to apply functions that
# take multiple arguments to multiple vectors.

# Example: apply rnorm to vectors n, mean and sd, which correspond to the 3
# arguments of rnorm.

# mapply: function first and then the vectors
mapply(FUN = rnorm, 
       n = c(5,10,15), 
       mean = c(10,20,30),
       sd = c(1,4,8))

# pmap: list of vectors first, and then the function
pmap(.l = list(n = c(5,10,15),
               mean = c(10,20,30),
               sd = c(1,4,8)),
     .f = rnorm)




