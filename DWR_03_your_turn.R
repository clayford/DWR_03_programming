# DWR 3 Your Turn answers

# YOUR TURN #1 ------------------------------------------------------------

# 1) Write a function that calculates the volume of a sphere, given a radius.
# Name it whatever you want. Don't use spaces or special character.
# Formula: A = 4/3*pi*r^3
# Note: "pi" is a built-in constant in R.
volSphere <- function(r) 4/3*pi*r^3

# 2) Try it out. Calculate the volume for sphere with radius of 3. Calculate the
# volume for spheres with radii ranging from 1:10
volSphere(r = 1:10)


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

QTable <- function(v){
  x <- vys[[v]]
  qs <- attr(x = x, which = "label")
  cnt <- as.matrix(table(x, useNA = "ifany"))
  prop <- round(cnt/sum(cnt),3)
  m <- cbind(cnt, prop)
  colnames(m) <- c("Count","Proportion")
  names(dimnames(m)) <- c("responses",qs)
  m
}

QTable("Q2")
QTable("Q100")


# YOUR TURN #3 ------------------------------------------------------------

# Apply the QTable function from YOUR TURN #2 to the categorical (factor)
# variables of the vys data frame. We can apply/map the is.factor function to
# get a logical vector of T/F so we can subset the names for just the factor
# variables. map_lgl returns a vector of logical values.

k <- map_lgl(vys, is.factor)
q.factor <- names(vys)[k]

# 1) apply/map the QTable function over the q.factor vector and save the output
# as qtable.out.
qtable.out <- map(q.factor, QTable)

# 2) rename the elements of the qtable.out list using the q.factor vector, which has
# the variable names. 
names(qtable.out) <- q.factor



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

redacted <- map_lgl(vys, ~ sum(is.na(.x)) == N)

# 2) How many columns have all missings? Which questions?
sum(redacted)
qs[redacted]



