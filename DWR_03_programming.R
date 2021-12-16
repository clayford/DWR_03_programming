setwd("/Users/jcf2d/Box Sync/__Workshops/DWR_03_programming/")
library(haven)
VaYouthSurvey <- read_sas("yrbs2013.sas7bdat")
class(VaYouthSurvey)

library(ggplot2)
ggplot(VaYouthSurvey, aes(x = Q1)) + 
  geom_bar() 


# get names of columns for character values
keep <- sapply(VaYouthSurvey, function(x)is.character(x))
questions <- names(VaYouthSurvey)[keep]

# with string names
bar_plot1 <- function(question){
  ggplot(VaYouthSurvey, aes_string(x = question)) + 
    geom_bar() 
}

bar_plot1("Q1")
bar_plot1("Q2")
library(purrr)
map(questions[1:3], bar_plot1)
lapply(questions[1:3], bar_plot1)
walk(questions[1:3], bar_plot1)





# non-standard evaluation
bar_plot2 <- function(question){
  q <- enquo(question)
  ggplot(VaYouthSurvey, aes(x = !! q)) + 
    geom_bar() 
}

bar_plot2(Q2)


# non-standard evaluation using base R get()
bar_plot2 <- function(question){
  ggplot(VaYouthSurvey, aes(x = get(question))) + 
    geom_bar() 
}

bar_plot2("Q2")



substitute("Q2")
quote(Q2)
bquote(Q2)



csvInfo <- function(file){
  d <- read.csv(file = file)
  data.frame(rows = nrow(d),
             columns = ncol(d),
             size = format(object.size(d), 
                           units = "Kb"), 
             row.names = file)}
csvInfo(file = "earthquakes.csv")


fnames <- list.files()
l.out <- lapply(fnames, csvInfo)


l.out2 <- vector(mode = "list", length = 7)
for(i in 1:7){
  l.out2[[i]] <- csvInfo(file = fnames[i])  
}
l.out2
