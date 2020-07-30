
#1. Preparing the working directory & load the data -------------------------
#setwd("D:\\BA World\\Data Science\\R-Programming\\Datasets\\Task") 

setwd("c:\\Users\\egot_\\Projects\\ABTest")

ABTest <- read.csv("Website Results.csv", header = TRUE)
save(ABTest, file = "c:\\Users\\egot_\\Projects\\ABTest\\rda\\ABTest.rda")
load("c:/Users/egot_/Projects/ABTest/rda/ABTest.rda")


head(ABTest, 12) #Check content  
str(ABTest)      #Check structure of the data
summary(ABTest)  