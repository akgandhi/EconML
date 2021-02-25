#This script opens and save a sample as .csv from data

library(ipumsr)
library(plyr)

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)

randomSamplefromData <- function(data, Nsample = 1000, fromTo = c(1, 3500000)){
  sample <- sample(fromTo[1]:fromTo[2], Nsample )
  return(data[sample,])
}

sampleData <- randomSamplefromData(data, Nsample = 2000)
sampleData <- sampleData[order(sampleData$YEAR , sampleData$MONTH, sampleData$SERIAL),]
write.csv(sampleData, file = "sampleData.csv")

