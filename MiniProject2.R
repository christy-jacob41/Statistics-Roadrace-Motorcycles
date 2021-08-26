
# getting tidyverse
library(tidyverse)

# importing datasets
roadrace <- read.csv("roadrace.csv")
motorcycle <- read.csv("motorcycle.csv")

# verifying that we have 5875 runners
nrow(roadrace)

# creating bar graph of whether contestant is from Maine
barplot(c(sum(roadrace$Maine=='Maine'), sum(roadrace$Maine == 'Away')), names.arg = c('Maine', 'Away'), space = 0.2, ylab = '# of runners')
# total number of runners from Maine
sum(roadrace$Maine=='Maine')
# total number of runners away from Maine
sum(roadrace$Maine=='Away')

# creating a histogram of the time in minutes of the runners from Maine and Away from Maine
hist(roadrace$Time..minutes.[which(roadrace$Maine=='Maine')])
hist(roadrace$Time..minutes.[which(roadrace$Maine=='Away')])

# summary statistics of the time in minutes of the runners from Maine and Away from Maine
summary(roadrace$Time..minutes.[which(roadrace$Maine=='Maine')])
summary(roadrace$Time..minutes.[which(roadrace$Maine=='Away')])
IQR(roadrace$Time..minutes.[which(roadrace$Maine=='Maine')])
IQR(roadrace$Time..minutes.[which(roadrace$Maine=='Away')])
range(roadrace$Time..minutes.[which(roadrace$Maine=='Maine')])
range(roadrace$Time..minutes.[which(roadrace$Maine=='Away')])
sd(roadrace$Time..minutes.[which(roadrace$Maine=='Maine')])
sd(roadrace$Time..minutes.[which(roadrace$Maine=='Away')])

# creating a side by side boxplot of the time in minutes of the runners from Maine and away from Maine
boxplot(roadrace$Time..minutes.[which(roadrace$Maine=='Maine')], roadrace$Time..minutes.[which(roadrace$Maine=='Away')], names = c('Maine', 'Away'))

# creating a side by side boxplot of the ages of the runners who are male or female
# had to change ages to numeric since they were character
MaleAges <- as.numeric(roadrace$Age[which(roadrace$Sex=='M')])
FemaleAges <- as.numeric(roadrace$Age[which(roadrace$Sex=='F')])
boxplot(MaleAges, FemaleAges, names = c('Male', 'Female'))

# summary statistics of the ages of the runners who are male or female
summary(MaleAges)
summary(FemaleAges)
IQR(MaleAges)
IQR(FemaleAges)
range(MaleAges)
range(FemaleAges)
sd(MaleAges)
sd(FemaleAges)

# creating a boxplot for the fatal motorcycle accidents dataset
boxplot(motorcycle$Fatal.Motorcycle.Accidents, ylab = "Number of Fatal Motorcycle Accidents")

# calculating lower and upper bounds to find outliers
lower <- max(quantile(motorcycle$Fatal.Motorcycle.Accidents, prob=0.25) - 1.5*IQR(motorcycle$Fatal.Motorcycle.Accidents), min(motorcycle$Fatal.Motorcycle.Accidents))
upper <- min(quantile(motorcycle$Fatal.Motorcycle.Accidents, prob=0.75) + 1.5*IQR(motorcycle$Fatal.Motorcycle.Accidents), max(motorcycle$Fatal.Motorcycle.Accidents))

# finding the outlier counties which are lower than lower bound or higher than upper bound
motorcycle$County[which(motorcycle$Fatal.Motorcycle.Accidents<lower | motorcycle$Fatal.Motorcycle.Accidents > upper)]

# summary statistics of fatal motorcycle accidents dataset
summary(motorcycle$Fatal.Motorcycle.Accidents)
IQR(motorcycle$Fatal.Motorcycle.Accidents)
range(motorcycle$Fatal.Motorcycle.Accidents)
sd(motorcycle$Fatal.Motorcycle.Accidents)
