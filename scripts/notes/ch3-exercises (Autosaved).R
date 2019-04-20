# Chapter 3: Exercises

setwd("/Users/jknabl/Documents/code-projects/baseball_R/data")

hofpitching <- read.csv("hofpitching.csv")
tail(hofpitching)

# BF statistic: batters faced.

hofpitching$BF.group <- with(hofpitching, cut(BF, c(0, 10000, 15000, 20000, 30000)))
labels = c("< 10000", "10k-15k", "15k-20k", "20k-30k")

# Construct a frequency table for grouped batters faced data
batters_faced_freq <- table(hofpitching$BF.group)

# Construct a bar graph of the table
barplot(batters_faced_freq, main="Batters faced")

hist(hofpitching$BF.group, labels=labels)

pie(batters_faced_freq, labels=labels, main="Batters faced")

