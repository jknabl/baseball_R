

setwd("/Users/jknabl/Documents/code-projects/baseball_R/data")
# Load a data frame from a CSV. Dataframe basically == 2d matrix
spahn <- read.csv("spahn.csv")
spahn[1:3, 1:10]

# Display all columns
spahn[1:3, ]

# Display all seasons, but only certain columns (stats) via bracket notation
spahn[1:10, c("Year", "W", "L")]

# We can get a vector for a single statistic like so:

spahn$ERA

# Descriptive statistics about the vector using summary

summary(spahn$ERA)

# Using logical operators to track down values matching some property, in this case Spahn's age when he had his lowest ERA:

spahn$Age[spahn$ERA == min(spahn$ERA)]

# Add new fields to a dataframe. e.g. calculating a more complex stat, FIP (fielding independent pitching)

spahn$FIP <- with(spahn, (13 * HR + 3 * BB - 2 * SO)/IP)

pos <- order(spahn$FIP)
head(spahn[pos, c("Year", "Age", "W", "L", "ERA", "FIP")])

# The `subset` function can be used to filter CSV data into a dataframe

spahn1 <- subset(spahn, Tm == "BSN" | Tm == "MLN")
spahn1

spahn1$Tm <- factor(spahn1$Tm, levels = c("BSN", "MLN"))
spahn1

# Can obtain summary statistics grouped on some field with the `by()` function
by(spahn1[, c("W.L", "ERA", "WHIP", "FIP")], spahn1$Tm, summary)

# Merging/combining dataframes
# `rbind` function combines/binds rows. Similar to `cbind`, which does the same for columns.

NLbatting <- read.csv("NLbatting.csv")
ALbatting <- read.csv("ALbatting.csv")
batting <- rbind(NLbatting, ALbatting)
batting

# `merge` function binds rows horizontally: 

NLpitching <- read.csv("NLpitching.csv")
NL <- merge(NLbatting, NLpitching, by = "Tm")
NL

# Use `subset` to choose only a subset of a dataframe that satisfies a condition. e.g., where home runs are greater than 150 

NL.150 <- subset(NLbatting, HR > 150)
NL.150

# It is common to want to split data into subsets, perform different operations on each subset, then re-combine the subsets. This is doable in R with the `sapply` and other functions in the `plyr` package.

Batting <- read.csv("Batting.csv")

# Only get batting records from 1960s seasons

Batting.60 <- subset(Batting, yearID > 1960 & yearID < 1970)

compute.hr <- function(pid) {
	d <- subset(Batting.60, playerID == pid)
	sum(d$HR)
}

players <- unique(Batting.60$playerID)
S <- sapply(players, compute.hr)

R <- data.frame(Player=players, HR=S)
R <- R[order(R$HR, decreasing=TRUE)]
head(R)

# plyr library has a number of functions for the split, apply, recombine operations. `ddply` applies a function to a subset of a dataframe, then recombines. 

library(plyr)
dataframe.AB <- ddply(Batting, .(playerID), summarize, Career.AB=sum(AB, na.rm=TRUE))

# Now we have a dataframe with total career at bats. Merge it with the existing batter statistics dataframe. 

Batting <- merge(Batting, dataframe.AB, by="playerID")

# Since we now have the career at bats statistic we can filter the batters list for only players w/ > 5000 career homers: 

Batting.5000 <- subset(Batting, Career.AB > 4999)
Batting.5000

# Compute more career statistics, using a function this time. 

ab.hr.so <- function(d) {
	c.AB <- sum(d$AB, na.rm=TRUE)
	c.HR <- sum(d$HR, na.rm=TRUE)
	c.SO <- sum(d$SO, na.rm=TRUE)
	data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}

aaron <- subset(Batting.5000, playerID == 'aaronha01')
ab.hr.so(aaron)

# Apply to each batter in the collection, just use `ddply`

d.5000 <- ddply(Batting.5000, .(playerID), ab.hr.so)
head(d.5000)

# `plot` function constructs a scatterplot

with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))