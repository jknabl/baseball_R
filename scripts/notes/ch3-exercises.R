# Chapter 3: Exercises

setwd("/Users/jknabl/Documents/code-projects/baseball_R/data")

hofpitching <- read.csv("hofpitching.csv")
tail(hofpitching)

# 1: BF statistic: batters faced.

hofpitching$BF.group <- with(hofpitching, cut(BF, c(0, 10000, 15000, 20000, 30000)))
labels = c("< 10000", "10k-15k", "15k-20k", "20k-30k")

# Construct a frequency table for grouped batters faced data
batters_faced_freq <- table(hofpitching$BF.group)

# Construct a bar graph of the table
barplot(batters_faced_freq, main="Batters faced")

hist(hofpitching$BF.group, labels=labels)

pie(batters_faced_freq, labels=labels, main="Batters faced")

# 2: WAR
# * Wins Above Replacement player

# Construct a histogram
hist(hofpitching$WAR)

# 2 pitchers stand out in terms of WAR. Who are they?
standouts <- subset(hofpitching, hofpitching$WAR >= 125)
standouts

# The 2 pitchers are: Cy Young and Walter Johnson.

# 3: WAR/season

hofpitching$WAR.Season <- with(hofpitching, WAR / Yrs)

# Construct a stripchart of WAR.Season for different levels of BF.group
stripchart(hofpitching$WAR.Season ~ hofpitching$BF.group)

# Construct a boxplot of WAR.Season for different levels of BF.group
boxplot(hofpitching$WAR.Season ~ hofpitching$BF.group, horizontal=TRUE, main="WAR/Season by Batters Faced Group")

# How does WAR.Season depend on BF.group? 
# * There is a clear pattern that the higher the BF/group a player is in, the higher his WAR/Season. Median WAR/Season increases with increased BF.group. Variability appears to decrease somewhat as BF.group increases, too. 

# 4: 1960+ pitchers

hofpitching$midYear <- with(hofpitching, (From + To) / 2)
hofpitching.recent <- subset(hofpitching, midYear >= 1960)

# Order the rows of the dataframe by WAR/Season

hofpitching.recent <- hofpitching.recent[order(hofpitching.recent$WAR.Season),]

# Construct dot plot of WAR.Season where labels are pitcher names

with(hofpitching.recent, plot(WAR.Season, type="p" ))
with(hofpitching.recent, text(WAR.Season, labels=Name))

# Which 2 pitchers are standouts w/r/t WAR per season?
# * Tom Seaver and Bob Gibson

# 5: Mid-Year vs. War

# Construct plot of mid-year (horizontal) vs. WAR/season (vertical)

with(hofpitching, plot(midYear, WAR.Season))
with(hofpitching, lines(lowess(midYear, WAR.Season, f=0.3)))

# Is there a general pattern in the scatterplot?
# * There seems to be something of a downward trend. From 1880-1920 this is very steep -- WAR.Season per player falls dramatically. Then values rise slightly to ~1960, at which point values start dropping again. 

# There are two pitchers with ~1880s midyears that had very bad WAR.Season. Identify who they are using the #identify function.
# * #identify is used to make a plot overlay 'clickable' -- if you click near a point, you can render labels based on the values for that point in the dataset. In this example we use the x,y data for our plot and specify the label to be pitcher name. when we click points on the plot, the name of the pitcher at those data values pops up.

with(hofpitching, identify(midYear, WAR.Season, labels=Name, plot=TRUE))

# The 2 pitchers are: Monte Ward and Hank O'Day

# 6: Lahman Batting

lahman <- read.csv("Master.csv")
batting <- read.csv("batting.csv")
head(lahman)
head(batting)

# Use getinfo function from the chapter to read in data for Ty Cobb, Ted Williams, Pete Rose

getinfo <- function(firstname, lastname) {
	playerline <- subset(master, nameFirst==firstname & nameLast==lastname)[1,] # Assumption: this will always grab the first matching player 
	name.code <- as.character(playerline$playerID)
	birthyear <- playerline$birthYear
	birthmonth <- playerline$birthMonth
	birthday <- playerline$birthDay
	byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
	list(name.code=name.code, byear=byear)
}

cobb.info <- getinfo("Ty", "Cobb")
williams.info <- getinfo("Ted", "Williams")
rose.info <- getinfo("Pete", "Rose")
rose.info

# Create dataframe for each player, add Age to it.

cobb.data <- subset(batting, playerID == cobb.info$name.code)
cobb.data$Age <- cobb.data$yearID - cobb.info$byear

williams.data <- subset(batting, playerID == williams.info$name.code)
williams.data$Age <- williams.data$yearID - williams.info$byear

rose.data <- subset(batting, playerID == rose.info$name.code)
rose.data$Age <- rose.data$yearID - rose.info$byear

# Using #plot, construct line graph of cumulative hit totals against age for Pete Rose

with(rose.data, plot(Age, cumsum(H)))
with(rose.data, lines(lowess(Age, cumsum(H), f=0.3), lty=1))

# Overlay cumulative totals for Cobb and Williams
with(williams.data, lines(lowess(Age, cumsum(H), f=0.3), lty=2))
with(cobb.data, lines(lowess(Age, cumsum(H), f=0.3), lty=3))
legend(20, 700, legend=c("Pete Rose", "Ted Williams", "Ty Cobb"), lty=1 : 3, lwd=2)

# What can we learn about hitting patterns for each player? 
# * Pete rose hit HR pretty consistently until about age 40. At that point his HR production begins slowing down.
# * Ty Cobb started out hitting very high numbers of HRs and was extremely consistent in his hitting until about 36. At that point he levels off slightly and he did not play for as long as Pete Rose.
# * Ted Williams started off very strong, hitting high numbers of home runs. However he levelled off pretty early on. His HR production was a slower increasing curve than either Rose or Cobb. 



