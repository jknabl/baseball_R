# Chapter 3: Graphics

setwd("/Users/jknabl/Documents/code-projects/baseball_R/data")
hof <- read.csv("hofbatting.csv")
hof

hof$midCareer <- with(hof, (From + To) / 2)
head(hof)


# Two fundamental data types: 1) measurement, 2) categorical. Represented in R as 1) numeric, and 2) factor variables.

# cut function breaks a dataset along an element (factor)


hof$Era <- cut(hof$midCareer, 
  breaks=c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
  labels=c("19th Century", "Lively Ball", "Dead Ball", "Integration", "Expansion", "Free Agency", "Long Ball")
)
head(hof)

# barplot function can be used to create a histogram of a factor variable. 

# create a count table of records by Era
T.Era <- table(hof$Era)
T.Era

# barplot(T.Era)

# For most graphics displays in R we can add xlab and ylab to give graphs labels. The main argument adds a title.

barplot(T.Era, xlab="Era", ylab="Frequency", main="Era of Nonpitcher Hall of Famers")

# plot function gives line plot, pie function gives pie chart

plot(T.Era)
pie(T.Era)

# Dot Plots -- alternative to bar graph.

# Convert T.Era (factor variable) to numeric using as.numeric
dotchart(as.numeric(T.Era), labels=names(T.Era), xlab="Frequency")

hof.500 <- subset(hof, HR >= 500)
hof.500 <- hof.500[order(hof.500$OPS, decreasing=TRUE), ]
tail(hof.500)

# Numeric variables: Stripchart and Histogram
# 
# Typically when investigating a numeric variable we want to know its distribution. Graphical displays are quick ways of identifying distributions.
# * Stripchart: one dimensional scatterplot. Basically a 'number line graph'

windows(width=7, height=3.5) 
stripchart(hof$midCareer, method="jitter", pch=1, xlab="Mid Career")

# * Histogram: divide possible values into equal-sized buckets and aggregate values over the buckets.

hist(hof$midCareer, xlab="Mid Career", main="")

# Charts with two numeric variables.
# * for when we want to explore the relationship between two values.
# * standard chart is the scatterplot

# Q: is there any relationship between OPS (On-Base Pct. plus Slugging Pct.) and era?
# * to see distribution, plot OPS vs Era, one on each axis.
# * it can be difficult to detect patterns in a scatterplot so we can add a smoothing curve to better visualize. (in R this function is lowess, which is an implementation of loess smoothing method.)

with(hof, plot(midCareer, OPS))
with(hof, lines(lowess(midCareer, OPS, f=0.3)))
with(hof, identify(midCareer, OPS, X, n=4))

# Notice bump in OPS between 1920-1940. Also notice that variability of data seems to become smaller as years go on, becoming very small in recent times.

# Two dimensions of hitting: ability to get on base, ability to advance runners already on base. OBP measuers getting onto base. SLG measures advancing runners. 
# Can compare the two dimensions to try and identify characteristics of hitters.

with(hof, plot(OBP, SLG))
with(hof, lines(lowess(OBP, SLG, f=0.9)))

# can limit the values displayed on a graph with xlim, ylim

with(hof, plot(OBP, SLG, 
			xlim=c(0.25, 0.50), ylim=c(0.28, 0.75), 
			pch=19, xlab="On-Base Percentage", ylab="Slugging Percentage"
		)
	)
with(hof, lines(lowess(OBP, SLG, f=0.3)))

# OPS statistic = OBP + SLG. We can draw lines representing constant values of OPS to the OBP/SLG graph to get some sense of hitter performance relative to OPS.
# Do this with the #curve function

curve(.7 - x, add=TRUE) # add means add this curve to the existing plot
curve(.8 - x, add=TRUE)
curve(.9 - x, add=TRUE)
curve(1.0 - x, add=TRUE)

# Using the #text function we can put labels on the plot at arbitrary points. By specifying x and y co-ordinates close to the OPS lines, we can 'label' the lines.

text(.27, .42, "OPS = 0.7")
text(.27, .52, "OPS = 0.8")
text(.27, .62, "OPS = 0.9")
text(.27, .72, "OPS = 1.0")

# Analyzing one numeric and one factor variable.
# * When we compare a numeric and a factor, we're typically looking to compute the distribution of the numeric across the factor.
# * We can use boxplot and stripchart for this.

hof$HR.Rate <- with(hof, HR/AB)

stripchart(HR.Rate ~ Era, data=hof)

# resize the plot. #par function is setting graphical parameters.

par(plt=c(.2, .94, .145, .883))
stripchart(HR.Rate ~ Era, data=hof, method="jitter", pch=1, las=2)

# Boxplots now: 

par(plt=c(.22, .94, .145, .883))
boxplot(HR.Rate ~ Era, data=hof, las=2, horizontal=TRUE, xlab="Home Run Rate")

# Comparing Ruth, Aaron, Bonds and A-Rod
# 1. Read data files into R. 2. Construct data frames containing home run data and age. 3. construct graph.

# Read from Lahman Master.csv
# * Note: Lahman DB 2018 does not have this file. I have switched the data files to have 2012 Lahman instead of 2018.

master <- read.csv("Master.csv")
tail(master)

# Write a function #getinfo to output a list of player ID and birth year given first and last names.

getinfo <- function(firstname, lastname) {
	playerline <- subset(master, nameFirst==firstname & nameLast==lastname)
	name.code <- as.character(playerline$playerID)
	birthyear <- playerline$birthYear
	birthmonth <- playerline$birthMonth
	birthday <- playerline$birthDay
	byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
	list(name.code=name.code, byear=byear)
}

ruth.info <- getinfo("Babe", "Ruth")
aaron.info <- getinfo("Hank", "Aaron")
bonds.info <- getinfo("Barry", "Bonds")
arod.info <- getinfo("Alex", "Rodriguez")
ruth.info

# Combine this data with Lahman batting file to create dataframes for each player 

batting <- read.csv("Batting.csv")


ruth.data <- subset(batting, playerID == ruth.info$name.code)
ruth.data$Age <- ruth.data$yearID - ruth.info$byear
ruth.data

aaron.data <- subset(batting, playerID == aaron.info$name.code)
aaron.data$Age <- aaron.data$yearID - aaron.info$byear

bonds.data <- subset(batting, playerID == bonds.info$name.code)
bonds.data$Age <- bonds.data$yearID - bonds.info$byear

arod.data <- subset(batting, playerID == arod.info$name.code)
arod.data$Age <- arod.data$yearID - arod.info$byear

# Plot cumulative sum of home runs against age for each player.
# * #cumsum function computes cumulative sums of a vector.

with(ruth.data, plot(Age, cumsum(HR), type="l", lty=3, lwd= 2, xlab="Age", ylab="Career Home runs", xlim=c(18, 45), ylim=c(0, 800)))
with(aaron.data, lines(Age, cumsum(HR), lty=2, lwd=2))
with(bonds.data, lines(Age, cumsum(HR), lty=1, lwd=2))
with(arod.data, lines(Age, cumsum(HR), lty=4, lwd=2))
legend(20, 700, legend=c("Bonds", "Aaaron", "Ruth", "ARod"), lty=1 : 4, lwd=2)

# 1998 home run race
# * Retrosheet play-by-play files are useful for learning aout patterns of play for a player during a season.

data1998 <- read.csv("all1998.csv", header = FALSE)
fields <- read.csv("fields.csv")
names(data1998) <- fields[, "Header"]
retro.ids <- read.csv("retrosheetIDs.csv")

sosa.id <- as.character(subset(retro.ids, FIRST=="Sammy" & LAST=="Sosa")$ID)
mac.id <- as.character(subset(retro.ids, FIRST=="Mark" & LAST=="McGwire")$ID)

sosa.data <- subset(data1998, BAT_ID==sosa.id)
mac.data <- subset(data1998, BAT_ID==mac.id)

createdata <- function(d) {
	d$Date <- as.Date(substr(d$GAME_ID, 4, 11), format="%Y%m%d")
	d <- d[order(d$Date), ]
	d$HR <- ifelse(d$EVENT_CD == 23, 1, 0)
	d$cumHR <- cumsum(d$HR)
	d[, c("Date", "cumHR")]
}

mac.hr <- createdata(mac.data)
sosa.hr <- createdata(sosa.data)
head(sosa.hr)

plot(mac.hr, type="l", lwd=2, ylab="Home Runs in the Season")
lines(sosa.hr, lwd=2, col="grey")
abline(h=62, lty=3)
text(10440, 65, "62")
legend(10440, 20, legend=c("McGwire (70)", "Sosa (66)"), lwd=2, col=c("black", "))