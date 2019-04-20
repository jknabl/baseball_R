# 1 - base stealing

Players <- c("Rickey Henderson", "Lou Brock", "Ty Cobb", "Eddie Collins", "Max Carey", "Joe Morgan", "Luis Aparicio", "Paul Molitor", "Roberto Alomar")
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474) 
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)

# b) for all players, compute # of stolen base attempts (SB + CS)

SB.Attempts <- SB + CS
Success.Rate <- SB / SB.Attempts
SB.Game <- SB / G

# plot(SB.Game, Success.Rate)
# lines(lowess(SB.Game, Success.Rate))

# Are there players with unusually high or low success rates? 
# Yes. 

players.data <- data.frame(Players, SB, CS, G, SB.Attempts, Success.Rate, SB.Game)
# Player with lowest rate:
subset(players.data, Success.Rate == min(Success.Rate))

# Player with highest rate:
subset(players.data, Success.Rate == max(Success.Rate))

# 2 - character, factor, logical variables

outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double", "Out", "Walk", "Out", "Single")
table(outcomes) # interpreting each unique value as a factor

f.outcomes <- factor(outcomes, levels=c("Out", "Walk", "Single", "Double"))
table(f.outcomes) # Ordered by specified factors

outcomes == "Walk" # vector of logical variables where TRUE if the value is Walk
sum(outcomes == "Walk") # count of logical variables in the vector that have "Walk" as the value. 

# 3 - pitchers in 350 wins club

W <- c(373, 354, 364, 417, 355, 373, 361, 363, 511)
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 316)
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Spahn", "Young")

Win.PCT <- 100 * W / (W + L)

Wins.350 <- data.frame(Name, W, L, Win.PCT)
Wins.350
Wins.350[order(-Wins.350$Win.PCT),] # Christy Matthewson has highest win percentage; Pud Galvin has lowest. 

SO <- c(2198, 4672, 1806, 3509, 3371, 2502, 1868, 2583, 2803)
BB <- c(951, 1580, 745, 1363, 999, 844, 1268, 1434, 1217)
SO.BB.Ratio <- SO / BB
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)
subset(SO.BB, SO.BB.Ratio > 2.8)

SO.BB[order(-BB),] # The pitcher with the highest number of walks also had a very high strikeout/walk ratio. This looks to be because he had way more of both strikeouts and walks by volume than any other pitcher. 

Pitching <- read.csv("/Users/jknabl/Documents/code-projects/baseball_R/data/pitching.csv")
head(Pitching)

stats <- function(d) {
	c.SO <- sum(d$SO, na.rm=TRUE)
	c.BB <- sum(d$BB, na.rm=TRUE)
	c.IPouts <- sum(d$IPouts, na.rm=TRUE)
	c.midYear <- median(d$yearID, na.rm=TRUE)
	data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts, midYear=c.midYear)
}

library(plyr)

career.pitching <- ddply(Pitching, .(playerID), git stats)
head(career.pitching)

new.cp <- merge(career.pitching, Pitching, by="playerID")
head(new.cp)

career.10000 <- subset(new.cp, IPouts.x > 9999)
head(career.10000)

with(career.10000, plot(midYear, SO.x/BB.x))
with(career.10000, lines(lowess(midYear, SO.x/BB.x))) 

# Based on the above plot, there is a slight downward trend in strikeout/walk ratio for batters with mid-career in 1880 to around 1920. Around 1920 the general trend reverses, and strikeout/walk ratios steadily increase to present day.