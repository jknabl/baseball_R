# Chapter 4: The Relation between Runs and Wins

# We want to know how much a player's strengths contribute to team wins (or weaknesses detract). We can measure contribution in terms of runs.

setwd("/Users/jknabl/Documents/code-projects/baseball_R/data")

teams <- read.csv("Teams.csv")
tail(teams)

myteams <- subset(teams, yearID > 2000)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
head(myteams)

# Run differential: diff between runs scored and allowed for a team. Winning proportion: fraction of games won by a team (aka win pct)

myteams$RD <- with(myteams, R - RA)
myteams$WPct <- with(myteams, W / (W + L))

# Scatterplot shows relationship: 

plot(myteams$RD, myteams$WPct, xlab="Run Differential", ylab="Win Percentage")

# Linear Regression

# Simple linear model: WPct = a + b * RD + epsilon
# a,b == unknown constants; epsilon == error term that captures all other factors, influencing the dependent variable (Which is WPct)
# * In R, #lm function fits a linear model to a data set
#   * Dependent variable on left side of tilde, independent variables on right side

linfit <- lm(WPct ~ RD, data=myteams)
linfit

# Now that we have the coefficients, we can plot a line using #abline

abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

# We have the equation: WPct = 0.4999925 + 0.0006279 * RD
# * So a team w/ RD of 0 will have WPct of .500, which makes sense.

# Function #predict can be used to calculate predicted values from a model. Function #residuals computes error.

myteams$linWPct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)
 
plot(myteams$RD, myteams$linResiduals, xlab="Run Differential", ylab="Residual")
abline(h=0, lty=3)

# * Interpret residuals as error of linear model in predicting WPct from RD. 

# Get average value of residuals to determine model bias: 

mean(myteams$linResiduals)

# Mean of residuals is ~0, so it is 'unbiased': equally likely to underpredict as to overpredict. 
# RMSE: Root Mean Square Error. Estimates the _average magnitude of errors_. 
#   1. Square residuals (so that each value becomes positive)
#   2. Calculate mean of squared residuals
#   3. Take square root of mean. 

linRMSE <- sqrt(mean(myteams$linResiduals ^ 2))
linRMSE

# 2/3 residuals fall between [-RMSE,+RMSE]. 95% fall between [-2*RMSE,+2*RMSE]

nrow(subset(myteams, abs(linResiduals) < linRMSE)) / nrow(myteams) # number of rows where residual is less than RMSE / total rows
nrow(subset(myteams, abs(linResiduals) < 2*linRMSE)) / nrow(myteams)

# Pythagorean Formula for Win Percentage
# Bill James
# WPct = R^2 / (R^2 + RA^2)

myteams$pytWPct <- with(myteams, R^2 / (R^2 + RA^2))
myteams$pytWPct

# We can calculate residials of this pythagorean formula prediction manually (just diff. between predicted and actual win percentages)

myteams$pytResiduals <- myteams$WPct - myteams$pytWPct

# Now calculate RMSE of residuals:

pytRMSE <- sqrt(mean(myteams$pytResiduals^2))
pytRMSE

# This is similar to values for our linear regression earlier. The question is, if the regression model is simpler, what justifes using the more complex pythagorean formula?
# * Basically, pythagorean formula better predicts more extreme scenarios. For example...
# * Imagine a team that hits 10 runs every game and allows 5. In a 162-game schedule the team hits 1620 runs, and allows 810. Run Differential in this case is 810. The linear regression model on Run Differential predicts this team to have a 1+ winning percentage, which is not possible.
# * Imagine a team that hits 1 run every game and allows 0. In a 162-game schedule the team hits 162 runs, and allows 0. RD is this 162. The linear regression model on RD predicts this team to have a WPct of .600, where the team clearly has a 0 WPct. 

# 2001 Seattle Mariners are an actual example. Had a 116-46 record and a +300 RD. (Regression model underpredicts by about .03 pct)
# 2003 Detroit Tigers are another example. 43-119 record and -337 RD. 

# The Exponent in the Pythagorean Formula
# * Subsequent investigations looked for an exponent other than 2 that would be a better predictive fit. 

# WPct = R^k / (R*k + RA^k)
# W/L = R^k / RA^k
# Take the log of both sides to obtain a linear relationship: 
# log(W/L) = log(R^k / RA^k)
# log(W/L) = k * (log(R / RA))
# 
# With this value we can estimate k via regression. Response (dependent) variable == log(W/L), predictor is log(R/RA).

myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams)
pytFit

# The coefficient value is 1.909 which suggests a smaller exponent than 2. 

# 4.6: Good and Bad Predictions by the Pythagorean Formula

# 2011 Boston Red Sox. Pythagorean formula predicted 95 wins. They actually won 90. This cost them a wildcard spot. To find out why the formula overpredicted, we can look at game-by-game data. 

gl2011 <- read.csv("gl2011.txt", sep=",")
glheaders <- read.csv("game_log_header.csv")
names(gl2011) <- names(glheaders)
BOS2011 <- subset(gl2011, HomeTeam == "BOS" | VisitingTeam == "BOS")[ , c("VisitingTeam", "HomeTeam", "VisitorRunsScored", "HomeRunsScore")]
head(BOS2011)

BOS2011$ScoreDiff <- with(BOS2011, ifelse(HomeTeam == "BOS", HomeRunsScore - VisitorRunsScored, VisitorRunsScored - HomeRunsScore))
BOS2011$W <- BOS2011$ScoreDiff > 0

# Let's compute summary statistics using the #aggregate function. The first argument to this function is the variable for which we want statistics to be calculated (absolute value of RD), second arg is a list of grouping factors (in our case just W, the boolean var indicating a win vs loss), final argument is summarizing function to apply.

aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)	

# Interpretation: when BOS lost, Run Differential was lower on average than when BOS won. i.e. Victories decided by larger margin than losses. This lead to underperforming pythagorean prediction. (under/overperformance often seen as (un)lucky in sabermetrics circles)
# * Team can overperform Pythagorean WPct by winning a disproportionate amount of close games:

results <- gl2011[ , c("VisitingTeam", "HomeTeam", "VisitorRunsScored", "HomeRunsScore")]
results$winner <- ifelse(results$HomeRunsScore > results$VisitorRunsScored, as.character(results$HomeTeam), as.character(results$VisitingTeam)) #contains winner
results$diff <- abs(results$VisitorRunsScored - results$HomeRunsScore) #contains run differential for the game

onerungames <- subset(results, diff == 1)
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")

# Now we can look at relationship between Pythagorean residuals and one run wins.

teams2011 <- subset(myteams, yearID == 2011)
teams2011[teams2011$teamID == "LAA", "teamID"] <- "ANA" # Angels coded as LAA in Lahman DB but ANA in Retrosheet. We are working with Retrosheet, so change any instances of LAA to ANA.
teams2011 <- merge(teams2011, onerunwins)
plot(teams2011$onerunW, teams2011$pytResiduals, xlab="One-Run Wins", ylab="Pythagorean Residuals")
identify(teams2011$onerunW, teams2011$pytResiduals, labels=teams2011$teamID)

# Data shows that e.g. SF giants have a very high one-run WPct and a high pythagorean residual (meaning, they won disproportionately more close games than expected and overperformed pythagorean expectation). Some analysts attribute this to luck. But others hypothesize that there are systematic factors that can lead to winning a disproportionate number of close games. For example having quality closers will tend to preserve small leads and be able to overperform pythagorean WPct. Check this:

pit <- read.csv("pitching.csv")
top_closers <- subset(pit, GF > 50 & ERA < 2.5)[ , c("playerID", "yearID", "teamID")]

# merge top_closers dataframe with myteams dataframe

teams_top_closers <- merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)


# How many runs per game? 
# Rule of thumb is 10 runs == 1 win. This is based directly on Pythagorean Formula w/ exponent of 2
# * Ralph Caola derived a more rigorous way of calculating extra wins needed for a run using calculus. Start from pythagorean formula:

# W = G * (R^2 / (R^2 + RA^2))
# 
# Take partial derivative of right side of equation w/r/t R, hold RA constant; result is incremental # of wins per run scored. Take the reciprocal of this, the result is # runs needed for an extra win. 
#
# R has function to calculate derivatives: #D

deriv_expression <- D(expression(G * R^2 / (R^2 + RA^2)), "R")
deriv_expression

# this leads to: IR/W = (R^2 + RA^2)^2 / (2 * G * R * RA^2)
# * If we express R and RA per game, we can eliminate G. Using that we can calculate incremental runs needed per one win for various runs scored/allowed situations.

IR <- function(RS=5, RA=5) {
	round((RS^2 + RA^2)^2 / (2 * RS * RA^2), 1) 
}

# We can use this to create a table for RS/RA situations. #seq and #expand.grid are functions that can help us do this.
# * #seq creates a vector of values within a range given an increment.
# * #expand.grid creates a dataframe from all combinations of elements of supplied vectors.

IRtable <- expand.grid(RS=seq(3, 6, 0.5), RA=seq(3, 6, 0.5))
rbind(head(IRtable), tail(IRtable))

# Now calculate incremental runs for the scenarios

IRtable$IRW <- IR(IRtable$RS, IRtable$RA)
xtabs(IRW ~ RS + RA, data=IRtable)

# Interpretation: In very low-scoring environments, a comparatively low number of incremental runs is needed for a win (~6). In normal environments, ~10 incremental runs is needed for a win. 