hr.rate <- function(age, hr, ab) { 
	rates <- round(100 * hr / ab, 1)
	list(x=age, y=rates)
}

HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age  <- 19 : 29
HR.Rates <- hr.rate(Age, HR, AB)
Mantle <- cbind(Age, HR, AB, Rates=HR.Rates$y)

write.csv(Mantle, "/Users/jknabl/Documents/code-projects/baseball_R/data/mantle.csv")