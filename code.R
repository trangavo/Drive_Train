# set directory and import data
setwd("~/Desktop/Fall 2019/CS112")
assignment1 <- read.csv("~/Desktop/Fall 2019/CS112/assignment1.0.csv", stringsAsFactors=FALSE)
assignment1
# numeric
assignment1[,8] <- as.numeric(assignment1[,8])
assignment1[,9] <- as.numeric(assignment1[,9])
assignment1[,10] <- as.numeric(assignment1[,10])
# date
assignment1[,4] <- 
  as.Date(assignment1[,4] - 35828,
          origin = as.Date('1998-02-02'))
assignment1[,5] <- 
  as.Date(assignment1[,5] - 35828,
          origin = as.Date('1998-02-02'))
assignment1[,6] <- 
  as.Date(assignment1[,6] - 35828,
          origin = as.Date('1998-02-02'))
assignment1[,7] <- 
  as.Date(assignment1[,7] - 35828,
          origin = as.Date('1998-02-02'))
# find my problems
set.seed(147)
my_questions_are_these <- sort(sample(c(1:10), 3, replace = FALSE))
my_questions_are_these
# get the relevant data
to_be_excluded <- which(assignment1[,4] < as.Date("1995-01-01") |
                          assignment1[,4] > as.Date("2016-12-31"))

assignment1 <- assignment1[-to_be_excluded, ]
# problem 4 - average number of projects approved each year
numberOfProjs = nrow(assignment1)
latestDate = max(assignment1[,4],na.rm = FALSE)
earliestDate = min(assignment1[,4],na.rm = FALSE)
latestYear <- as.numeric(substr(latestDate, 1,4))
earliestYear <- as.numeric(substr(earliestDate, 1,4))
numberOfYears <- latestYear - earliestYear + 1
numberOfProjsPerYear <- numberOfProjs / numberOfYears
numberOfProjsPerYear

# problem 6 - fraction with completion date != revised date
# only take projects with data about both original and revised date available
differentDates <- length(which(assignment1$original.completion.date !=
                                 assignment1$revised.completion.date))

fraction <- differentDates / numberOfProjs
fraction

# problem 10 - fraction of assessed projs
assignment1$revised.completion.year <- as.numeric(substr(assignment1$revised.completion.date,1,4))
year <- levels(factor(assignment1$revised.completion.year))

today <- as.Date("1996/1/1")
years <- c(seq(today, by="year", length=length(year)))
s <- list()
for (i in years) {
  exclude <- which(assignment1$revised.completion.date > as.Date(i, origin = as.Date('1970-01-01')) |
                     assignment1$revised.completion.date < as.Date(i, origin = as.Date('1969-01-01')))
  dataset <- assignment1[-to_be_excluded, ]
  s <- c(s, 1 - (sum(is.na(dataset$success.rating))/nrow(dataset)))
}
plot(years, s)

# so it has changed!
