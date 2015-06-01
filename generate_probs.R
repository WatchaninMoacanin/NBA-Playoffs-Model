### generate_probs.R

# This script will generate probabilities for each team to reach the second round,
# Conference finals, NBA final, and winning the NBA Championship.
# The script will generate these probabilities for several years and output them
# to a single file.


###########################
## Set working directory ##
###########################
setwd("~/Documents/Watchanin Moacanin/Projects/2015 NBA Playoffs Model/")

##############################
## Load necessary libraries ## --------------------------------------------------------------------------
##############################
library(PlayerRatings)
library(knitr)

#############################
#####                   #####
##### SET EVERYTHING UP ##### ---------------------------------------------------------------------------
#####                   #####
#############################

# File Name
file_name <- "results_2015.csv"

# Set number of simulations at 15,000
num_sims <- 15000

east.1 <- "Atlanta Hawks"
east.2 <- "Cleveland Cavaliers"
east.3 <- "Chicago Bulls"
east.4 <- "Toronto Raptors"
east.5 <- "Washington Wizards"
east.6 <- "Milwaukee Bucks"
east.7 <- "Boston Celtics"
east.8 <- "Brooklyn Nets"

west.1 <- "Golden State Warriors"
west.2 <- "Houston Rockets"
west.3 <- "Los Angeles Clippers"
west.4 <- "Portland Trail Blazers"
west.5 <- "Memphis Grizzlies"
west.6 <- "San Antonio Spurs"
west.7 <- "Dallas Mavericks"
west.8 <- "New Orleans Pelicans"

###################################
#####                         #####
##### ALL NECESSARY FUNCTIONS ##### ---------------------------------------------------------------------
#####                         #####
###################################

## convert_pct
convert_pct <- function(x)paste(round(100*x, 1), "%", sep="")

## simulate.series
simulate.series <- function(team1, team2){
  
  # Extract Ratings
  rating.1 <- ratings[ratings$team == team1, "rating"]
  rating.2 <- ratings[ratings$team == team2, "rating"]
  
  # Probabilities
  p.1.2 <- 1 / ( 10 ^ ((rating.2 - rating.1)/400) + 1 )
  p.2.1 <- 1 / ( 10 ^ ((rating.1 - rating.2)/400) + 1 )
  
  
  # Simulate Series 
  games.1 <- 0    # Initialize games won by Team 1 at 0
  games.2 <- 0    # Initialize games won by Team 2 at 0
  
  while (games.1 < 4 & games.2 < 4){
    
    game.result <- sample(c(team1, team2), size=1, prob=c(p.1.2, p.2.1), replace=TRUE)
    
    if (game.result==team1){
      games.1 <- games.1 + 1
    } else {
      games.2 <- games.2 + 1
    }
  }
  
  if (games.1 == 4) {
    winner = team1
  } else {
    winner = team2
  }
  winner
}

## chance.df
chance.df <- function(series){
  
  tbl <- table(sim.results.df[ , series])
  df <- data.frame(team = names(tbl), chance = as.numeric(tbl)/sum(tbl))
  df <- df[order(df$chance, decreasing=TRUE), ]
  df
}

#####################################
#####                           #####
##### Read and Process the data ##### ---------------------------------------------------------------
#####                           #####
#####################################

data <- read.csv(file_name)
data <- data[, c("Date", "Visitor.Neutral", "PTS", "Home.Neutral", "PTS.1")]
names(data) <- c("date", "away", "pts.away", "home", "pts.home")
data$result <- ifelse(data$pts.away > data$pts.home, 1, 0)

# Get rid of point totals
data <- data[,c("date", "away", "home", "result")]

# Update the dates
data$date <- as.character(data$date)     # Convert dates to character
new.dates <- c()
for (i in 1:nrow(data)){
  
  date.i <- data$date[i]
  parts.i <- strsplit(date.i, " ")
  date.i <- paste(parts.i[[1]][2], parts.i[[1]][3],  parts.i[[1]][4], sep=" ")
  new.date <- as.Date(date.i, "%b %d %Y")
  new.dates <- c(new.dates, new.date)
}
# Add new date format to data frame
data$date <- new.dates

# Convert teams to characters
data$away <- as.character(data$away)
data$home <- as.character(data$home)

##############################
#####                    #####    
##### Create the Ratings ##### ----------------------------------------------------------------------
#####                    #####   
##############################

ratings.elo <- elo(data)
ratings <- ratings.elo$ratings
ratings <- ratings[,c("Player", "Rating")]
names(ratings) <- c("team","rating")

###############################
#####                     #####
##### Run the Simulations ##### ------------------------------------------------------------------------
#####                     #####
###############################

set.seed(23)

simulation.results <- c()

i = 1
while (i <= num_sims){
  
  # Eastern Conference First Round
  series.1 <- simulate.series(east.1, east.8)
  series.2 <- simulate.series(east.4, east.5)
  series.3 <- simulate.series(east.3, east.6)
  series.4 <- simulate.series(east.2, east.7)
  
  # Western Conference First Round
  series.5 <- simulate.series(west.1, west.8)
  series.6 <- simulate.series(west.4, west.5)
  series.7 <- simulate.series(west.3, west.6)
  series.8 <- simulate.series(west.2, west.7)
  
  # Eastern Conference Semi-Finals
  series.9 <- simulate.series(series.1, series.2)
  series.10 <- simulate.series(series.3, series.4)
  
  # Western Conference Semi-Finals
  series.11 <- simulate.series(series.5, series.6)
  series.12 <- simulate.series(series.7, series.8)
  
  # Eastern Conference Finals
  series.13 <- simulate.series(series.9, series.10)
  
  # Western Conference Finals
  series.14 <- simulate.series(series.11, series.12)
  
  # Finals
  series.15 <- simulate.series(series.13, series.14)
  
  results.all <- c( 
    i,
    series.1, series.2, series.3, series.4,
    series.5, series.6, series.7, series.8,
    series.9, series.10,
    series.11, series.12,
    series.13,
    series.14,
    series.15
  )
  simulation.results <- c(simulation.results, results.all)
  
  i = i + 1
}

sim.results.mat <- matrix(simulation.results, ncol=16, byrow=TRUE)
sim.results.df <- as.data.frame(sim.results.mat)
names(sim.results.df) <- c( 
  "sim",
  "series.1", "series.2", "series.3", "series.4",
  "series.5", "series.6", "series.7", "series.8",
  "series.9", "series.10",
  "series.11", "series.12",
  "series.13",
  "series.14",
  "series.15"
)


#################################################
#####                                       #####
##### Create a table with all probabilities ##### -----------------------------------------------------------------------------------------------
#####                                       #####
#################################################

# NBA Champions
champs.df <- chance.df("series.15")

# Conference Champions
west.champs.df <- chance.df("series.14")
east.champs.df <- chance.df("series.13")
finals <- rbind(west.champs.df, east.champs.df)

# Conference Finals
east.1.df <- chance.df("series.9")
east.2.df <- chance.df("series.10")
west.1.df <- chance.df("series.11")
west.2.df <- chance.df("series.12")
conf.finals <- rbind(east.1.df, east.2.df, west.1.df, west.2.df)

# Second Round
east.1.two <- chance.df("series.1")
east.2.two <- chance.df("series.2")
east.3.two <- chance.df("series.3")
east.4.two <- chance.df("series.4")
west.1.two <- chance.df("series.5")
west.2.two <- chance.df("series.6")
west.3.two <- chance.df("series.7")
west.4.two <- chance.df("series.8")
round.2.df <- rbind(east.1.two, east.2.two, east.3.two, east.4.two,
                    west.1.two, west.2.two, west.3.two, west.4.two)

# Merge all probabilities
all.chances.df <- merge(round.2.df, conf.finals, by="team")
names(all.chances.df) <- c("team", "round.2", "conf.finals")
all.chances.df <- merge(all.chances.df, finals, by="team")
all.chances.df <- merge(all.chances.df, champs.df, by="team")
names(all.chances.df) <- c("team", "round.2", "conf.finals", "finals", "champs")
all.chances.df <- all.chances.df[order(all.chances.df$champs, decreasing=TRUE), ]
all.chances.df[,2:5] <- sapply(all.chances.df[,2:5], convert_pct)

# View results
kable(all.chances.df)

# Write to a file
output_filename <- "playoff_probs_2015.csv"
write.csv(all.chances.df, output_filename, row.names=FALSE)
