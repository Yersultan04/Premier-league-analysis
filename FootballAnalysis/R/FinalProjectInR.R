install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
library(readr)
library(dplyr)
library(ggplot2)



# Load the dataset
football_data <- read_csv("premier-league-matches.csv")

# View the first few rows of the dataset
str(football_data)

#head of dataset
head(football_data)

#summary statistics
summary(football_data)

#check missing value
sum(is.na(football_data))

football_data$Date <- as.Date(football_data$Date)

football_data$FTR <- as.factor(football_data$FTR)

#Creating New Variables:
football_data$TotalGoals <- football_data$HomeGoals + football_data$AwayGoals
football_data$HomeWin <- football_data$FTR == 'H'

#Remove Unnecessary Columns
football_data <- select(football_data, -Wk, -Season_End_Year)


#Data Sorting and Filtering
football_data <- filter(football_data, Date >= as.Date("2000-01-01"))
football_data <- arrange(football_data, Date)


# Export Cleaned Data
write_csv(football_data, "cleaned_football_data.csv")

#Exploratory Data Analysis (EDA)
cleaned_football_data <- read_csv("cleaned_football_data.csv")

# Descriptive Statistics
# summary of home goals
summary(cleaned_football_data)
#mean of home goals and median of home goals
mean(cleaned_football_data$HomeGoals)
median(cleaned_football_data$HomeGoals)
sd(cleaned_football_data$HomeGoals)

# mean of away goals and median of away goals
mean(cleaned_football_data$AwayGoals)
median(cleaned_football_data$AwayGoals)
sd(cleaned_football_data$AwayGoals)

# mean of total goals and median of total goals
mean(cleaned_football_data$TotalGoals)
median(cleaned_football_data$TotalGoals)
sd(cleaned_football_data$TotalGoals)


# Data Visualization

# Plot of Total goals
ggplot(cleaned_football_data, aes(x = TotalGoals)) + geom_histogram(binwidth = 1)

# Visualize the number of home wins over time.
cleaned_football_data %>%
  group_by(Date) %>%
  summarize(HomeWins = sum(HomeWin)) %>%
  ggplot(aes(x = Date, y = HomeWins)) + geom_line()


# Correlation Analysis:
cor(cleaned_football_data$HomeGoals, cleaned_football_data$AwayGoals)


# Convert Date to Date type and extract Year
football_data$Year <- format(as.Date(football_data$Date), "%Y")

# Calculate average goals per year
avg_goals_per_year <- aggregate(TotalGoals ~ Year, data = football_data, FUN = mean)

# Calculate home and away wins per year
home_wins <- aggregate(HomeWin ~ Year, data = football_data, FUN = sum)
away_wins <- aggregate(FTR == "A" ~ Year, data = football_data, FUN = sum)

# Merge home and away wins data for comparison
win_comparison <- merge(home_wins, away_wins, by = "Year")

# Convert factors to numeric if they are not
avg_goals_per_year$Year <- as.numeric(as.character(avg_goals_per_year$Year))
avg_goals_per_year$TotalGoals <- as.numeric(avg_goals_per_year$TotalGoals)

# Visualizing the Trends plot for average goals per year
ggplot(avg_goals_per_year, aes(x = Year, y = TotalGoals)) +
  geom_line() +
  labs(title = "Average Goals per Year", x = "Year", y = "Average Goals")


# Lets do specific analysis of Chelsea football club (I'm fan of Chelsea)
# Subset data for Chelsea's matches
chelsea_matches <- subset(football_data, Home == 'Chelsea' | Away == 'Chelsea')
total_matches <- nrow(chelsea_matches)

# Calculate Chelsea's win/loss/draw rate
chelsea_results <- with(chelsea_matches, {
  home_wins = sum(Home == 'Chelsea' & FTR == 'H')
  away_wins = sum(Away == 'Chelsea' & FTR == 'A')
  draws = sum(FTR == 'D')
  home_losses = sum(Home == 'Chelsea' & FTR == 'A')
  away_losses = sum(Away == 'Chelsea' & FTR == 'H')
  data.frame(Wins = home_wins + away_wins, Losses = home_losses + away_losses, Draws = draws, Total = total_matches)
})

# Calculate average goals scored by and against Chelsea
chelsea_goals <- with(chelsea_matches, {
  goals_for_home = sum(HomeGoals[Home == 'Chelsea'])
  goals_against_home = sum(AwayGoals[Home == 'Chelsea'])
  goals_for_away = sum(AwayGoals[Away == 'Chelsea'])
  goals_against_away = sum(HomeGoals[Away == 'Chelsea'])
  total_goals_for = goals_for_home + goals_for_away
  total_goals_against = goals_against_home + goals_against_away
  average_goals_for = total_goals_for / total_matches
  average_goals_against = total_goals_against / total_matches
  data.frame(TotalGoalsFor = total_goals_for, TotalGoalsAgainst = total_goals_against,
             AvgGoalsFor = average_goals_for, AvgGoalsAgainst = average_goals_against)
})


# Output the results
chelsea_results
chelsea_goals


# Define the teams for head-to-head comparison
opponents <- c('Manchester Utd', 'Arsenal', 'Liverpool', 'Manchester City', 'Tottenham')

# Function to get head-to-head stats
# Function to get head-to-head stats
get_head_to_head_stats <- function(team, opponent, data) {
  matches <- subset(data, (data$Home == team & data$Away == opponent) | (data$Home == opponent & data$Away == team))

  chelsea_wins <- sum((matches$Home == team & matches$FTR == 'H') | (matches$Away == team & matches$FTR == 'A'), na.rm = TRUE)
  opponent_wins <- sum((matches$Home == opponent & matches$FTR == 'H') | (matches$Away == opponent & matches$FTR == 'A'), na.rm = TRUE)
  draws <- sum(matches$FTR == 'D', na.rm = TRUE)

  data.frame(Team = team, Opponent = opponent, Wins = chelsea_wins, Losses = opponent_wins, Draws = draws)
}


# Loop over the opponents and bind the results
head_to_head_stats <- do.call(rbind, lapply(opponents, function(opponent) get_head_to_head_stats('Chelsea', opponent, football_data)))

# Output the head-to-head stats
head_to_head_stats


# Performance Over Time

# Ensure 'Date' column is in Date format and extract the season year
# Assuming that the season starts in August and ends in May of the next year
football_data$Date <- as.Date(football_data$Date, format = "%Y-%m-%d")
football_data$Season <- ifelse(format(football_data$Date, "%m") >= 8,
                               as.numeric(format(football_data$Date, "%Y")) + 1,
                               as.numeric(format(football_data$Date, "%Y")))

# Subset Chelsea matches
chelsea_matches <- subset(football_data, Home == 'Chelsea' | Away == 'Chelsea')

# Define a helper function to create a season identifier based on the date
get_season <- function(date) {
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  if (month >= 8) { # If the month is August or later, it's the next year's season
    return(paste(year, year + 1, sep="-"))
  } else {
    return(paste(year - 1, year, sep="-"))
  }
}

chelsea_matches$Season <- as.factor(sapply(chelsea_matches$Date, get_season))

# Calculate performance metrics per season
chelsea_season_stats <- aggregate(cbind(Wins = HomeWin, GoalsFor = TotalGoals) ~ Season, data = chelsea_matches[chelsea_matches$Home == 'Chelsea',], sum)
away_stats <- aggregate(cbind(Wins = (FTR == "A"), GoalsFor = AwayGoals) ~ Season, data = chelsea_matches[chelsea_matches$Away == 'Chelsea',], sum)
chelsea_season_stats <- merge(chelsea_season_stats, away_stats, by = "Season")
chelsea_season_stats$TotalWins <- with(chelsea_season_stats, Wins.x + Wins.y)
chelsea_season_stats$TotalGoalsFor <- with(chelsea_season_stats, GoalsFor.x + GoalsFor.y)

# Calculate win rates and average goals
chelsea_season_stats$TotalMatches <- aggregate(FTR ~ Season, data = chelsea_matches, length)$FTR
chelsea_season_stats$WinRate <- with(chelsea_season_stats, TotalWins / TotalMatches)
chelsea_season_stats$AvgGoalsFor <- with(chelsea_season_stats, TotalGoalsFor / TotalMatches)

# Order the data frame by Season for chronological plotting
chelsea_season_stats <- chelsea_season_stats[order(as.numeric(substring(chelsea_season_stats$Season, 1, 4))), ]

# Output the season stats
chelsea_season_stats


# Visualize chelsea season statistics
# Plot for win rate over seasons
ggplot(chelsea_season_stats, aes(x = Season, y = WinRate)) +
  geom_line(group=1, color="blue") +
  geom_point(color="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Chelsea Win Rate Over Seasons",
       x = "Season",
       y = "Win Rate")


# Plot for average goals scored per match over seasons
ggplot(chelsea_season_stats, aes(x = Season, y = AvgGoalsFor)) +
  geom_line(group=1, color="green") +
  geom_point(color="orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Chelsea Average Goals Scored Per Match Over Seasons",
       x = "Season",
       y = "Average Goals Scored")


# Filter the dataset for Chelsea's matches in the 2023 season
chelsea_matches_2023 <- subset(football_data, (Home == 'Chelsea' | Away == 'Chelsea') & Season == 2023)

# Create a new column for the number of goals Chelsea scored in each match
chelsea_matches_2023$ChelseaGoals <- ifelse(chelsea_matches_2023$Home == 'Chelsea',
                                            chelsea_matches_2023$HomeGoals,
                                            chelsea_matches_2023$AwayGoals)

# Create a scatter plot with ggplot2
ggplot(chelsea_matches_2023, aes(x = Date, y = ChelseaGoals)) +
  geom_point() +
  labs(title = "Chelsea Goals in 2023", x = "Date", y = "Goals Scored") +
  theme_minimal()


# Add a column to the dataset that represents Chelsea's goals in each match
football_data$ChelseaGoals <- ifelse(football_data$Home == 'Chelsea',
                                     football_data$HomeGoals,
                                     ifelse(football_data$Away == 'Chelsea',
                                            football_data$AwayGoals,
                                            NA))

# Filter out the matches that don't involve Chelsea
chelsea_goals <- na.omit(football_data$ChelseaGoals)

# Create a box plot with ggplot2
ggplot(data = football_data, aes(x = factor(0), y = ChelseaGoals)) +
  geom_boxplot() +
  labs(title = "Distribution of Goals Scored by Chelsea Over the Period",
       x = "Chelsea",
       y = "Goals Scored") +
  theme_minimal()



# Top teams over the period

# Add points for home and away teams for each match
football_data$HomePoints <- ifelse(football_data$FTR == "H", 3, ifelse(football_data$FTR == "D", 1, 0))
football_data$AwayPoints <- ifelse(football_data$FTR == "A", 3, ifelse(football_data$FTR == "D", 1, 0))

# Sum points for each team
home_points <- aggregate(HomePoints ~ Home, data = football_data, FUN = sum)
away_points <- aggregate(AwayPoints ~ Away, data = football_data, FUN = sum)

# Rename columns for merging
names(home_points) <- c("Team", "Points")
names(away_points) <- c("Team", "Points")

# Merge the two datasets
total_points <- merge(home_points, away_points, by = "Team")

# Calculate total points
total_points$TotalPoints <- total_points$Points.x + total_points$Points.y

# Rank teams by total points
ranked_teams <- total_points[order(-total_points$TotalPoints), ]
ranked_teams$Rank <- rank(-ranked_teams$TotalPoints, ties.method = "min")

# View the top-ranked team
top_team <- ranked_teams[1, ]
top_team

# Top 5 teams and bottom 5 teams

# Get the top 5 teams
top_teams <- head(ranked_teams, 5)

# Get the bottom 5 teams
bottom_teams <- tail(ranked_teams, 5)

# Output the top 5 and bottom 5 teams
list(Top5 = top_teams, Bottom5 = bottom_teams)

#Bar chart visualization of top 5 and bottom 5 teams

# Assuming 'ranked_teams' has already been created and contains the 'TotalPoints' column

# Combine them into one data frame for plotting
visualization_teams <- rbind(top_teams, bottom_teams)

# Add a factor to differentiate the top and bottom teams
visualization_teams$Type <- rep(c("Top 5", "Bottom 5"), each = 5)

# Create a bar plot
ggplot(visualization_teams, aes(x = reorder(Team, TotalPoints), y = TotalPoints, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() + # Flip coordinates for a horizontal bar chart
  labs(title = "Top 5 and Bottom 5 Football Teams by Total Points",
       x = "Team",
       y = "Total Points") +
  scale_fill_manual(values = c("Top 5" = "blue", "Bottom 5" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Display the plot
ggsave("team_points_comparison.png", width = 10, height = 8)


# 2023 Table

# Filter the data for the 2023 season
season_2023 <- subset(football_data, Season == 2023)

# Calculate points, goal difference and total goals for each team
calculate_metrics <- function(data, team_column) {
  points <- ifelse(data$FTR == 'H' & data$Home == team_column, 3,
                   ifelse(data$FTR == 'A' & data$Away == team_column, 3,
                          ifelse(data$FTR == 'D', 1, 0)))

  goals_for <- ifelse(data$Home == team_column, data$HomeGoals, data$AwayGoals)
  goals_against <- ifelse(data$Home == team_column, data$AwayGoals, data$HomeGoals)

  data.frame(Team = team_column,
             Points = sum(points, na.rm = TRUE),
             GoalDifference = sum(goals_for, na.rm = TRUE) - sum(goals_against, na.rm = TRUE),
             TotalGoals = sum(goals_for, na.rm = TRUE))
}

teams <- unique(c(season_2023$Home, season_2023$Away))
team_metrics <- do.call("rbind", lapply(teams, calculate_metrics, data = season_2023))

# Sort the teams by points, then goal difference, then total goals
premier_league_table_2023 <- team_metrics[order(-team_metrics$Points, -team_metrics$GoalDifference, -team_metrics$TotalGoals), ]

# Add a rank column
premier_league_table_2023$Rank <- 1:nrow(premier_league_table_2023)

# Display the Premier League table for 2023
premier_league_table_2023


#Home and Away goals Plot

# Assuming 'season_2023' data is already filtered for the 2023 season

# Calculate total home and away goals for each team
home_goals <- aggregate(HomeGoals ~ Home, data = season_2023, sum)
away_goals <- aggregate(AwayGoals ~ Away, data = season_2023, sum)

# Rename columns for clarity
names(home_goals) <- c("Team", "TotalHomeGoals")
names(away_goals) <- c("Team", "TotalAwayGoals")

# Merging home and away goals data
team_goals <- merge(home_goals, away_goals, by = "Team")

# Plot for Home Goals
ggplot(team_goals, aes(x = reorder(Team, TotalHomeGoals), y = TotalHomeGoals)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() + # Flip for horizontal bars
  labs(title = "Total Home Goals by Team in Premier League 2023",
       x = "Team",
       y = "Total Home Goals") +
  theme_minimal()

# Plot for Away Goals
ggplot(team_goals, aes(x = reorder(Team, TotalAwayGoals), y = TotalAwayGoals)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() + # Flip for horizontal bars
  labs(title = "Total Away Goals by Team in Premier League 2023",
       x = "Team",
       y = "Total Away Goals") +
  theme_minimal()



total_home_goals <- sum(football_data$HomeGoals, na.rm = TRUE)
total_away_goals <- sum(football_data$AwayGoals, na.rm = TRUE)

# Create a data frame for the pie chart
goals_data <- data.frame(
  Category = c("Home Goals", "Away Goals"),
  Count = c(total_home_goals, total_away_goals)
)

# Create a pie chart
ggplot(goals_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Goal Type") +
  theme(legend.position = "right")
