# Kaggle March Learning Mania 2017
# start 2-10-17
# Jared Cross model from 2016
library(data.table)
library(dplyr)
library(reshape2)

TourneySeeds <- fread("Data/TourneySeeds.csv")
SampleSubmission <- fread("Data/sample_submission.csv")
Seasons <- fread("Data/Seasons.csv")
Teams <- fread("Data/Teams.csv")
TourneySlots <- fread("Data/TourneySlots.csv")
TourneyDetailedResults <- fread("Data/TourneyDetailedResults.csv")
TourneyCompactResults <- fread("Data/TourneyCompactResults.csv")

head(TourneySeeds)
head(TourneySlots)
head(SampleSubmission)
head(Seasons)
head(Teams)
head(TourneyDetailedResults)
head(TourneyCompactResults)

# Extract tourney seeds
TourneySeeds <- TourneySeeds %>% 
  mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)

head(TourneySeeds)

# Extract games to predict
games.to.predict <- cbind(SampleSubmission$id, colsplit(SampleSubmission$id, 
                          "_", names = c('season', 'team1', 'team2')))   
head(games.to.predict)

