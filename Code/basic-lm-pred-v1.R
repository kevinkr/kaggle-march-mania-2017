# Kaggle March Learning Mania 2017
# start 2-10-17
# Jared Cross model from 2016 (https://www.kaggle.com/jaredcross/march-machine-learning-mania-2016/getting-started)
#
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

# Joining games with team seed
temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")
games.to.predict <- games.to.predict %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))

# Joining (compact) Results with Team Seeds
temp <- left_join(as.data.frame(TourneyCompactResults), TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))
head(compact.results)

# Every win for one team is a loss for the other team…
set1 <- compact.results %>% select(SeedNum.x, SeedNum.y) %>% mutate(result=1)
set2 <- compact.results %>% select(SeedNum.y, SeedNum.x) %>% mutate(result=0)
colnames(set1) <- c("team1seed", "team2seed", "team1win")
colnames(set2) <- c("team1seed", "team2seed", "team1win")
full.set <- rbind(set1, set2)
full.set <- full.set %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))

# Building a Simple Linear Model Based on the Difference in Team Seeds
m.seed.diff <- lm(team1win~ I(team2seed-team1seed), data=full.set)
summary(m.seed.diff)

# Making Predictions using the Team Seeds Model
games.to.predict$Pred <- predict(m.seed.diff, games.to.predict)
write.csv(games.to.predict %>% select(Id, Pred), 'Submissions/simple-linear-model-diff-team-seeds-v1-2-13-17.csv', row.names=FALSE)

# 2-13-17 PL 0.579107