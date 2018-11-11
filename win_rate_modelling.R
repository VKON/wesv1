library(caret)
library(ROCR)
source('tidy_data.R')
load_wesnoth()



elo_set <- two_player_games %>%
    left_join(game_info, by = c('game_id')) %>% 
    select(game_id, first_player_id, second_player_id, first_player_wins, date = date) %>% 
    left_join(elos, by = c('first_player_id' = 'player_id', 'date' = 'date')) %>% 
    rename(first_player_elo = elo) %>% 
    left_join(elos, by = c('second_player_id' = 'player_id', 'date' = 'date')) %>% 
    rename(second_player_elo = elo) %>% 
    mutate(elo_diff = first_player_elo - second_player_elo) %>% 
    mutate(first_player_wins = factor(first_player_wins))

elo_set %>% 
    ggplot(aes(x = elo_diff, y = as.numeric(first_player_wins))) + 
        geom_point() +
        geom_smooth()



train_index <- createDataPartition(elo_set$first_player_wins, p = .8, 
                                  list = FALSE, 
                                  times = 1)

x_train <- elo_set[train_index,] 
x_test <- elo_set[-train_index,]

model = glm(first_player_wins ~ elo_diff, family = 'binomial', data = x_train )
summary(model)

predicted <- predict(model, newdata = x_test, type = 'response')
pr <- prediction(predicted, x_test$first_player_wins)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = 'auc')
