library(caret)
library(glmnet)
library(ROCR)
library(broom)
source('tidy_data.R')
load_wesnoth()

game_factions <- player_game_statistics %>% 
    select(game_id, player_id, faction, leader)

faction_cleaned <-  two_player_games %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
    rename(fp_faction = faction, fp_leader = leader ) %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
    rename(sp_faction = faction, sp_leader = leader) %>% 
    mutate(fp_faction_leader = str_c(fp_faction,fp_leader),
           sp_faction_leader = str_c(sp_faction,sp_leader)) %>% 
    mutate(factor_relation = fp_faction_leader <= sp_faction_leader) %>% 
    mutate(final_factor = if_else(factor_relation, 
                                  str_c(fp_faction_leader, sp_faction_leader),
                                  str_c(sp_faction_leader, fp_faction_leader)),
           final_factor = factor(final_factor),
           faction_win = if_else(factor_relation,
                                 first_player_wins,
                                 1-first_player_wins)) %>% 
    filter(!is.na(faction_win)) %>% 
    select(final_factor, faction_win)
    
num_unique <- faction_cleaned %>% select(final_factor) %>% unique() %>% count()

naive_guess <- faction_cleaned %>% 
                group_by(final_factor) %>% 
                summarise(wins = sum(faction_win),
                          num_played = n()) %>% 
                mutate(adj_dev = 2*(wins - num_played*0.5)/sqrt(num_played))

naive_guess %>% 
  arrange(desc(abs(adj_dev))) %>% 
  select(final_factor, adj_dev) %>% 
  head(30) %>% 
  View()

lr_model <- glm(faction_win ~ final_factor, data = faction_cleaned, family = 'binomial')
tidy_model <- lr_model %>% tidy()

one_hot_encoder <- faction_cleaned %>% select(final_factor) %>% dummyVars(formula = '~.')
new_variables <- faction_cleaned %>% select(final_factor) %>% predict(one_hot_encoder, newdata = .)
new_variables <- as.matrix(new_variables)
y <- faction_cleaned %>% select(faction_win) %>% pull() %>% as.double()

# l1_model <- glmnet(x = new_variables, y = y, family="binomial",alpha=1)
cvfit <-  cv.glmnet(x = new_variables, y = y, family = 'binomial', alpha = 1)
lambda_min <- cvfit$lambda.min

# Extraction of different lambda parameter
extract_glmnet_coefs <- function(cvfit, s="lambda.min") {
  ind <- which(coef(cvfit, s=s) != 0)
  df <- tibble(
    feature=rownames(coef(cvfit, s=s))[ind],
    coeficient=coef(cvfit, s=s)[ind]
  ) %>% mutate(lambda = s,
               feature = str_replace(feature,'final_factor.',''))
  return(df)
}

# Create a sequence of powers of 10 to pass later as lambda parameters
pow <- function(a,b){
  a^b
}
powers <- seq(-10, 1,by = 1)
s <- map_dbl(s,~pow(a=10, .x))

extracted_list <- map(s,~extract_glmnet_coefs(cvfit, .x))
top_extracts <-  bind_rows(extracted_list)

top_extracts %>% 
  group_by(lambda) %>% 
  summarise(non_zero_params = n()) %>% 
  arrange(desc(lambda))

# Looks like the action is between s = 0.1 and s = 0.001
s <- seq(0.001,0.02,by = 0.001)

extracted_list <- map(s,~extract_glmnet_coefs(cvfit, .x))
top_extracts <-  bind_rows(extracted_list)

top_extracts %>% 
  group_by(lambda) %>% 
  summarise(non_zero_params = n()) %>% 
  arrange(desc(lambda)) %>% 
  head(30)

# Interesting values : s = 0.015, 0.016, 0.011, 0.01

start_stats <- player_game_statistics %>% 
                select(game_id, player_id, number)

two_player_games %>% 
  left_join(game_info %>% select(game_id, map), by = c('game_id')) %>% 
  left_join(start_stats, by = c( "game_id"="game_id", "first_player_id"="player_id")) %>% 
  rename('fp_number'='number') %>% 
  left_join(start_stats, by = c( "game_id"="game_id", "second_player_id"="player_id")) %>% 
  rename('sp_number'='number') %>% 
  filter(sp_number == 2)

two_player_games %>% 
  left_join(game_info %>% select(game_id, date), by = c('game_id')) %>% 
  left_join(elos %>% rename(elo_date = date), by = c('first_player_id'='player_id')) %>% 
  group_by(game_id, first_player_id) %>% 
  # Select for the latest Ranking
  filter(elo_date < date) %>% 
  filter(elo_date == max(elo_date)) %>% 
  rename(fp_elo = elo) %>% 
  select(-elo_date) %>% 
  left_join(elos %>% rename(elo_date = date), by = c('second_player_id'='player_id')) %>% 
  group_by(game_id, second_player_id) %>% 
  # Select for the latest Ranking
  filter(elo_date < date) %>% 
  filter(elo_date == max(elo_date)) %>% 
  rename(sp_elo = elo) %>% 
  select(-elo_date)  %>% 
  mutate(elo_diff = fp_elo - sp_elo,
         elo_diff = if_else(is.na(elo_diff), 0.0, elo_diff)) %>% 
  select(-c(date, fp_elo, sp_elo))


add_faction_parameters <- function(df, 
                                   faction_lookup = faction_cleaned, 
                                   statistics = player_game_statistics){
  # Select the relevant game statistics
  game_factions <- player_game_statistics %>% 
    select(game_id, player_id, faction)
  
  # Add the faction information 
  df <-  df %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
    rename(fp_faction = faction) %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
    rename(sp_faction = faction)
  
  
  
  df <- df %>% 
    mutate(faction_relation = as.character(fp_faction) <= as.character(sp_faction),
           faction_factor = if_else(faction_relation, 
                                    str_c(fp_faction,' - ', sp_faction),
                                    str_c(sp_faction,' - ', fp_faction))) %>% 
    left_join(faction_lookup, on ='faction_factor') %>% 
    mutate(faction_alpha = if_else(faction_relation, alpha, beta),
           faction_beta = if_else(faction_relation, beta, alpha)) %>% 
    # Set default values
    mutate(faction_alpha = if_else(is.na(faction_alpha), 1, faction_alpha),
           faction_beta = if_else(is.na(faction_beta), 1, faction_beta)) %>% 
    select(-c(alpha,beta,fp_faction, sp_faction, faction_relation, faction_factor ))
  return(df)
}



add_leader_parameters <- function(df, 
                                  leader_lookup = leader_cleaned, 
                                  statistics = player_game_statistics){
  # Select the relevant game statistics
  game_factions <- player_game_statistics %>% 
    select(game_id, player_id, leader)
  
  # Add the faction information 
  df <-  df %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
    rename(fp_leader = leader) %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
    rename(sp_leader = leader)
  
  
  
  df <- df %>% 
    mutate(leader_relation = as.character(fp_leader) <= as.character(sp_leader),
           leader_factor = if_else(leader_relation, 
                                   str_c(fp_leader,' - ', sp_leader),
                                   str_c(sp_leader,' - ', fp_leader))) %>% 
    left_join(leader_lookup, on ='leader_factor') %>% 
    mutate(leader_alpha = if_else(leader_relation, alpha, beta),
           leader_beta = if_else(leader_relation, beta, alpha)) %>% 
    # Set default values
    mutate(leader_alpha = if_else(is.na(leader_alpha), 1, leader_alpha),
           leader_beta = if_else(is.na(leader_beta), 1, leader_beta)) %>% 
    select(-c(alpha,beta,fp_leader, sp_leader, leader_relation, leader_factor ))
  return(df)
}

add_map_parameters <- function(df, 
                               map_lookup = map_cleaned, 
                               statistics = game_info){
  
  
  # Add the faction information 
  df <-  df %>% 
    left_join(statistics %>% select(game_id, map), by = c('game_id')) 
  
  print(colnames(df))
  
  df <- df %>% 
    left_join(map_lookup, by =c('map'))%>%
    mutate(map_alpha = alpha,
           map_beta = beta) %>%
    # Set default values
    mutate(map_alpha = if_else(is.na(map_alpha), 1, map_alpha),
           map_beta = if_else(is.na(map_beta), 1, map_beta)) %>%
    select(-c(alpha,beta ))
  return(df)
}

add_elo_diff <- function(df, game_info_tbl = game_info, elo_tbl = elos){
  
  df_with_features <- df %>%
    left_join(game_info_tbl %>% select(game_id, date) , by = c('game_id')) %>%
    left_join(elo_tbl %>% rename(elo_date = date), by = c('first_player_id'='player_id')) %>%
    group_by(game_id, first_player_id)%>%
    # Select for the latest Ranking
    filter(elo_date < date) %>%
    filter(elo_date == max(elo_date)) %>%
    rename(fp_elo = elo) %>%
    select(-elo_date) %>%
    left_join(elo_tbl %>% rename(elo_date = date), by = c('second_player_id'='player_id')) %>%
    group_by(game_id, second_player_id) %>%
    # Select for the latest Ranking
    filter(elo_date < date) %>%
    filter(elo_date == max(elo_date)) %>%
    rename(sp_elo = elo) %>%
    select(-elo_date)  %>%
    mutate(elo_diff = fp_elo - sp_elo,
           elo_diff = if_else(is.na(elo_diff), 0.0, elo_diff)) %>%
    select(-c(date, fp_elo, sp_elo)) %>% 
    ungroup()
  return(df_with_features)
}

prepare_data <- function(df){
  df <- add_map_parameters(df)
  df <- add_faction_parameters(df)
  df <- add_leader_parameters(df)
  
  df <- df %>% 
    mutate(leader_mu = leader_alpha/(leader_alpha+leader_beta),
           faction_mu = faction_alpha/(faction_alpha+faction_beta),
           map_mu = map_alpha/(map_alpha + map_beta),
           leader_evid = leader_alpha + leader_beta,
           faction_evid = faction_alpha + faction_beta,
           map_evid = map_alpha + map_beta,
           sum_evid = leader_evid + faction_evid + map_evid,
           leader_evid = leader_evid/sum_evid,
           faction_evid = faction_evid/sum_evid,
           map_evid = map_evid/sum_evid)
  
  df <- add_elo_diff(df)
  
  df <- df %>% 
    select(game_id, leader_mu, leader_evid, faction_mu, faction_evid,
           map_mu, map_evid, elo_diff)
  return(df)
}

elo_tbl <- elos
df <- train_set
game_info_tbl <- game_info


# Calculate the Median of all players as a default choice
median_elo <- elo_tbl %>% summarise(median_elo = median(elo)) %>% pull()

# Create defaults by setting a very early date. This is the easiest to prevent 
#  Double counting
default_elos <- df %>% 
  select(first_player_id, second_player_id) %>% 
  gather(key = player_position, value = player_id) %>% 
  select(player_id) %>% 
  distinct() %>% 
  mutate(player_id = factor(player_id),
         elo = median_elo,
         date = ymd_hms('1900-01-01 01:01:01'))

all_elos <- elos %>% 
  group_by(player_id, date) %>% 
  # Occasionally the same player has two elos for the same instance in time
  # This prevents that
  summarise(elo = median(elo)) %>% 
  union_all(default_elos) %>% 
  ungroup()


faction_lookup = faction_cleaned
statistics = player_game_statistics
  # Select the relevant game statistics
game_factions <- player_game_statistics %>% 
  select(game_id, player_id, faction)

df <- train_set
  # Add the faction information 
df <-  df %>% 
  left_join(game_factions, 
            by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
  rename(fp_faction = faction) %>% 
  left_join(game_factions, 
            by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
  rename(sp_faction = faction)
  
  
  
df %>% 
  mutate(faction_relation = as.character(fp_faction) <= as.character(sp_faction),
         faction_factor = if_else(faction_relation, 
                                  str_c(fp_faction,' - ', sp_faction),
                                  str_c(sp_faction,' - ', fp_faction))) %>% 
  left_join(faction_lookup, on ='faction_factor') %>% 
  mutate(faction_alpha = if_else(faction_relation, alpha, beta),
         faction_beta = if_else(faction_relation, beta, alpha)) %>% 
  # Set default values
  mutate(faction_alpha = if_else(is.na(faction_alpha), 1, faction_alpha),
         faction_beta = if_else(is.na(faction_beta), 1, faction_beta)) %>% 
  select(-c(alpha,beta,fp_faction, sp_faction, faction_relation, faction_factor ))


df <- train_set

df <- add_map_parameters(df)
df <- add_faction_parameters(df)
df <- add_leader_parameters(df)

df <- df %>% 
  mutate(leader_mu = leader_alpha/(leader_alpha+leader_beta),
         faction_mu = faction_alpha/(faction_alpha+faction_beta),
         map_mu = map_alpha/(map_alpha + map_beta),
         leader_evid = leader_alpha + leader_beta,
         faction_evid = faction_alpha + faction_beta,
         map_evid = map_alpha + map_beta,
         sum_evid = leader_evid + faction_evid + map_evid,
         leader_evid = leader_evid/sum_evid,
         faction_evid = faction_evid/sum_evid,
         map_evid = map_evid/sum_evid)



df <- add_elo_diff(df)



df <- df %>% 
  select(game_id, leader_mu, leader_evid, faction_mu, faction_evid,
         map_mu, map_evid, elo_diff)

tc <- trainControl("cv", 5)
lr_model <- train(factor(first_player_wins) ~ .,
             data      = train_features    ,
             method    = "glm"    ,
             family    = binomial ,
             trControl = tc)
predictions <- predict(fit, newdata = test_features, type = 'prob')[,2]
auc(test_set$first_player_wins, predictions)
