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
