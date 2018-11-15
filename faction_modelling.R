library(caret)
library(glmnet)
library(ROCR)
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
    select(final_factor, faction_win)
    
