source('tidy_data.R')
load_wesnoth()

two_player_games %>% glimpse()
player_game_statistics %>% glimpse()

# To find which faction which player used in which game
game_factions <- player_game_statistics %>% 
    select(game_id, player_id, faction)

# Map on each game
map_lookup <- game_info %>%
    select(game_id, map)

# Find the Map with the most games. Not many games present, probably average over Maps
map_lookup %>% 
    group_by(map) %>% 
    summarise(num_games = n()) %>% 
    arrange(desc(num_games))

# Generate statistics
two_player_games %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
    rename(first_faction = faction) %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
    rename(second_faction = faction) %>% 
    # left_join(map_lookup, by = c('game_id')) %>% 
    group_by( first_faction, second_faction) %>% 
    summarise(wins = sum(first_player_wins),
              total_games = n())
