setwd("~/Documents/AllThingsR/wesnoth")
library(tidyverse)
library(lubridate)
library(rebus)


load_wesnoth <- function(path = 'Wesv15.csv'){
    
    data <- read_csv(path)

    player_lookup <- data %>% 
                    select(player) %>% 
                    unique() %>% 
                    mutate(player_id = row_number(),
                           player_id = factor(player_id))
    
    era_lookup <- data %>% 
                    select(era) %>% 
                    unique() %>% 
                    mutate(era_id = row_number(),
                           era_id = factor(era_id)) 
    
    map_lookup <- data %>% 
        select(map) %>% 
        unique() %>% 
        mutate(map_id = row_number(),
               map_id = factor(map_id)) 
    
    color_lookup <- data %>% 
        select(color) %>% 
        unique() %>% 
        mutate(color_id = row_number(),
               color_id = factor(color_id)) 
    
    faction_lookup <- data %>% 
        select(faction) %>% 
        unique() %>% 
        mutate(faction_id = row_number(),
               faction_id = factor(faction_id))
    
    leader_lookup <- data %>% 
        select(leader) %>% 
        unique() %>% 
        mutate(leader_id = row_number(),
               leader_id = factor(leader_id))
    
    team_lookup <- data %>% 
        select(team) %>% 
        unique() %>% 
        mutate(team_id = row_number(),
               team_id = factor(team_id))
    
    version_lookup <- data %>% 
        select(version) %>% 
        unique() %>% 
        mutate(version_id = row_number(),
               version_id = factor(version_id))
    
    data <- data %>% 
        select(-c('year','mon','wday' )) %>% 
        left_join(player_lookup, by = 'player') %>% 
        select(-player) %>% 
        left_join(color_lookup, by = 'color') %>% 
        select(-color) %>% 
        left_join(era_lookup, by = 'era') %>% 
        select(-era) %>% 
        left_join(map_lookup, by = 'map') %>% 
        select(-map) %>% 
        left_join(faction_lookup, by = 'faction') %>% 
        select(-faction) %>% 
        left_join(leader_lookup, by = 'leader') %>% 
        select(-leader) %>% 
        left_join(team_lookup, by = 'team') %>% 
        select(-team) %>% 
        left_join(version_lookup, by = 'version') %>% 
        select(-version) %>% 
        left_join(player_lookup %>% 
            rename(winner_id = player_id), by = c('winner' = 'player')) %>% 
        select(-winner) %>% 
        left_join(player_lookup %>% 
                      rename(loser_id = player_id), by = c('loser' = 'player')) %>% 
        select(-loser)
    
    game_info <- data %>% 
        group_by(game_id) %>% 
        summarise(turns = first(turns),
                  date = first(date),
                  map_id = first(map_id),
                  title = first(title),
                  game_file = first(game_file),
                  num_players = max(number)) %>%
        mutate(date = ymd_hm(date))
    
    elos <- data %>% 
            select(player_id = winner_id,
                   elo = winner_elo,
                   date) %>% 
            union(data %>% 
                      select(player_id = loser_id,
                             elo = loser_elo,
                             date))
    
    two_player_game_ids <- game_info %>% 
        filter(num_players == 2) %>% 
        pull(game_id)
    
    two_player_games <- data %>% 
            filter(game_id %in% two_player_game_ids) %>% 
            group_by(game_id) %>% 
            summarise(first_player_id = first(player_id),
                      second_player_id = last(player_id),
                      first_player_wins = if_else(first_player_id == first(winner_id), 1, 0))
    
    player_statistics_cols <- c('stats.cost', 
                                'stats.inflicted_actual',
                                'stats.inflicted_expected', 
                                'stats.taken_actual', 
                                'stats.taken_expected')
    
    player_game_statistics <- data %>% 
        select(game_id, player_id, color_id, team_id,
               cost = stats.cost,
               infliced_expected = stats.inflicted_expected,
               inflicted_actual = stats.inflicted_actual,
               taken_expected = stats.taken_expected,
               taken_actual = stats.taken_actual
               )
    
    statistic_pattern <- START %R% 'stats.' %R%
                            capture(one_or_more(WRD)) %R%
                            '.' %R% capture(one_or_more(WRD)) %R% END
    
    
    player_unit_statistics <- data %>% 
        select(game_id, player_id, starts_with('stats')) %>% 
        select(-player_statistics_cols) %>% 
        gather(key = description, value = number, -game_id,-player_id) %>% 
        filter(!is.na(number)) %>% 
        mutate(statistic = str_match(description, statistic_pattern)[,2],
               unit = str_match(description, statistic_pattern)[,3]) %>% 
        select(-description)
    
    
    units_lookup <- player_unit_statistics %>% 
        select(unit) %>% 
        unique() %>% 
        mutate(unit_id = row_number(),
               unit_id = factor(unit_id))
    
    player_unit_statistics <- player_unit_statistics %>% 
        left_join(units_lookup, by = c('unit')) %>% 
        select(-unit)
    
    advances_stats <- player_unit_statistics %>% 
            filter(statistic == "advances") %>% 
            select(-statistic)
    
    deaths_stats <-  player_unit_statistics %>% 
        filter(statistic == "deaths") %>% 
        select(-statistic)
    
    kills_stats <-  player_unit_statistics %>% 
        filter(statistic == "kills") %>% 
        select(-statistic)
    
    recruits_stats <- player_unit_statistics %>% 
        filter(statistic == "recruits") %>% 
        select(-statistic)
    
    to_export <- list('player_lookup'= player_lookup,
                      'era_lookup' = era_lookup,
                      'map_lookup' = map_lookup, 
                      'color_lookup' = color_lookup, 
                      'faction_lookup'=faction_lookup, 
                      'team_lookup' = team_lookup, 
                      'leader_lookup' = leader_lookup, 
                      'version_lookup' = version_lookup,
                      'units_lookup' = units_lookup,
                      'game_info' = game_info, 
                      'two_player_games' = two_player_games, 
                      'player_game_statistics' = player_game_statistics, 
                      'elos' = elos, 
                      'advances_stats' = advances_stats, 
                      'deats_stats' = deaths_stats,
                      'kills_stats' = kills_stats, 
                      'recruits_stats' = recruits_stats)
    list2env(to_export, envir = .GlobalEnv)
}

load_wesnoth()
