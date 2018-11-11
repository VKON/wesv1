
library(tidyverse)
library(lubridate)
library(rebus)


load_wesnoth <- function(path = 'Wesv15.csv', unit_data_path = '.'){
    
    data <- read_csv(path)
    unit_data <- load_unit_data(unit_data_path)

    player_lookup <- data %>% 
                    select(player) %>% 
                    unique() %>% 
                    mutate(player_id = row_number(),
                           player_id = factor(player_id))
    
    
    data <- data %>% 
        mutate(date = ymd_hm(date)) %>% 
        select(-c('year','mon','wday' )) %>% 
        mutate(color = factor(color),
               map = factor(map),
               era = factor(era),
               faction = factor(faction),
               leader = factor(leader),
               team = factor(team),
               version = factor(version)) %>% 
        left_join(player_lookup, by = c('player')) %>% 
        select(-player) %>% 
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
                  map = first(map),
                  title = first(title),
                  game_file = first(game_file),
                  num_players = max(number))
    
    winner_elos <-  data %>% select(player_id = winner_id,
                           elo = winner_elo,
                           date)
    loser_elos <- data %>% 
        select(player_id = loser_id,
               elo = loser_elo,
               date)
    
    elos <- winner_elos%>% 
            bind_rows(loser_elos)
    
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
        select(game_id, player_id, color, team, faction, leader,
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
    
    
    units_lookup <- unit_data$unit_basic %>% 
        select(unit, unit_id) 
    
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
                      'units_lookup' = units_lookup,
                      'game_info' = game_info, 
                      'two_player_games' = two_player_games, 
                      'player_game_statistics' = player_game_statistics, 
                      'elos' = elos, 
                      'advances_stats' = advances_stats, 
                      'deats_stats' = deaths_stats,
                      'kills_stats' = kills_stats, 
                      'recruits_stats' = recruits_stats,
                      'unit_data'= unit_data)
    list2env(to_export, envir = .GlobalEnv)
}

load_unit_data <- function(path = '.'){
    unit_basic <- read_csv(paste(path,'/','unit_basic.csv', sep = ''))
    unit_attack<- read_csv(paste(path,'/','unit_attack.csv', sep = ''))
    unit_movement <- read_csv(paste(path,'/','unit_movement.csv', sep = ''))
    unit_resistance <- read_csv(paste(path,'/','unit_resistance.csv', sep = ''))
    return (list('unit_basic' = unit_basic, 'unit_attack' = unit_attack, 
                'unit_movement'= unit_movement, 'unit_resistance' = unit_resistance))
}

