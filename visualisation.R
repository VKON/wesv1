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
faction_statistics <- two_player_games %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
    rename(first_faction = faction) %>% 
    left_join(game_factions, 
              by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
    rename(second_faction = faction) %>% 
    group_by( first_faction, second_faction) %>% 
    summarise(wins = sum(first_player_wins),
              total_games = n()) %>% 
    mutate(win_ratio = wins/total_games,
           dist_to_fair = win_ratio - 0.5,
    # Variance of Bernoulli = N*p*(1-p), for fair coin sigma = sqrt(N)
           adjust_dist_to_fair = 2*(wins - 0.5*total_games)/sqrt(total_games))

faction_plot <- faction_statistics %>% 
    ggplot(aes(y = first_faction, x = second_faction, fill = adjust_dist_to_fair))+
        geom_tile() +
        scale_fill_gradient2('Adj. Win Ratio',high = 'red', low = 'blue', mid = 'white')+
        ggtitle('Certain Factions overpower others', 
                subtitle = 'Note the asymmetry across the diagonal')+
        coord_equal()+
        labs(x = '', y='')+
        theme_bw()+
        theme(text = element_text(family = 'Futura Std Medium'), 
              plot.subtitle = element_text(size = 8),
              axis.text.x = element_text(angle = 90),
              panel.border = element_blank(), panel.grid= element_blank())


leader_lookup <- player_game_statistics %>% 
        select(game_id, player_id, leader)

leader_statistics <- two_player_games %>% 
    left_join(leader_lookup, 
              by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
    rename('fp_leader' = 'leader') %>% 
    left_join(leader_lookup, 
              by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
    rename('sp_leader' = 'leader') %>% 
    group_by( fp_leader, sp_leader) %>% 
    summarise(wins = sum(first_player_wins),
              total_games = n()) %>% 
    mutate(win_ratio = wins/total_games,
           dist_to_fair = win_ratio - 0.5,
           # Variance of Bernoulli = N*p*(1-p), for fair coin sigma = sqrt(N)
           adjust_dist_to_fair = 2*(wins - 0.5*total_games)/sqrt(total_games))

leader_statistics%>% 
    ggplot(aes(y = fp_leader, x = sp_leader, fill = adjust_dist_to_fair))+
    geom_tile() +
    scale_fill_gradient2('Adj. Win Ratio',high = 'red', low = 'blue', mid = 'white')+
    coord_equal()+
    labs(x = '', y='')+
    theme_bw()+
    theme(text = element_text(family = 'Futura Std Medium'), 
          plot.subtitle = element_text(size = 8),
          axis.text.x = element_text(angle = 90),
          panel.border = element_blank(), panel.grid= element_blank())

# Test significance of faction asymmetry

two_player_games %>% 
  mutate(fold = row_number()%%10) %>% 
  left_join(game_factions, 
            by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
  rename(first_faction = faction) %>% 
  left_join(game_factions, 
            by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
  rename(second_faction = faction) %>% 
  group_by(fold, first_faction, second_faction) %>% 
  summarise(alph = 1+sum(first_player_wins),
            bet = 1+ n()-sum(first_player_wins)) %>% 
  mutate(lwr_p50 = qbeta(0.25, alph, bet),
         upr_p50 = qbeta(0.75, alph, bet),
         lwr_p80 = qbeta(0.1, alph, bet),
         upr_p80 = qbeta(0.9, alph, bet),
         lwr_p90 = qbeta(0.05, alph, bet),
         upr_p90 = qbeta(0.95, alph, bet)) %>% 
  mutate(in_p50 = if_else(0.5 >= lwr_p50 & 0.5 <= upr_p50, 1, 0),
         in_p80 = if_else(0.5 >= lwr_p80 & 0.5 <= upr_p80, 1, 0),
         in_p90 = if_else(0.5 >= lwr_p90 & 0.5 <= upr_p90, 1, 0)) %>% 
  group_by(fold) %>% 
  summarise(rate_in_50 = mean(in_p50),
            rate_in_80 = mean(in_p80),
            rate_in_90 = mean(in_p90)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fold)) +
    geom_line(aes(y = rate_in_50), color = 'red')+
    geom_hline(yintercept = 0.5, color = 'red', linetype = 2)+
    geom_line(aes(y = rate_in_80), color = 'blue')+
    geom_hline(yintercept = 0.8, color = 'blue', linetype = 2)+
    geom_line(aes(y = rate_in_90), color = 'violet')+
    geom_hline(yintercept = 0.9, color = 'violet', linetype = 2)+
    scale_y_continuous('Mean Occurence of p= 0.5', limits = c(0,1))+
    scale_x_continuous('Data Fold')+
    ggtitle('Occurance of p = 0.5 in plausible intervals', 
            subtitle = 'Empirical occurance consistently too low')+
    theme_bw()


# Visualisation for plausible distribution
M_value <- 35
N_value <- 60
alpha_value <- M_value + 1
beta_value <- N_value - M_value + 1
x_axis <- seq(0,1,0.0001)
distribution <- dbeta(x_axis, alpha_value, beta_value)

p50_interval <- c(qbeta(0.25,alpha_value, beta_value), qbeta(0.75,alpha_value, beta_value))
p80_interval <-  c(qbeta(0.10,alpha_value, beta_value), qbeta(0.9,alpha_value, beta_value))
p90_interval <- c(qbeta(0.05,alpha_value, beta_value), qbeta(0.95,alpha_value, beta_value))
ggplot()+
  geom_line(aes(x = x_axis, y = distribution))+
  geom_line(aes(x = p90_interval, y = 0), color = 'violet', size = 3, alpha = 0.7)+
  geom_line(aes(x = p80_interval, y = 0), color = 'blue', size = 3, alpha = 0.7)+
  geom_line(aes(x = p50_interval, y = 0), color = 'red', size = 3, alpha = 0.7)+
  scale_x_continuous('Probability', limits = c(0,1), breaks = c(0, 0.5, 1))+
  geom_point(aes(x = 0.5, y = 0), shape = 4, size = 6) +
  ggtitle(str_c('Plausible interval for ', M_value, ' wins in ', N_value, ' games'))+
  theme_bw()+
  theme( axis.line.y = element_blank(), 
         axis.ticks.y = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank())

# Visualisation of hierarchical splitting
library(glmnet)

game_factions <- player_game_statistics %>% 
  select(game_id, player_id, faction, leader)

faction_cleaned <-  two_player_games %>% 
  left_join(game_factions, 
            by =c('game_id' = 'game_id','first_player_id'='player_id') ) %>% 
  rename(fp_faction = faction, fp_leader = leader ) %>% 
  left_join(game_factions, 
            by =c('game_id' = 'game_id','second_player_id'='player_id') ) %>% 
  rename(sp_faction = faction, sp_leader = leader) %>% 
  # Select only those combinations that are well defined
  filter(!is.na(first_player_wins)) %>% 
  mutate(fp_faction_leader = str_c(fp_faction, '-', fp_leader),
         sp_faction_leader = str_c(sp_faction,'-',sp_leader)) %>% 
  # Order the factors alphabetically, this way more data per faction is obtained  
  mutate(factor_relation = fp_faction_leader <= sp_faction_leader) %>% 
  mutate(final_factor = if_else(factor_relation, 
                                str_c(fp_faction_leader,' vs. ', sp_faction_leader),
                                str_c(sp_faction_leader,' vs. ', fp_faction_leader)),
         final_factor = factor(final_factor),
         faction_win = if_else(factor_relation,
                               first_player_wins,
                               1-first_player_wins)) %>% 
  select(final_factor, faction_win)

one_hot_encoder <- faction_cleaned %>% 
  select(final_factor) %>% 
  dummyVars(formula = '~.')

x <- faction_cleaned %>% 
  select(final_factor) %>% 
  predict(one_hot_encoder, newdata = .) %>% 
  # The glmnet works with matrices as input instead
  as.matrix()

y <- faction_cleaned %>% 
  select(faction_win) %>% 
  pull() %>% 
  as.double()

fit <-  glmnet(x = x, y = y, family = 'binomial', alpha = 1)
hierarchy_plot <- plot(fit)
png('~/wesv1/presentations/hierarchies.png')
plot(fit)
dev.off()

# A trelliscope for the Unit Data
library(tidyverse)
library(trelliscopejs)
unit_images <- read_csv('data/unit_images.csv') 
unit_basic <- read_csv('data/unit_basic.csv') %>% distinct()


unit_attack <- read_csv('data/unit_attack.csv') %>% 
                  distinct() %>% 
                  mutate(total_damage = att_damage*att_count,
                         description = str_c(att_name,': ', 
                                             att_type, ' ', att_count, 
                                             'x', att_damage, ' (', att_category,')')) %>% 
                  select(-special1, -special2) %>% 
                  group_by(unit_id) %>% 
                  mutate(attack_num = row_number()) %>% 
                  ungroup()

first_attacks <- unit_attack %>% 
              filter(attack_num == 1)
second_attacks <- unit_attack %>% 
  filter(attack_num == 2)
tert_attacks <- unit_attack %>% 
  filter(attack_num == 3)
colnames(tert_attacks) <- str_c(colnames(tert_attacks), '_tert')

all_attacks <- first_attacks %>% 
  left_join(second_attacks, by = 'unit_id', suffix=c('_prim','_second'))

all_attacks <- all_attacks %>% 
  left_join(tert_attacks, by = c('unit_id'='unit_id_tert'))

unit_resistances <- read_csv('data/unit_resistance.csv') %>% 
                  group_by(unit_id) %>% 
                  distinct() %>% 
                  spread(key = 'category', value = 'percentage')
colnames(unit_resistances) <- str_c('resistance_', colnames(unit_resistances))


trellis <- unit_images %>%  
  distinct() %>% 
  left_join(unit_basic, by = c('clean_name'='unit')) %>% 
  left_join(unit_resistances, by = c('unit_id'='resistance_unit_id')) %>%
  left_join(all_attacks, by = c('unit_id'='unit_id')) %>% 
  mutate(name = cog(name, default_active = T),
         faction = cog(faction, default_active = T),
         alignment = cog(alignment, default_active = T),
         img_url = cog(img_url, default_active = F),
         hp = cog(hp, default_active = T),
         cost = cog(cost, default_active = T),
         panel = img_panel(img_url),
         description_prim = cog(description_prim, desc = 'Primary Attack', default_active = T),
         description_second = cog(description_second, desc = 'Secondary Attack', default_active = T),
         description_tert = cog(description_prim, desc = 'Tertiary Attack', default_active = T)) %>% 
  trelliscope(name = 'Unit Explorer', nrow = 1, ncol = 3, width = 1000)


