
library(dplyr)
library(tidyr)
library(googlesheets4)

player_data <- read_sheet('https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY', sheet = 'PLAYERS') %>% 
  fill(GameID, .direction = "down")
outcomes <- read_sheet('https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY', sheet = 'Game') %>% 
  select(GameID, winning_team = `Winning team`) %>% 
  filter(!is.na(winning_team))

outcomes %>% group_by(winning_team) %>% tally()

player_outcome <- 
  player_data %>% 
  left_join(outcomes, by = "GameID") %>% 
  mutate(
    player_won = (Alignment == winning_team)
  ) %>% 
  filter(winning_team %in% c("Good", "Evil"))

player_summary <- player_outcome %>% 
  group_by(Player) %>% 
  summarise(
    games_played = n(),
    games_won = sum(player_won),
    times_evil = sum(Alignment == "Evil"),
    times_good = sum(Alignment == "Good"),
    won_while_good = sum(Alignment == "Good" & winning_team == "Good"),
    won_while_evil = sum(Alignment == "Evil" & winning_team == "Evil"),
    times_outsider = sum(Type == "Outsider"),
    times_demon = sum(Type == "Demon"),
    times_nommer = sum(Nommed, na.rm = T),
    times_nommee = sum(`Was Nommed`, na.rm = T)
  ) %>% 
  ungroup() %>% 
  transmute(
    Player,
    games_played,
    win_rate = games_won / games_played,
    evil_rate = times_evil / games_played,
    evil_win_rate = won_while_evil / times_evil,
    good_win_rate = won_while_good / times_good,
    demon_rate = times_demon / games_played,
    nommer_avg = times_nommer / games_played,
    nommee_avg = times_nommee / games_played,
    outsider_rate = times_outsider / games_played
  ) %>% 
  filter(games_played > 5)

teammates <- player_outcome %>% 
  left_join(player_outcome, by = c("GameID", "Alignment"))

opponents <- player_outcome %>% 
  left_join(player_outcome, by = c("GameID")) %>% 
  filter(Alignment.x != Alignment.y)

get_co_win_rates <- function(player_pairs) {
  
  co_win_rate <- player_pairs %>%
    group_by(Player.x, Player.y) %>% 
    summarise(
      win_rate = paste0(round(100*mean(player_won.x), 1), "%"),
      played_together = n()
    ) %>% 
    filter(played_together > 3) %>% 
    ungroup() %>% 
    arrange(Player.x, Player.y) %>% 
    pivot_wider(
      id_cols = Player.x,
      names_from = Player.y,
      values_from = win_rate,
      values_fill = ""
    ) %>% 
    rename(Player = Player.x)
  
  return(co_win_rate[,c("Player", co_win_rate$Player)])
}

good_co_win_rates <- get_co_win_rates(teammates %>% filter(Alignment == "Good"))
evil_co_win_rates <- get_co_win_rates(teammates %>% filter(Alignment == "Evil"))
total_co_win_rates <- get_co_win_rates(teammates)

rival_win_rates <- get_co_win_rates(opponents)

nom_behaviour_by_alignment <- player_outcome %>% 
  filter(!is.na(Nommed), !is.na(`Was Nommed`)) %>% 
  group_by(Player, Alignment) %>% 
  summarise(
    games_played = n(),
    times_nommer = sum(Nommed, na.rm = T),
    times_nommee = sum(`Was Nommed`, na.rm = T)
  ) %>% 
  ungroup() %>% 
  transmute(
    Player,
    Alignment,
    games_played,
    nommed_avg = times_nommer / games_played,
    nommee_avg = times_nommee / games_played
  ) %>% 
  filter(games_played > 3)

role_win_rates <- player_outcome %>% 
  group_by(Role) %>% 
  summarise(
    games_played = n(),
    win_rate = mean(player_won)
  ) %>% 
  ungroup() %>% 
  filter(games_played > 3) %>% 
  arrange(desc(win_rate))




write_sheet(
  player_summary,
  ss = 'https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY',
  sheet = "Summary"
)
write_sheet(
  role_win_rates,
  ss = 'https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY',
  sheet = "Character summary"
)
write_sheet(
  total_co_win_rates,
  ss = 'https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY',
  sheet = "Total co-win rates"
)
write_sheet(
  good_co_win_rates,
  ss = 'https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY',
  sheet = "Good co-win rates"
)
write_sheet(
  evil_co_win_rates,
  ss = 'https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY',
  sheet = "Evil co-win rates"
)
write_sheet(
  rival_win_rates,
  ss = 'https://docs.google.com/spreadsheets/d/138ZzfYkyrMZREyXB_a1HF3YU_aflHJXFTvv5otYfYbY',
  sheet = "Rival win rates"
)
