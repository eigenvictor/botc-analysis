
library(dplyr)
library(tidyr)

readxl::excel_sheets("Data/BOTCT.xlsx")
player_data <- readxl::read_excel("Data/BOTCT.xlsx", sheet = "PLAYERS") %>% 
  fill(GameID, .direction = "down")
outcomes <- readxl::read_excel("Data/BOTCT.xlsx", sheet = "Game") %>% 
  select(GameID, winning_team = `Winning team`)

outcomes %>% group_by(winning_team) %>% tally()

player_outcome <- 
  player_data %>% 
  left_join(outcomes, by = "GameID") %>% 
  mutate(
    player_won = (Alignment == winning_team)
  )

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
    times_nommed = sum(Nommed, na.rm = T),
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
    nommed_avg = times_nommed / games_played,
    nommee_avg = times_nommee / games_played,
    outsider_rate = times_outsider / games_played
  ) %>% 
  filter(games_played > 5)

teammates <- player_outcome %>% 
  left_join(player_outcome, by = c("GameID", "Alignment"))

teammate_win_rate <- teammates %>%
  filter(Alignment == "Good") %>% 
  group_by(Player.x, Player.y) %>% 
  summarise(
    win_rate = mean(player_won.x),
    played_together = n()
  ) %>% 
  filter(played_together > 3) %>% 
  ungroup() %>% 
  arrange(Player.x, Player.y) %>% 
  pivot_wider(
    id_cols = Player.x,
    names_from = Player.y,
    values_from = win_rate
  )

teammate_win_rate[,c("Player.x", teammate_win_rate$Player.x)] %>% 
  View()

nom_behaviour_by_alignment <- player_outcome %>% 
  filter(!is.na(Nommed), !is.na(`Was Nommed`)) %>% 
  group_by(Player, Alignment) %>% 
  summarise(
    games_played = n(),
    times_nommed = sum(Nommed, na.rm = T),
    times_nommee = sum(`Was Nommed`, na.rm = T)
  ) %>% 
  ungroup() %>% 
  transmute(
    Player,
    Alignment,
    games_played,
    nommed_avg = times_nommed / games_played,
    nommee_avg = times_nommee / games_played
  ) %>% 
  filter(games_played > 3)

player_outcome %>% 
  group_by(Role) %>% 
  summarise(
    games_played = n(),
    win_rate = mean(player_won)
  ) %>% 
  ungroup() %>% 
  filter(games_played > 3) %>% 
  arrange(desc(win_rate)) %>% 
  View()
