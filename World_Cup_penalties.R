# Filename: "World_Cup_penalties.R"
# Description: Analysis of data for penalty shoot-outs in FIFA World Cup finals

# Structure of analysis is of:
# A. % of success
# B. Records
#
# with categories of analysis being
# .0 Kick analysis
# .1 Tournaments
# .2 Teams (countries)
# .3 Players (penalty takers)
# .4 Goalkeepers
# .5 Clubs


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries, directories & parameters

# load packages that are or may be used
library(tidyverse)    # includes ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(lubridate)
library(scales)
library(rvest)      # reading tables from a wikipedia page


# Set directory paths
dir_main = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
dir_input = paste(dir_main, "/Input", sep = "")
dir_output = paste(dir_main, "/R_output", sep = "")

setwd(dir_main)

# Initialise variables & parameters
years = c(seq(1930, 1938, 4), seq(1950, 2018, 4))
years_pens = c(seq(1978, 2018, 4))
no_teams = c(13, 16, 15, 13, rep(16, 7), rep(24, 4), rep(32, 6))

pos_level_order = c("GK", "DF", "MF","FW")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 

success_by_vars = function(df, vars){
  propn_success = df %>%
    group_by(across({{vars}})) %>%
    summarise(count_successful = sum(pen_result),
              count_taken = n(),
              propn_successful = mean(pen_result)) %>%
    arrange(desc(count_taken))
  propn_success}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files

# Pre-run
# use player_database from World_Cup_players.R
#source("World_Cup_players.R")
setwd(dir_output)
player_database = read.csv("World_Cup_player_database.csv")

setwd(dir_input)
penalty_shootout_results_raw = read_csv("World_Cup_penalty_shootout_results.csv")
penalty_score_difference = read_csv("World_Cup_penalty_score_difference.csv")
penalty_shootout_details_raw = read_csv("World_Cup_penalty_shootout_details.csv") %>%
  mutate(pen_result = ifelse(Result == "Goal", 1, 0))

# initialise variables for graph
start_yr = min(years)
end_yr = max(years)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations

# Includes data cleansing for fields which have values that are not consistent throughout
# & add additional variables
colnames(penalty_shootout_results_raw) = gsub(" ", "_", colnames(penalty_shootout_results_raw))
colnames(penalty_shootout_results_raw)[1] = "Shootout_no"

colnames(penalty_shootout_details_raw) = gsub(" ", "_", colnames(penalty_shootout_details_raw))
colnames(penalty_shootout_details_raw)[1] = "Shootout_no"

colnames(penalty_score_difference) = gsub(" ", "_", colnames(penalty_score_difference))


# Get single listing of teams & kicks taken, for use later
totals_winners_from_results = penalty_shootout_results_raw %>%
  select(Shootout_no:Round, Winner, Successful_kicks_winner, Count_kicks_winner, Date, Winner_gk_no:Winner_gk_start_mins)

totals_losers_from_results = penalty_shootout_results_raw %>%
  select(Shootout_no:Round, Loser, Successful_kicks_loser, Count_kicks_loser, Date, Loser_gk_no:Loser_gk_start_mins)

headers_totals = c("Shootout_no", "Year", "Round", "Team",
                   "Successful_kicks", "Count_kicks", "Date", "GK_no", "GK_start_mins")
colnames(totals_winners_from_results) = headers_totals
colnames(totals_losers_from_results) = headers_totals

totals_from_results = totals_winners_from_results %>%
  bind_rows(totals_losers_from_results) %>%
  arrange(Shootout_no)  %>%
  left_join(player_database, by = c("Year" = "years",
                                    "Team" = "squad",
                                    "GK_no" = "no")) %>%
  select(Shootout_no:caps, captain, age_int, club_current_name)

penalty_shootout_results_base = penalty_shootout_results_raw %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
         total_successful_kicks = Successful_kicks_winner + Successful_kicks_loser,
         total_missed = Total_pens - total_successful_kicks,
         match_score_winner = paste(Match_score_goals_each_side, "-", Match_score_goals_each_side,
                             " (", Successful_kicks_winner, "-", Successful_kicks_loser, ")", sep=""),
         match_score_loser = paste(substr(match_score_winner,1,5),
                                   Successful_kicks_loser, "-", Successful_kicks_winner, ")", sep=""),
         host_result = ifelse(Host_country == Winner, "Winner", 
                              ifelse(Host_country == Loser, "Loser", "NA")))

penalty_shootout_results_base_winners = penalty_shootout_results_base %>%
  select(Shootout_no:Second_team, Successful_kicks_winner, Count_kicks_winner,Total_pens:Date,
         Winner_gk_no:Winner_gk_start_mins, total_successful_kicks:total_missed, match_score_winner, host_result) %>%
  mutate(Team = Winner,
         Opponent = Loser,
         shootout_result = 1)
colnames(penalty_shootout_results_base_winners) = gsub("_winner", "_team", colnames(penalty_shootout_results_base_winners))
colnames(penalty_shootout_results_base_winners) = gsub("Winner_", "Team_", colnames(penalty_shootout_results_base_winners))

penalty_shootout_results_base_losers = penalty_shootout_results_base %>%
  select(Shootout_no:Second_team, Successful_kicks_loser, Count_kicks_loser,Total_pens:Date,
         Loser_gk_no:Loser_gk_start_mins, total_successful_kicks:total_missed, match_score_loser, host_result) %>%
  mutate(Team = Loser,
         Opponent = Winner,
         shootout_result = 0)
colnames(penalty_shootout_results_base_losers) = gsub("_loser", "_team", colnames(penalty_shootout_results_base_losers))
colnames(penalty_shootout_results_base_losers) = gsub("Loser_", "Team_", colnames(penalty_shootout_results_base_losers))

penalty_shootout_results_team = penalty_shootout_results_base_winners %>%
  bind_rows(penalty_shootout_results_base_losers) %>%
  arrange(Shootout_no) %>%
  mutate(opponent_successful_kicks = total_successful_kicks - Successful_kicks_team,
         opponent_kicks_taken = Total_pens - Count_kicks_team,
         winner_current_name = ifelse(Winner == "West Germany", "Germany", Winner),
         loser_current_name = ifelse(Loser == "West Germany", "Germany", Loser),
         team_current_name = ifelse(Team == "West Germany", "Germany", Team),
         opponent_current_name = ifelse(Opponent == "West Germany", "Germany", Opponent),
         alpha_first_team = ifelse(team_current_name > opponent_current_name, opponent_current_name, team_current_name),
         alpha_second_team = ifelse(team_current_name > opponent_current_name, team_current_name, opponent_current_name)) %>%
  group_by(team_current_name) %>%
  mutate(no_in_sequence_team_result = sequence(rle(shootout_result)$lengths),
         no_consecutive_won_team = ifelse(shootout_result == 0, 0, no_in_sequence_team_result),
         no_consecutive_lost_team = ifelse(shootout_result == 1, 0, no_in_sequence_team_result)) %>%
  ungroup()


# additional data for detailed data for each penalty kick
penalty_shootout_details_full = penalty_shootout_details_raw %>%
  mutate(player_minutes = ifelse(Player_start_min == 1, "Full game", paste("Sub", ifelse(Player_start_min > 90, "extra", "regular"), "time", sep = " ")),
         team_kick_no = (Pen - 1) %/% 2 + 1,
         team_pen_order = ifelse(Pen %% 2 == 1, "A", "B")) %>%
  left_join(totals_from_results, by = c("Shootout_no", "Year", "Round",
                                        "Team" = "Team")) %>%
  rename("Successful_kicks_team" = "Successful_kicks",
         "Count_kicks_team" = "Count_kicks") %>%
  left_join(totals_from_results, by = c("Shootout_no", "Year", "Round",
                                        "Opponent" = "Team")) %>%
  rename("Successful_kicks_opponent" = "Successful_kicks",
         "Count_kicks_opponent" = "Count_kicks") %>%
  select(-c(GK_no.x:club_current_name.x, Date.y)) %>%
  mutate(Winner = ifelse(Successful_kicks_team > Successful_kicks_opponent, Team, Opponent),
         Loser = ifelse(Successful_kicks_team < Successful_kicks_opponent, Team, Opponent),
         team_ultimate_result = ifelse(Team == Winner, "Win", "Loss")) %>%
  group_by(Shootout_no) %>% 
  mutate(cum_score_team_A = cumsum(ifelse(!is.na(team_pen_order) & team_pen_order=="A", pen_result, 0)),
         cum_score_team_B = cumsum(ifelse(!is.na(team_pen_order) & team_pen_order=="B", pen_result, 0)),
         cum_score = paste(cum_score_team_A, "-", cum_score_team_B, sep = ""),
         cum_kick_A = cumsum(ifelse(!is.na(team_pen_order) & team_pen_order=="A", 1, 0)),
         cum_kick_B = cumsum(ifelse(!is.na(team_pen_order) & team_pen_order=="B", 1, 0))) %>%
  ungroup() %>%
  mutate(cum_score_team_A_before = cum_score_team_A - ifelse(team_pen_order == "A", pen_result, 0),
         cum_score_team_B_before = cum_score_team_B - ifelse(team_pen_order == "B", pen_result, 0),
         cum_score_before = paste(cum_score_team_A_before, "-", cum_score_team_B_before, sep = ""),
         cum_score_team_A_if_successful = cum_score_team_A_before + ifelse(team_pen_order == "A", 1, 0),
         cum_score_team_B_if_successful = cum_score_team_B_before + ifelse(team_pen_order == "B", 1, 0),
         cum_score_if_successful = paste(cum_score_team_A_if_successful, "-", cum_score_team_B_if_successful, sep = ""),
         cum_kicks_taken_A_before = cum_kick_A - ifelse(team_pen_order == "A", 1, 0),
         cum_kicks_taken_B_before = cum_kick_B - ifelse(team_pen_order == "B", 1, 0),
         no_missed_A = cum_score_team_A_before - cum_kicks_taken_A_before,
         no_missed_B = cum_score_team_B_before - cum_kicks_taken_B_before,
         score_diff = no_missed_A - no_missed_B) %>%
  left_join(penalty_score_difference, by = c("score_diff" = "Score_diff_1st_team_less_2nd_team")) %>%
  mutate(max_possible_team_A_if_score = ifelse(Pen > 10, cum_score_team_A_before + ifelse(team_pen_order == "A", 1, 0), 5 + no_missed_A),
         max_possible_team_B_if_score = ifelse(Pen > 10, cum_score_team_B_before + 1, 5 + no_missed_B),
         max_possible_team_A_if_miss = ifelse(Pen > 10, cum_score_team_A_before, 5 + no_missed_A + ifelse(team_pen_order == "A", -1, 0)),
         max_possible_team_B_if_miss = ifelse(Pen > 10, cum_score_team_B_before + ifelse(team_pen_order == "B", 0, 1), 
                                              5 + no_missed_B + ifelse(team_pen_order == "B", -1, 0)),
         match_state = case_when(
           Pen > 10 &  max_possible_team_B_if_score > max_possible_team_A_if_score ~ "Match point",
           Pen > 10 &  max_possible_team_B_if_miss < max_possible_team_A_if_score ~ "Saving match point",
           Pen <= 10 & team_pen_order == "A" & cum_score_team_A_if_successful > max_possible_team_B_if_score ~ "Match point",
           Pen <= 10 & team_pen_order == "B" & cum_score_team_B_if_successful > max_possible_team_A_if_score ~ "Match point",
           Pen <= 10 & team_pen_order == "A" & max_possible_team_A_if_miss < cum_score_team_B ~ "Saving match point",
           Pen <= 10 & team_pen_order == "B" & max_possible_team_B_if_miss < cum_score_team_A ~ "Saving match point",
           TRUE ~ "live")) %>%
  left_join(player_database, by = c("Year" = "years",
                                    "Team" = "squad",
                                    # join by number instead of name
                                    #"Player_name" = "player",
                                    "Number" = "no")) %>%
  select(Shootout_no:Count_kicks_opponent, Winner:club_url, captain, age_int, nat_team_alpha3, nat_team_confederation:nat_team_current_name,
         club_current_name, GK_no.y:club_current_name.y, host_flag) %>%
  # if more countries change name, source this from Team & nat_team_current_name
  mutate(opponent_current_name = ifelse(Opponent == "West Germany", "Germany", Opponent),
         gk_player_minutes = ifelse(GK_start_mins.y == 1, "Full game", paste("Sub", ifelse(GK_start_mins.y > 90, "extra", "regular"), "time", sep = " "))) %>%
  group_by(nat_team_current_name) %>%
  mutate(cum_score_team = cumsum(pen_result),
         cum_score_team_prev = lag(cum_score_team, value = 1),
         no_in_sequence_team = sequence(rle(pen_result)$lengths),
         no_consecutive_scored_team = ifelse(pen_result == 0, 0, no_in_sequence_team),
         no_consecutive_missed_team = ifelse(pen_result == 1, 0, no_in_sequence_team)) %>%
  ungroup() %>%
  group_by(opponent_current_name) %>%
  mutate(cum_score_opponent = cumsum(pen_result),
         cum_score_opponent_prev = lag(cum_score_team, value = 1),
         no_in_sequence_opponent = sequence(rle(pen_result)$lengths),
         no_consecutive_conceded_opponent = ifelse(pen_result == 0, 0, no_in_sequence_opponent),
         no_consecutive_saved_opponent = ifelse(pen_result == 1, 0, no_in_sequence_opponent)) %>%
  ungroup() 

colnames(penalty_shootout_details_full) = gsub("\\.y", "_gk", colnames(penalty_shootout_details_full))
colnames(penalty_shootout_details_full) = gsub("GK_", "", colnames(penalty_shootout_details_full))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Analysis  ####

# check totals in penalty_shootout_results vs penalty_shootout_details_full
totals_from_details = penalty_shootout_details_full %>%
  group_by(Shootout_no, Year, Round, Team) %>%
  summarise(count_details_successful = sum(pen_result),
            count_details_kicks = n(),
            count_team_winner = sum(Team == Winner) / count_details_kicks,
            count_team_loser = sum(Team == Loser) / count_details_kicks)

totals_from_both_sources = totals_from_results %>%
  left_join(totals_from_details, by = c("Shootout_no", "Year", "Round", "Team"))

diff_successful_kicks = totals_from_both_sources %>%
  filter(!(Successful_kicks == count_details_successful))
dim(diff_successful_kicks)[1]

diff_kicks_taken = totals_from_both_sources %>%
  filter(!(Count_kicks == count_details_kicks))
dim(diff_kicks_taken)[1]

# Summaries
summ_by_tournament = penalty_shootout_results_base %>%
  group_by(Year) %>%
  summarise(count = n(),
            total_successful_kicks = sum(Successful_kicks_winner + Successful_kicks_loser),
            total_kicks_taken = sum(Total_pens),
            winner_team_A = sum(Winning_team_order == "1st"),
            winner_team_B = sum(Winning_team_order == "2nd")) %>%
  mutate(success_rate = total_successful_kicks / total_kicks_taken) %>%
  right_join(as_tibble(years_pens), by = c("Year" = "value")) %>%
  arrange(Year) %>%
  select(Year:total_kicks_taken, success_rate, winner_team_A:winner_team_B) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 
summ_by_tournament

summ_by_opponents = penalty_shootout_results_team %>%
  group_by(alpha_first_team, alpha_second_team) %>%
  summarise(count = n()/2)
summ_by_opponents

summ_by_team = penalty_shootout_results_team %>%
  group_by(team_current_name) %>%
  summarise(count = n(),
            won = sum(team_current_name == winner_current_name),
            lost = sum(team_current_name == loser_current_name),
            total_successful_kicks = sum(Successful_kicks_team),
            total_kicks_taken = sum(Count_kicks_team),
            total_opponent_successful_kicks = sum(opponent_successful_kicks),
            total_opponent_kicks_taken = sum(opponent_kicks_taken),
            most_consecutive_won_team = max(no_consecutive_won_team),
            most_consecutive_lost_team = max(no_consecutive_lost_team),
            first_shootout = min(Year),
            last_shootout = max(Year)) %>%
  mutate(team_success_rate = total_successful_kicks / total_kicks_taken,
         opponent_success_rate = total_opponent_successful_kicks / total_opponent_kicks_taken)
summ_by_team

summ_by_team_year = penalty_shootout_results_team %>%
  group_by(Team, team_current_name, Year) %>%
  summarise(count = n())
summ_by_team_year

summ_by_player = penalty_shootout_details_full %>%
  group_by(Player_name, nat_team_current_name) %>%
  summarise(count = n(),
            count_decider = sum(match_state == "Match point"),
            total_scored = sum(pen_result),
            first_shootout = min(Year),
            last_shootout = max(Year),
            team_shootout_win = sum(team_ultimate_result == "Win"),
            team_shootout_loss = sum(team_ultimate_result == "Loss"),
            min_shootout_no = min(Shootout_no),
            max_shootout_no = max(Shootout_no))
summ_by_player

summ_by_gk_shootout = penalty_shootout_details_full %>%
  group_by(player_gk, opponent_current_name, Shootout_no, nat_team_current_name, Year) %>%
  summarise(count = n(),
            total_scored_against = sum(pen_result),
            total_missed_against = sum(pen_result == 0),
            total_saved = sum(!is.na(How_goalkeeper_saves)),
            total_direction_correct = sum(Goalkeeper_direction_correct == "right"))
summ_by_gk_shootout

summ_gk_shootout_count = summ_by_gk_shootout %>%
  group_by(player_gk, opponent_current_name) %>%
  summarise(count_shoutouts = n())
summ_gk_shootout_count

summ_by_gk = penalty_shootout_details_full %>%
  group_by(player_gk, opponent_current_name) %>%
  summarise(count = n(),
            total_scored_against = sum(pen_result),
            total_missed_against = sum(pen_result == 0),
            total_saved = sum(!is.na(How_goalkeeper_saves)),
            total_direction_correct = sum(Goalkeeper_direction_correct == "right"),
            propn_direction_correct = mean(Goalkeeper_direction_correct == "right"),
            propn_scored_against = mean(pen_result),
            count_decider_faced = sum(match_state == "Saving match point"),
            count_decider_saved = sum(match_state == "Saving match point" & !is.na(How_goalkeeper_saves)),
            first_shootout = min(Year),
            last_shootout = max(Year),
            min_shootout_no = min(Shootout_no),
            max_shootout_no = max(Shootout_no)) %>%
  ungroup() %>%
  left_join(summ_gk_shootout_count, by = c("player_gk", "opponent_current_name"))
summ_by_gk

summary_by_gk_direction_history = penalty_shootout_details_full %>%
  group_by(Shootout_no, Team, team_ultimate_result, Goalkeeper_direction_correct) %>%
  summarise(count_kicks = n()) %>%
  ungroup() %>%
  group_by(Shootout_no, Team, team_ultimate_result) %>%
  mutate(total_kicks = sum(count_kicks),
         propn_direction = count_kicks / total_kicks) %>%
  ungroup() %>%
  arrange(Shootout_no, desc(team_ultimate_result))
summary_by_gk_direction_history

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Probability of Success  ####

# A.0 Kick analysis
no_taken = dim(penalty_shootout_details_full)[1]
no_successful = sum(penalty_shootout_details_full$pen_result)
historical_propn_successful = mean(penalty_shootout_details_full$pen_result)
historical_propn_successful
# Same as success_by_vars(penalty_shootout_details_full, c())

winner_team_A = sum(summ_by_tournament$winner_team_A)
winner_team_B = sum(summ_by_tournament$winner_team_B)
propn_winner_team_A = winner_team_A / (winner_team_A + winner_team_B)
propn_winner_team_A
propn_winner_team_B = winner_team_B / (winner_team_A + winner_team_B)
propn_winner_team_B

# by kick characteristics
success_by_vars(penalty_shootout_details_full, Foot)
success_by_vars(penalty_shootout_details_full, Run_up_length)
success_by_vars(penalty_shootout_details_full, Run_up_speed)
success_by_vars(penalty_shootout_details_full, Kick_Height)
success_by_vars(penalty_shootout_details_full, Kick_Direction)
success_by_vars(penalty_shootout_details_full, Kick_Speed)
success_by_vars(penalty_shootout_details_full, Direction_of_miss)
success_by_vars(penalty_shootout_details_full, c(Foot, Kick_Direction))

# by shootout score
success_by_vars(penalty_shootout_details_full, Pen)
success_by_vars(penalty_shootout_details_full, match_state)
success_by_vars(penalty_shootout_details_full, Game_state)
success_by_vars(penalty_shootout_details_full, c(team_pen_order, Game_state))
success_by_vars(penalty_shootout_details_full, team_pen_order)
success_by_vars(penalty_shootout_details_full, team_kick_no)
success_by_vars(penalty_shootout_details_full, Pen)
success_by_vars(penalty_shootout_details_full, team_ultimate_result)

# A.1 by Tournament
success_by_vars(penalty_shootout_details_full, c())
success_by_vars(penalty_shootout_details_full, Shootout_no) %>%
  arrange(Shootout_no)
success_by_vars(penalty_shootout_details_full, Year)
success_by_vars(penalty_shootout_details_full, Round)
success_by_vars(penalty_shootout_details_full, host_flag)

# A.2 by Team
success_by_vars(penalty_shootout_details_full, nat_team_current_name)
success_by_vars(penalty_shootout_details_full, opponent_current_name)
success_by_vars(penalty_shootout_details_full, nat_team_confederation)

# A.3 Penalty Takers
success_by_vars(penalty_shootout_details_full, Goal_in_game)
success_by_vars(penalty_shootout_details_full, player_minutes)
success_by_vars(penalty_shootout_details_full, pos)
success_by_vars(penalty_shootout_details_full, captain)
success_by_vars(penalty_shootout_details_full, age_int)

# A.4 Goalkeepers
success_by_vars(penalty_shootout_details_full, Goalkeeper_direction_correct)
success_by_vars(penalty_shootout_details_full, How_goalkeeper_saves)

# Note: for goalkeepers, lower propn_successful is better for GK stats
success_by_vars(penalty_shootout_details_full, c(player_gk, opponent_current_name))
success_by_vars(penalty_shootout_details_full, gk_player_minutes)
success_by_vars(penalty_shootout_details_full, age_int_gk)

# A.5 by Club
success_by_vars(penalty_shootout_details_full, club_current_name)


#### B.Records  ####

# B.1. Tournaments
# list shootouts
list_shootouts = penalty_shootout_results_base %>%
  select(Year, Winner, Loser, match_score_winner)
list_shootouts

# most in tournament
most_shootouts_tourn = summ_by_tournament %>%
  top_n(1, count) %>%
  select(Year:total_kicks_taken)
most_shootouts_tourn

least_shootouts_tourn = summ_by_tournament %>%
  top_n(-1, count) %>%
  select(Year:total_kicks_taken)
least_shootouts_tourn

# longest & shortest shootouts
most_pens_shootout = penalty_shootout_results_base %>%
  filter(Total_pens == max(penalty_shootout_results_base$Total_pens)) %>%
  select(Year:Loser, Total_pens)
most_pens_shootout

additional_pens_shootout = penalty_shootout_results_base %>%
  filter(Total_pens > 10) %>%
  select(Year:Loser, Total_pens)
additional_pens_shootout

least_pens_shootout = penalty_shootout_results_base %>%
  filter(Total_pens == min(penalty_shootout_results_base$Total_pens)) %>%
  select(Year:Loser, Total_pens)
least_pens_shootout

# least kicks in shootouts
least_kicks_winner_shootout = penalty_shootout_results_base %>%
  filter(Count_kicks_winner == min(penalty_shootout_results_base$Count_kicks_winner)) %>%
  select(Year:Loser, Count_kicks_winner)
least_kicks_winner_shootout

least_successful_kicks_winner_shootout = penalty_shootout_results_base %>%
  filter(Successful_kicks_winner == min(penalty_shootout_results_base$Successful_kicks_winner)) %>%
  select(Year:Loser, Successful_kicks_winner, Count_kicks_winner)
least_successful_kicks_winner_shootout

most_scored_shootout = penalty_shootout_results_base %>%
  filter(total_successful_kicks == max(penalty_shootout_results_base$total_successful_kicks)) %>%
  select(Year:Loser, total_successful_kicks)
most_scored_shootout

least_scored_shootout = penalty_shootout_results_base %>%
  filter(total_successful_kicks == min(penalty_shootout_results_base$total_successful_kicks)) %>%
  select(Year:Loser, total_successful_kicks)
least_scored_shootout

most_misses_shootout = penalty_shootout_results_base %>%
  filter(total_missed == max(penalty_shootout_results_base$total_missed)) %>%
  select(Year:Loser, total_missed)
most_misses_shootout

least_misses_shootout = penalty_shootout_results_base %>%
  filter(total_missed == min(penalty_shootout_results_base$total_missed)) %>%
  select(Year:Loser, total_missed)
least_misses_shootout

# B.2. Teams
# overall results
teams_overall_shootouts = summ_by_team %>%
  select(team_current_name:lost, total_successful_kicks, total_opponent_successful_kicks) %>%
  mutate(win_perc = won / count,
         for_against = paste(total_successful_kicks, "-", total_opponent_successful_kicks, sep = ""),
         net_for_against = total_successful_kicks - total_opponent_successful_kicks) %>%
  arrange(desc(won), lost, desc(net_for_against), desc(total_successful_kicks)) %>%
  select(team_current_name:lost, win_perc:for_against)
teams_overall_shootouts

teams_success_rate = summ_by_team %>%
  select(team_current_name, total_kicks_taken, total_successful_kicks, team_success_rate) %>%
  arrange(desc(team_success_rate), desc(total_kicks_taken), team_current_name) 
teams_success_rate

most_played = summ_by_team %>%
  arrange(desc(count), team_current_name) %>%
  top_n(1, count) %>%
  select(team_current_name, count)
most_played

most_won = summ_by_team %>%
  arrange(desc(won), team_current_name) %>%
  top_n(1, won) %>%
  select(team_current_name, won)
most_won

most_lost = summ_by_team %>%
  arrange(desc(lost), team_current_name) %>%
  top_n(1, lost) %>%
  select(team_current_name, lost)
most_lost

# most consecutive won
teams_most_consec_won = summ_by_team %>%
  arrange(desc(most_consecutive_won_team), team_current_name) %>%
  top_n(1, most_consecutive_won_team) %>%
  select(team_current_name, most_consecutive_won_team)
teams_most_consec_won

# most consecutive lost
teams_most_consec_lost = summ_by_team %>%
  arrange(desc(most_consecutive_lost_team), team_current_name) %>%
  top_n(1, most_consecutive_lost_team) %>%
  select(team_current_name, most_consecutive_lost_team)
teams_most_consec_lost 

team_undefeated_most_wins = summ_by_team %>%
  filter(lost == 0) %>%
  arrange(desc(won), team_current_name) %>%
  top_n(1, won) %>%
  select(team_current_name, won)
team_undefeated_most_wins 

team_winless_most_losses = summ_by_team %>%
  filter(won == 0) %>%
  arrange(desc(lost), team_current_name) %>%
  top_n(1, lost) %>%
  select(team_current_name, lost)
team_winless_most_losses 

# multiple shootouts in one tournament
teams_multiple_shootouts = summ_by_team_year %>%
  filter(count > 1)
teams_multiple_shootouts

# most common shootout match-ups
teams_most_played = summ_by_opponents %>%
  filter(count == max(summ_by_opponents$count))
teams_most_played

# most consecutive scored
teams_most_consec_scored = penalty_shootout_details_full %>%
  arrange(desc(no_consecutive_scored_team), nat_team_current_name) %>%
  top_n(1, no_consecutive_scored_team) %>%
  select(nat_team_current_name, no_consecutive_scored_team)
teams_most_consec_scored
  
# most consecutive missed
teams_most_consec_missed = penalty_shootout_details_full %>%
  arrange(desc(no_consecutive_missed_team), nat_team_current_name) %>%
  top_n(1, no_consecutive_missed_team) %>%
  select(nat_team_current_name, no_consecutive_missed_team)
teams_most_consec_missed 
  
# B.3. Penalty Takers
most_taken = summ_by_player %>%
  filter(count == max(summ_by_player$count)) %>%
  arrange(max_shootout_no) %>%
  select(Player_name, nat_team_current_name, count, first_shootout, last_shootout, total_scored, team_shootout_win)
most_taken

most_pen_scored = summ_by_player %>%
  filter(total_scored == max(summ_by_player$total_scored)) %>%
  arrange(max_shootout_no) %>%
  select(Player_name, nat_team_current_name, total_scored)
most_pen_scored

most_pen_scored_and_won = summ_by_player %>%
  filter(total_scored == max(summ_by_player$total_scored) & 
           team_shootout_win == max(summ_by_player$team_shootout_win)) %>%
  arrange(max_shootout_no) %>%
  select(Player_name, nat_team_current_name, total_scored, team_shootout_win)
most_pen_scored_and_won

most_pen_scored_and_lost = summ_by_player %>%
  filter(total_scored == max(summ_by_player$total_scored) & 
           team_shootout_win == min(summ_by_player$team_shootout_win)) %>%
  arrange(max_shootout_no) %>%
  select(Player_name, nat_team_current_name, total_scored, team_shootout_win)
most_pen_scored_and_lost

most_decider_pen = summ_by_player %>%
  filter(count_decider == max(summ_by_player$count_decider)) %>%
  arrange(max_shootout_no) %>%
  select(Player_name, nat_team_current_name, count_decider)
most_decider_pen


# multiple shootouts in one tournament
players_multiple_shooutouts = penalty_shootout_details_full %>%
  group_by(Year, Team, player) %>%
  summarise(count_successful = sum(pen_result),
            count_taken = n(),
            count_left = sum(Kick_Direction == "left"),
            count_right = sum(Kick_Direction == "right"),
            count_centre = sum(Kick_Direction == "centre")) %>%
  filter(count_taken > 1)
players_multiple_shooutouts

players_multiple_shooutouts_same_direction = players_multiple_shooutouts %>%
  filter(count_left > 1 | count_right > 1 | count_centre > 1)

propn_same_direction = nrow(players_multiple_shooutouts_same_direction) /
  nrow(players_multiple_shooutouts)
propn_same_direction

# B.4 Goalkeeper
most_shootouts_gk = summ_by_gk %>%
  filter(count_shoutouts == max(summ_by_gk$count_shoutouts)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, count_shoutouts, first_shootout, last_shootout)
most_shootouts_gk

most_pens_faced = summ_by_gk %>%
  filter(count == max(summ_by_gk$count)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, count)
most_pens_faced

most_pens_scored_against = summ_by_gk %>%
  filter(total_scored_against == max(summ_by_gk$total_scored_against)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, total_scored_against)
most_pens_scored_against

most_pens_missed_against = summ_by_gk %>%
  filter(total_missed_against == max(summ_by_gk$total_missed_against)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, total_missed_against)
most_pens_missed_against

most_pens_saved = summ_by_gk %>%
  filter(total_saved == max(summ_by_gk$total_saved)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, total_saved)
most_pens_saved

most_pens_saved_shootout = summ_by_gk_shootout %>%
  filter(total_saved == max(summ_by_gk_shootout$total_saved)) %>%
  arrange(Shootout_no) %>%
  select(player_gk, opponent_current_name, total_saved, nat_team_current_name)
most_pens_saved_shootout

most_decider_pen_faced = summ_by_gk %>%
  filter(count_decider_faced == max(summ_by_gk$count_decider_faced)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, count_decider_faced)
most_decider_pen_faced

most_decider_pen_saved = summ_by_gk %>%
  filter(count_decider_saved == max(summ_by_gk$count_decider_saved)) %>%
  arrange(max_shootout_no) %>%
  select(player_gk, opponent_current_name, count_decider_saved)
most_decider_pen_saved

# highest proportion guesses correctly
highest_Goalkeeper_direction_correct = summary_by_gk_direction_history %>%
  filter(propn_direction == 1 & Goalkeeper_direction_correct == "right")
highest_Goalkeeper_direction_correct

lowest_Goalkeeper_direction_correct = summary_by_gk_direction_history %>%
  filter(propn_direction == 1 & Goalkeeper_direction_correct == "wrong")
lowest_Goalkeeper_direction_correct

# GK - most consecutive saved
teams_most_consec_saved = penalty_shootout_details_full %>%
  arrange(desc(no_consecutive_saved_opponent), nat_team_current_name) %>%
  top_n(1, no_consecutive_saved_opponent) %>%
  select(nat_team_current_name, no_consecutive_saved_opponent)
teams_most_consec_saved 

# GK - most consecutive conceded
teams_most_consec_conceded = penalty_shootout_details_full %>%
  arrange(desc(no_consecutive_conceded_opponent), nat_team_current_name) %>%
  top_n(1, no_consecutive_conceded_opponent) %>%
  select(nat_team_current_name, no_consecutive_conceded_opponent)
teams_most_consec_conceded 

# B.5 Clubs
taker_gk_same_club = penalty_shootout_details_full %>%
  filter(club_current_name == club_current_name_gk) %>%
  select(Year:Opponent, player_gk, club_current_name, Result)
taker_gk_same_club

  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Graphs ####
end_yr = max(penalty_shootout_details_full$Year)

# Graph 1 - Order of Penalty Kick
data_for_graph_01 = success_by_vars(penalty_shootout_details_full, c(Pen, team_pen_order)) %>%
  filter(count_taken >= 5) 
data_for_graph_01

graph_01 = ggplot(data_for_graph_01, aes(x = Pen, y = propn_successful, fill = team_pen_order), size=1.08) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(yintercept = historical_propn_successful, linetype="dashed", colour = "red", size=1.08) +
  ggtitle(paste("Success Rate in Penalty Shoot-outs in World Cup Finals - to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0)) +
  scale_fill_discrete(name = "Team/nPenalty/nOrder") +
  labs(x="Kick Number", y="Proportion Successful",subtitle="Minimum 5 kicks") +
  annotate("text", x = 9, y = propn_successful + 0.03, 
           label = c(paste("Average = ", round(historical_propn_successful,4)*100, "%", sep = "")), 
           colour="red", size=3 , fontface="bold") 
graph_01

# Graph 2 - Age of Penalty Kick Taker
data_for_graph_02 = success_by_vars(penalty_shootout_details_full, age_int) %>%
  filter(count_taken >= 5) %>%
  arrange(age_int)
data_for_graph_02

graph_02 = ggplot(data_for_graph_02, aes(x = age_int, y = propn_successful), size=1.08) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(yintercept = propn_successful, linetype="dashed", colour = "red", size=1.08) +
  ggtitle(paste("Success Rate in Penalty Shoot-outs in World Cup Finals - to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0)) +
  labs(x="Age of Kick Taker", y="Proportion Successful",subtitle="Minimum 5 kicks") +
  annotate("text", x = 35, y = propn_successful + 0.03, 
           label = c(paste("Average = ", round(propn_successful,4)*100, "%", sep = "")), 
           colour="red", size=3 , fontface="bold") 
graph_02

# Graph 3 - Position of Penalty Kick Taker
data_for_graph_03 = success_by_vars(penalty_shootout_details_full, pos) %>%
  mutate(pos = factor(pos, levels=pos_level_order)) %>%
  filter(count_taken >= 5) %>%
  arrange(pos)
data_for_graph_03

graph_03 = ggplot(data_for_graph_03, aes(x = pos, y = propn_successful), size=1.08) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(yintercept = propn_successful, linetype="dashed", colour = "red", size=1.08) +
  ggtitle(paste("Success Rate in Penalty Shoot-outs in World Cup Finals - to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0)) +
  labs(x="Position of Kick Taker", y="Proportion Successful",subtitle="Minimum 5 kicks") +
  annotate("text", x = 2, y = propn_successful + 0.03, 
           label = c(paste("Average = ", round(propn_successful,4)*100, "%", sep = "")), 
           colour="red", size=3 , fontface="bold") 
graph_03

# Graph 4 - Age of Goalkeeper Facing Penalty Kick
data_for_graph_04 = success_by_vars(penalty_shootout_details_full, age_int_gk) %>%
  filter(count_taken >= 5) %>%
  arrange(age_int_gk)
data_for_graph_04

graph_04 = ggplot(data_for_graph_04, aes(x = age_int_gk, y = propn_successful), size=1.08) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(yintercept = propn_successful, linetype="dashed", colour = "red", size=1.08) +
  ggtitle(paste("Success Rate in Penalty Shoot-outs in World Cup Finals - to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0)) +
  labs(x="Age of Goalkeeper", y="Proportion Successful",subtitle="Minimum 5 kicks") +
  annotate("text", x = 32, y = propn_successful + 0.03, 
           label = c(paste("Average = ", round(propn_successful,4)*100, "%", sep = "")), 
           colour="red", size=3 , fontface="bold") 
graph_04

data_for_graph_05 = summ_by_gk %>%
  filter(count > 5)

graph_05 = ggplot(data_for_graph_05, aes(x = propn_direction_correct, y = propn_scored_against)) +
  geom_point() +
  geom_text(aes(label=player_gk),hjust=-0.08, vjust=0) +
  ggtitle("Goalkeepers in World Cup Penalty Shotouts") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(x="Proportion of Times Goalkeeper Moved in Correct Direction", y="Proportion of Kicks Scored Against",
       subtitle="Minimum 5 kicks faced") +
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0))
graph_05


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Output ####
# export output to csv format
setwd(dir_output)
write.csv(penalty_shootout_details_full, file = "World_Cup_penalty_shootout_details_full.csv")
write.csv(penalty_shootout_results_full, file = "World_Cup_penalty_shootout_results_full.csv")
write.csv(penalty_shootout_results_team, file = "World_Cup_penalty_shootout_results_team.csv")

write.csv(list_shootouts, file = "World_Cup_list_shootouts.csv")
write.csv(teams_overall_shootouts, file = "World_Cup_teams_overall_shootouts.csv")
write.csv(teams_success_rate, file = "World_Cup_teams_success_rate.csv")
setwd(dir_input)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Temporary (working) code




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To do:

