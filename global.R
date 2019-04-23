#load liibraries #####
library(icon)
library(shinycssloaders)
library(ggExtra)
library(readr)
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)

#read in the data frames #####
win_prob_df = read_csv('./www/win_prob_df.csv')
team_eff_df = read_csv('./www/team_eff_df.csv')
qb_df = read_csv('./www/qb_df.csv')
skill_df = read_csv('./www/skill_df.csv')
explore_df = read_csv('./www/explore_df.csv')

#create the global variable for max_wk and min_yr for help with adding on info from past, and in future #####
max_wk = max(win_prob_df$wk_ovr_rk, na.rm = T)
min_year = min(win_prob_df$year,na.rm =T)

#create a variable that lists all the teams for future filtering #####
teams = sort(unique(win_prob_df %>% .$home_team))


#gather the win_prob_df #####
win_prob_df = win_prob_df %>%
  mutate(wks_back =  1 + max_wk - wk_ovr_rk) %>%
  gather(team, wpa, 
         -game_seconds_remaining, -home_team, -wk_ovr_rk, -wks_back,
         -home_team_score, -away_team_score, -home_final_score, -away_final_score,
         -away_team, -year, -week, -desc, -game_id, -play_id, -matchup, -posteam)

#add the wks_back variable to the team_eff_df #####
team_eff_df = team_eff_df %>%
  mutate(wks_back =  1 + max_wk - wk_ovr_rk)


#create summary dfs for plot outputs ######
qb_summary_df = qb_df %>%
  group_by(opp_yds) %>%
  summarize(
    pacr = mean(pacr, na.rm = T),
    apacr = mean(apacr, na.rm = T),
    epa = mean(epa, na.rm = T),
    wpa = mean(wpa, na.rm = T),
    anya = mean(anya, na.rm = T)
  ) %>%
  na.omit()

skill_summary_df = skill_df %>%
  group_by(opp_yds) %>%
  summarize(
    racr = mean(racr, na.rm = T),
    aracr = mean(aracr, na.rm = T),
    epa = mean(epa, na.rm = T),
    wpa = mean(wpa, na.rm = T),
    anya = mean(anya, na.rm = T)
  ) %>%
  na.omit()


#text explantaions of the included advanced metrics with links for more details #####
epa_detail = "EPA (Expected Points Added) measures the expected scoring each play will produce. Learn more "
epa_link = 'http://www.stat.cmu.edu/~ryurko/talk/glsac/'
wpa_detail = "WPA (Win Probability Added) measures the expected change in the game's outcome\n
each play will produce. There are at least six prominent NFL win probability models in deployment. Learn more "
wpa_link = "https://statsbylopez.com/2017/03/08/all-win-probability-models-are-wrong-some-are-useful/comment-page-1/"
pacr_detail = "PACR (Passing Air Conversion Ratio) is an efficiency metric that measures how often a yard thrown in the air is converted into receiving yardage on the field.\n
It combines signals from both catch rate (Catch %) and yards after the catch (YAC). Learn more "
pacr_link = "http://airyards.com/the_league.html#MathJax-Span-342"
racr_detail = "RACR (Receiver Air Conversion Ratio) is an efficiency metric that measures how often a yard thrown in the air is converted into receiving yardage on the field.\n
It combines signals from both catch rate (Catch %) and yards after the catch (YAC).\n
In this graph, rushes are included as 0 yard passes. Learn more "
racr_link = "http://airyards.com/the_league.html#MathJax-Span-342"
apacr_detail = "aPACR is a variant of PACR that adds TDs and INTs to the PACR calculation,\n
with the multipliers suggested by the AYA formula outlined in 'The Hidden Game of Football'. Learn more "
apacr_link = "http://airyards.com/the_league.html#MathJax-Span-521"
aracr_detail = "aRACR is a variant of RACR that adds TDs and INTs to the RACR calculation,\n
with the multipliers suggested by the AYA formula outlined in 'The Hidden Game of Football'. \n
In this graph, rushes are included as 0 yard passes. Learn more "
aracr_link = "http://airyards.com/the_league.html#MathJax-Span-521"
anya_detail = "ANYA (Adjusted Net Yards per Attempt, or AYA) adds multipliers for interceptions and touchdowns to the simple Yards Gained per Opportunity metric,\n
as outlined by 'The Hidden Game of Football' by Bob Carroll, Pete Palmer, and John Thorn"
anya_link = "https://www.pro-football-reference.com/about/glossary.htm#ay/a"
wopr_detail = "Weighted Opportunity Rating (WOPR) is a weighted combination of a player's share of team targets and share of team air yards.\n More info "
wopr_link = "https://www.4for4.com/fantasy-football/2018/preseason/air-yards-identify-wr-best-ball-values"
awopr_detail = "Adjusted/Weighted WOPR (aWOPR) simply includes rushes as 0-yard passes in the WOPR calculation. More info "
awopr_link = "https://www.4for4.com/fantasy-football/2018/preseason/air-yards-identify-wr-best-ball-values"

