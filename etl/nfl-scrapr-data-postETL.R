library(tidyverse)
#read in our csvs #####
app_path = '~/Dropbox/Matt\ Savoca/Courses/nycdsa/shinyapp/'
pbp = read_csv("./www/pbp.csv")
gbg = read_csv("./www/gbg.csv")
rosters = read_csv("./www/rosters.csv")
pbp_df = pbp
gbg_df = gbg
rosters_df = rosters
min_year = min(pbp_df$year,na.rm =T)


#left_join in the game by game info we need #####
pbp_df = gbg_df %>% 
  rename(home_final_score = home_score,  away_final_score = away_score, year = season) %>% 
  select(-state_of_game, -game_url, -type) %>% 
  mutate(year = as.numeric(year)) %>% 
  right_join(pbp_df, by = c("game_id","year","home_team","away_team"))
write_csv(pbp_df, "./www/pbp_joined.csv")

#(re)exploring the data #####
#noticed something weird in the qb_scramble column, investigating
# scramblepass = pbp_df %>% filter(qb_scramble == 1&play_type== "pass") %>% select(play_type) %>% unique()
# scramblepass[[1]]


# do penalties have there own play_type? Narrator: Nope.
# pbp_df %>% filter(is.na(play_type)) %>% select(desc) %>% unique()
# pbp_df %>% select(play_type) %>% unique()


# home/away_score&scrambles
#all the plays that have qb_scramble == 1 & play_type == "pass", are challenged and reversed plays or no plays, coercing them to scramble == 0
#also create an opportunity yards metric

#adjust the pbp_df for use with shiny ######
pbp_df_adj = pbp_df %>% 
  mutate(
    qb_scramble = ifelse(qb_scramble==1&play_type=="pass",0,qb_scramble),
    play_type2 = as.factor(ifelse(qb_scramble==1|play_type=="pass", "dropback",play_type)),
    away_team_score = ifelse(posteam == home_team, defteam_score, posteam_score),
    home_team_score = ifelse(posteam == home_team, posteam_score, defteam_score),
    opp_yds = ifelse(air_yards < 0|is.na(air_yards), 0, air_yards),
    adj_opp_yds = ifelse(opp_yds == 0, 1, opp_yds),
    gamewk = as.numeric(paste0(year,ifelse(week>=10,week,paste0(0,week)))),
    wk_ovr_rk = dense_rank(gamewk),
    #combine Rusher_ID and Receiver_ID variable into a skillplayer ID, 'skill_ID'
    skill_ID = ifelse(play_type2=="run",rusher_player_id,ifelse(
                      play_type2=="dropback",receiver_player_id,NA)),
    #combine Passer_ID and QB-scramble Rusher_ID into a dropboack_ID, 'dropback_ID'
    dropback_ID = ifelse(qb_dropback==1, ifelse(
      qb_scramble==1,skill_ID,passer_player_id),"None"),
    matchup = paste0(year, " Wk ", week, " ", away_team, " @ ", home_team)
  )%>% 
  filter(play_type2 != "no_play" & is.na(play_type2)==F & is.na(away_wp)==F) %>%
  filter(
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0)

#create the max_wk gvar, also needed to "look back" n weeks in the past #####
max_wk = max(pbp_df_adj$wk_ovr_rk, na.rm = T)


#write_csv(pbp_df_adj, path = paste0(app_path,'pbp_df.csv'))
#adjust the rosters_df for shiny consumption #####
rosters_df = rosters_df %>%
  mutate(player_info = paste0(player_info = paste0(Player,", ", Pos))) %>%
  write_csv("./rosters_df.csv")


#df for tab-1, win probability #####
win_prob_df = pbp_df_adj %>% 
  select(
    game_seconds_remaining, game_id, play_id, matchup, wk_ovr_rk,
    home_team_score, away_team_score, home_final_score, away_final_score, 
    home_wp, away_wp, home_team, away_team, year, week, desc, posteam) %>%
  write_csv(., './win_prob_df.csv')

#df for tab-2, team efficiency #####
pbp_df_adj %>% filter(
  !is.na(home_wp),
  !is.na(away_wp),
  timeout == 0) %>%
  mutate(
    apacr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    pacr = yards_gained/adj_opp_yds
  ) %>%
  select(
    epa, wpa, apacr, anya, pacr, wk_ovr_rk, posteam, defteam, week, year
  ) %>%
  write_csv(., './team_eff_df.csv')


#df for tab-3, QB analysis #####
pbp_df_adj %>% 
  filter(
    dropback_ID != "None",
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>% 
  transmute(
    desc = desc,
    qb_dropback = qb_dropback,
    matchup = matchup,
    year = year,
    wk_ovr_rk = wk_ovr_rk,
    wks_back = 1 + max_wk - wk_ovr_rk,
    qb_dropback = qb_dropback,
    yards_gained, opp_yds,
    dropback_ID = dropback_ID,
    apacr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    pacr = yards_gained/adj_opp_yds,
    epa = epa,
    wpa = wpa
  ) %>% left_join(rosters_df, by = c("year", "dropback_ID" = "GSIS_ID")) %>%
  write_csv('./qb_df.csv')

#df for tab-4, skill player analysis #####
team_agg_df = pbp_df_adj %>% 
  filter(
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>%
  group_by(game_id, posteam) %>%
  summarize(
    team_dropbacks = sum(qb_dropback, na.rm = T),
    team_pass_attempts = sum(pass_attempt, na.rm = T),
    team_rush_attempts = sum(rush_attempt, na.rm = T),
    team_opportunities = team_pass_attempts+team_rush_attempts,
    team_air_yards = sum(opp_yds, na.rm = T),
    team_total_yards = sum(yards_gained, na.rm = T)
  )

skill_df = pbp_df_adj %>% 
  filter(
    skill_ID != "None",
    qb_scramble == 0,
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>%
  transmute(
    desc = desc,
    game_id = game_id,
    year = year,
    posteam = posteam,
    target = pass_attempt,
    rush = rush_attempt,
    matchup = matchup,
    yards_gained= yards_gained,
    opp_yds = opp_yds,
    wk_ovr_rk = wk_ovr_rk,
    wks_back = 1 + max_wk - wk_ovr_rk,
    skill_ID = skill_ID,
    aracr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    racr = yards_gained/adj_opp_yds,
    epa = epa,
    wpa = wpa
  ) %>% left_join(rosters_df, by = c("year", "skill_ID" = "GSIS_ID")) %>%
  select(-name) %>% 
  mutate(Pos_grp = ifelse(Pos == "QB", "QB", ifelse(Pos %in% c("RB","FB"), "RB", "REC"))) %>%
  left_join(team_agg_df, by = c("game_id", "posteam")) %>% 
  group_by(skill_ID, game_id) %>%
  mutate(
    ms_team_rushes = round(sum(rush, na.rm=T)/mean(team_rush_attempts),3),
    ms_team_opportunities = round((sum(target, na.rm=T)+sum(rush, na.rm=T))/mean(team_opportunities),3),
    ms_team_targets = round(sum(target)/mean(team_pass_attempts),3),
    ms_team_air_yards = round(sum(opp_yds)/mean(team_air_yards),3),
    wopr = round(1/2.2 * (1.5 * ms_team_targets + 0.7 * ms_team_targets),3),
    awopr = round(1/2.2 * (1.5 * ms_team_opportunities + 0.7 * ms_team_targets),3)
    ) %>%
  write_csv("./skill_df.csv")

#df for tab-5, exploration #####
pbp_explore = pbp_df_adj %>%
  select(
    play_id, game_id, matchup, wk_ovr_rk, gamewk, game_seconds_remaining,
    play_type, ep, wp, epa, wpa, yards_gained, touchdown
    )  %>%
  write_csv("./explore_df.csv")

explore_df %>% 
  filter(play_type %in% c("Run","Pass"))