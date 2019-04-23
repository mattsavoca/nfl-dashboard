# libraries #####
library(tidyverse)
library(ggplot2)
library(nflscrapR)
library(plotly)

# app_path #####
app_path = '~/Dropbox/Matt\ Savoca/Courses/nycdsa/shinyapp/'


#pbp_df edits #####
pbp_df = pbp_df %>% 
  mutate(
    qb_scramble = ifelse(qb_scramble==1&play_type=="pass",0,qb_scramble),
    play_type2 = as.factor(ifelse(
                      qb_scramble==1|play_type=="pass", "dropback",play_type)),
    away_team_score = ifelse(posteam == home_team, defteam_score, posteam_score),
    home_team_score = ifelse(posteam == home_team, posteam_score, defteam_score),
    opp_yds = ifelse(air_yards < 0|is.na(air_yards), 0, air_yards),
    adj_opp_yds = ifelse(opp_yds == 0, 1, opp_yds),
    wk_ovr_rk = (year - min_year + 1) * week,
    #combine Rusher_ID and Receiver_ID variable into a skillplayer ID, 'skill_ID'
    skill_ID = ifelse(play_type2=="run",rusher_player_id,
        ifelse(play_type2=="dropback",receiver_player_id,NA)),
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



  

  

write_csv(pbp_df, 'pbp_df.csv')


#gVAR #####
max_wk = max(pbp_df$wk_ovr_rk, na.rm = T)
min_year = min(pbp_df$year,na.rm =T)




#DF tab-1 #####
pbp_df %>% filter(
  !is.na(home_wp),
  !is.na(away_wp),
  timeout == 0) %>%
  select(
    game_seconds_remaining, game_id, play_id, matchup, wk_ovr_rk,
    home_wp, away_wp, home_team, away_team, year, week, desc, posteam) %>%
  write_csv(., './win_prob_df.csv')

#DF tab-2 #####
team_eff_df = pbp_df %>% filter(
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


off_eff_df = team_eff_df %>%
  filter(wk_ovr_rk > max_wk - 9) %>%
  group_by(posteam) %>%
  summarize(   
            epa = mean(epa, na.rm = T),
            wpa = mean(wpa, na.rm = T),
            apacr = mean(apacr, na.rm = T),
            pacr = mean(pacr, na.rm = T),
            anya = mean(anya, na.rm = T)) 

team_eff_df = team_eff_df %>%
  filter(wk_ovr_rk > max_wk - 9) %>%
  group_by(defteam) %>%
  summarise(def_epa = mean(epa, na.rm = T),
            def_wpa = mean(wpa, na.rm = T),
            def_apacr = mean(apacr, na.rm = T),
            def_pacr = mean(pacr, na.rm = T),
            def_anya = mean(anya, na.rm = T)) %>% 
  full_join(off_eff_df, by = c("defteam" = "posteam"))



#DF tab-3 #####
qb_df = pbp_df %>% 
  filter(
    dropback_ID != "None",
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>%
  # filter(wk_ovr_rk >= max_wk - 8) %>% # **USER FILTER**
  transmute(
    qb_dropback = qb_dropback,
    year = year,
    wk_ovr_rk = wk_ovr_rk,
    qb_dropback = qb_dropback,
    yards_gained, opp_yds,
    dropback_ID = dropback_ID,
    apacr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    pacr = yards_gained/adj_opp_yds,
    epa = epa
  ) %>% left_join(rosters, 
                  by = c("year", "dropback_ID" = "GSIS_ID")) %>%
  select(-name, -yards_gained, -opp_yds) %>% write_csv(qb_df, "./qb_df.csv")



#REACTIVE - TAB 3 #####
qb_eff_graph = qb_df %>% group_by(dropback_ID) %>% mutate(
    total_dropbacks = sum(qb_dropback),
    mean_apacr = mean(apacr, na.rm =T)
  ) %>% ungroup() %>% filter(total_dropbacks > 20) %>%
  mutate(apacr_rk = dense_rank(-mean_apacr))


#GRAPH -  TAB 3 #####
qb_eff_graph %>%
  
  ggplot()+
  aes(x = Player, y = apacr, group = Player, fill = Player)+
  geom_violin(position = "dodge")+
  theme(legend.position = 'none')+
  coord_flip()








#DF tab-4 #####
skill_df = pbp_df %>% 
  filter(
    skill_ID != "None",
    qb_scramble == 0,
    !is.na(home_wp),
    !is.na(away_wp),
    timeout == 0) %>%
  transmute(
    wk_ovr_rk = wk_ovr_rk,
    skill_ID = skill_ID,
    aracr = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      adj_opp_yds,
    anya = (yards_gained + touchdown * 20 - 45 * (interception + fumble))/
      ifelse(pass_attempt+rush_attempt==0,1,pass_attempt+rush_attempt),
    racr = yards_gained/adj_opp_yds,
    epa = epa
  ) %>% left_join(rosters, 
                  by = c("year", "skill_ID" = "GSIS_ID")) %>%
  select(-name) %>% 
  mutate(Pos_grp = ifelse(Pos == "QB", "QB", 
                    ifelse(Pos %in% c("RB","FB"), "RB", "REC")))


write_csv(skill_df, "./skill_df.csv")

#REACTIVE - TAB 4 #####
temp_skill_df = 
skill_df %>% 
  filter(wk_ovr_rk >= max_wk - 9-1) %>% # **USER FILTER**
  group_by(skill_ID) %>% mutate(
    total_opps = n()/9,
    mean_aracr = mean(aracr, na.rm =T)
  ) %>% group_by(Pos_grp) %>% 
  mutate(
    aracr_rk = dense_rank(-mean_aracr),
    opps_rk = dense_rank(-total_opps)) 

#GRAPH -  TAB 4 #####
temp_skill_df %>%
  filter(is.na(Player)==F,
         opps_rk <= 5,
         Pos != "QB") %>%
  ggplot()+
  aes(x = reorder(Player,-aracr,median, na.rm =T), y = aracr, fill = Player)+
  geom_violin()+
  theme(legend.position = 'none')+
  labs(x = "Player")+
  geom_hline(yintercept = 1, linetype="dashed")+
  scale_y_continuous(limits = c(-1,4))+
  coord_flip()

#REACTIVE2 - TAB 4 #####
temp_skill_opp_df = pbp_df %>%
  filter(is.na(skill_ID)==F) %>%
  group_by(year, wk_ovr_rk, posteam) %>%
  mutate(
    tm_targets = sum(pass_attempt),
    tm_air_yards = sum(opp_yds)) %>%
  group_by(skill_ID, year, wk_ovr_rk) %>%
  mutate(
      targets = sum(pass_attempt),
      air_yards = sum(opp_yds),
      wopr = 1/2.2*(1.7 * (targets/tm_targets) + .7 * (air_yards/tm_air_yards))) %>% 
  select(skill_ID, wk_ovr_rk, year, wopr, targets, tm_targets, air_yards, tm_air_yards) %>% 
  left_join(rosters, by = c("year", "skill_ID" = "GSIS_ID")) %>%
  mutate(Pos_grp = ifelse(Pos == "QB", "QB", 
                          ifelse(Pos %in% c("RB","FB"), "RB", "REC"))) %>%
  filter(wk_ovr_rk >= max_wk - 9-1) %>% #USER FILTER


#GRAPH2 -  TAB 4 #####
temp_skill_opp_df %>% group_by(Player) %>%
  summarize_if(is.numeric, mean, na.rm =T) %>%
  arrange(-wopr) %>%
  top_n(20, wopr) %>%
  mutate(
    Player = reorder(Player, wopr)) %>%
  ggplot()+
  aes(x = Player, y = wopr, fill = Player)+
  geom_col()+
  coord_flip()+
  theme(legend.position = 'none')+
  labs(y = "Weighted Percentage of Team Targets and Air Yards (WOPR)")

# graphing scratchwork #####
win_prob %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed")+
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = paste0(wp$year," Week ", unique(wp$week)," Probability Chart"),
    subtitle = paste0(unique(wp$away_team), " (away) vs. ", unique(wp$home_team), " (home)"),
    caption = "Data from nflscrapR"
  )


wp_game = win_prob_df %>% filter(game_id == 2016091107) %>%
  mutate(tooltip = paste0('\n Possession: ', posteam,". \n",desc),
         game_remaining = round(game_seconds_remaining/3600,3)) 

wp_graph %>% 
  ggplot()+
  aes(x = game_remaining, y = wpa, color = team, text = tooltip, group = 1)+
  scale_x_reverse(breaks = c(1,0.5,0))+
  geom_point(size = .1)+geom_line(size = 2)

ggplotly(wp_graph)