library(tidyverse)
library(nflscrapR)

app_path = '~/Dropbox/Matt\ Savoca/Courses/nycdsa/shinyapp/'
scrapr_data_path = '~/Dropbox/Matt\ Savoca/Courses/nycdsa/02_r/nflscrapR-data/'

#gather and clean the play-by-play data #####
pbp_09_15_file_list = 
  list.files(
    path = paste0(scrapr_data_path,"play_by_play_data/regular_season/"), 
    pattern = "*.csv")


pbp_09_15 = do.call(
  "rbind",
  lapply(
    pbp_09_15_file_list,
    function(x)
      read_csv(
        paste0(
          scrapr_data_path, 'play_by_play_data/regular_season/',
          x),
        progress = T) %>%
      mutate(year = substr(x, nchar(x) - 7, nchar(x) - 4),
             touchback = NA)))

pbp_16_18_file_list = 
  list.files(
    path = paste0(scrapr_data_path,"play_by_play_data/regular_season/16-18/"), 
    pattern = "*.csv")


pbp_16_18 = do.call(
  "rbind",
  lapply(
    pbp_16_18_file_list,
    function(x)
      read_csv(
        paste0(
          scrapr_data_path, 'play_by_play_data/regular_season/16-18/',
          x),
        progress = T) %>%
      mutate(year = substr(x, nchar(x) - 7, nchar(x) - 4))))

rbind(pbp_09_15, pbp_16_18) %>% write_csv(paste0(app_path,'./www/pbp.csv'))

#gather and cleanr the game by game data #####
gbg_file_list = list.files(
  path = paste0(scrapr_data_path,"games_data/regular_season/"), 
  pattern = "*.csv")

gbg  =
  do.call("rbind",
          lapply(gbg_file_list,
                 function(x)
                   read.csv(
                     paste0(scrapr_data_path, 'games_data/regular_season/', x),
                     header = T,
                     stringsAsFactors = FALSE
                   )))
write_csv(gbg, paste0(app_path,'./www/gbg.csv'))

#gather and clean the roster data #####
roster_list = list.files(
  path = paste0(scrapr_data_path,"legacy_data/team_rosters/"), pattern = "*.csv")


rosters = do.call(
  "rbind",
  lapply(roster_list,
         function(x)
           read.csv(
             paste0(scrapr_data_path,'legacy_data/team_rosters/', x),
             header = T,
             stringsAsFactors = FALSE)
  )) %>% rename(year = Season)

write_csv(rosters, paste0(app_path,'./www/rosters.csv'))


# roster files ORIGINALLY did not include 2018, grabbing them using the nflscrapr package #####
    # teams = sort(unique(gbg %>% filter(season == 2018) %>% .$home_team))

# splitting into 3 separate dl because NFL servers kept bumping me...
    # team_2018_rosters_1 = nflscrapR::season_rosters(season = 2018, teams = teams[1:11])
    # write_csv(team_2018_rosters_1, 
           #path = paste0(scrapr_data_path,'legacy_data/team_rosters/','team_2018_rosters_1.csv'))
    # team_2018_rosters_2 = nflscrapR::season_rosters(season = 2018, teams = teams[12:22])
    # write_csv(team_2018_rosters_2, 
           #path = paste0(scrapr_data_path,'legacy_data/team_rosters/','team_2018_rosters_2.csv'))
    # team_2018_rosters_3 = nflscrapR::season_rosters(season = 2018, teams = teams[23:32])
    # write_csv(team_2018_rosters_3, 
           #path = paste0(scrapr_data_path,'legacy_data/team_rosters/','team_2018_rosters_3.csv'))

# bundle it with twine
    # team_2018_rosters_list = list(team_2018_rosters_1, team_2018_rosters_2, team_2018_rosters_3)
    # team_2018_rosters = team_2018_rosters_list %>% reduce(full_join) %>% rename(year = Season)
    # write_csv(team_2018_rosters, 
           #path = paste0(scrapr_data_path,'legacy_data/team_rosters/','team_2018_rosters.csv'))

# add it to the rest of the bunch
    # full_rosters = full_join(team_2018_rosters, rosters)
    # write_csv(full_rosters, paste0(app_path, './www/rosters.csv'))