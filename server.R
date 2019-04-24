shinyServer(function(input, output, session){
  # reactive data frames #####
  win_prob_reactive = reactive({
    win_prob_df %>% 
      filter(year == input$wp_season) %>%
      filter(week == input$wp_week) %>%
      filter(away_team == input$wp_team|home_team == input$wp_team) %>%
      mutate(wpa = round(wpa, 3))
  })
  
  team_off_reactive = reactive({
    team_off_df = team_eff_df %>%
      filter(wk_ovr_rk > (max_wk - as.numeric(input$weeks_back))) %>%
      group_by(posteam, wks_back) %>%
      summarize(   
        epa = mean(epa, na.rm = T),
        wpa = mean(wpa, na.rm = T),
        apacr = mean(apacr, na.rm = T),
        pacr = mean(pacr, na.rm = T),
        anya = mean(anya, na.rm = T))
  })
  
  qb_reactive = reactive({
   qb_df = qb_df %>% 
      filter(wk_ovr_rk > (max_wk - as.numeric(input$qb_weeks_back))) %>%
      filter( !(player_info %in%  input$qb_filter)) %>%
      group_by(dropback_ID) %>% mutate(
        Player = Player, 
        total_dropbacks = sum(qb_dropback),
        apacr = mean(apacr, na.rm =T),
        pacr = mean(pacr, na.rm =T),
        epa = mean(epa, na.rm =T),
        wpa = mean(wpa, na.rm =T),
        anya = mean(anya, na.rm =T),
        yards_gained = sum(yards_gained)
      ) %>% ungroup()  %>%
      filter(total_dropbacks >= max(total_dropbacks)*.55) %>%
      transmute(
        matchup = matchup,
        total_dropbacks = total_dropbacks,
        Player = Player,
        total_yards = yards_gained,
        total_yards_rk = dense_rank(-total_yards),
        apacr = apacr,
        apacr_rk = dense_rank(-apacr),
        pacr = pacr,
        pacr_rk = dense_rank(-pacr),
        epa = epa,
        epa_rk = dense_rank(-epa),
        wpa = wpa,
        wpa_rk = dense_rank(-wpa),
        anya = anya,
        anya_rk = dense_rank(-anya)
      ) 
  })
  
  qb_indiv_reactive = reactive({
    qb_indiv_df = qb_df %>% 
      filter(wk_ovr_rk > (max_wk - as.numeric(input$qb_weeks_back))) %>%
      filter(Player == input$qb_indiv) %>%
      filter( !(player_info %in%  input$qb_filter))
    
  })
  
  skill_reactive =  reactive({
    skill_df %>% 
      filter(wk_ovr_rk > (max_wk - as.numeric(input$skill_weeks_back))) %>%
      filter(Pos %in% input$checkGroup) %>%
      filter( !(player_info %in%  input$skill_filter)) %>%
      group_by(skill_ID) %>% mutate(
        Player = Player, 
        total_opps = n(),
        ms_team_opportunities = mean(ms_team_opportunities, na.rm = T),
        ms_team_rushes = mean(ms_team_rushes, na.rm = T),
        ms_team_targets = mean(ms_team_targets, na.rm = T),
        ms_team_air_yards = mean(ms_team_air_yards, na.rm = T),
        wopr = mean(wopr,  na.rm = T),
        awopr = mean(awopr,  na.rm = T),
        aracr = mean(aracr, na.rm =T),
        racr = mean(racr, na.rm =T),
        epa = mean(epa, na.rm =T),
        wpa = mean(wpa, na.rm =T),
        anya = mean(anya, na.rm =T)
      ) %>% ungroup()  %>%
      filter(total_opps >= max(total_opps,na.rm = T)*.35  ) %>%
      transmute(
        total_opps = total_opps,
        Player = Player,
        aracr = aracr,
        aracr_rk = dense_rank(-aracr),
        racr = racr,
        racr_rk = dense_rank(-racr),
        epa = epa,
        epa_rk = dense_rank(-epa),
        wpa = wpa,
        wpa_rk = dense_rank(-wpa),
        anya = anya,
        anya_rk = dense_rank(-anya),
        ms_team_opportunities, ms_team_opportunities_rk = dense_rank(-ms_team_opportunities), 
        ms_team_rushes, ms_team_rushes_rk = dense_rank(-ms_team_rushes), 
        ms_team_targets, ms_team_targets_rk = dense_rank(-ms_team_targets),
        ms_team_air_yards, ms_team_air_yards_rk = dense_rank(-ms_team_air_yards),
        wopr, wopr_rk = dense_rank(-wopr),
        awopr, awopr_rk = dense_rank(-awopr)
      ) 
  })
  
  skill_indiv_reactive = reactive({
    skill_indiv_df = skill_df %>% 
      filter(wk_ovr_rk > (max_wk - as.numeric(input$skill_weeks_back))) %>%
      filter(Player == input$skill_indiv) %>%
      filter(Pos %in% input$checkGroup) %>%
      filter( !(player_info %in%  input$qb_filter))
  })
  
  explore_reactive = reactive({
    explore_df %>% 
      filter(wk_ovr_rk > (max_wk - as.numeric(input$explore_weeks_back)))
  })

  team_def_reactive = reactive({
    team_eff_df %>%
      filter(wk_ovr_rk > (max_wk - as.numeric(input$weeks_back))) %>%
      group_by(defteam, wks_back) %>%
      summarise(def_epa = mean(epa, na.rm = T),
                def_wpa = mean(wpa, na.rm = T),
                def_apacr = mean(apacr, na.rm = T),
                def_pacr = mean(pacr, na.rm = T),
                def_anya = mean(anya, na.rm = T)) 
  })
  
  team_eff_reactive = reactive({
    full_join(team_def_reactive(), team_off_reactive(), by = c("defteam" = "posteam", "wks_back"))
  })
  
  team_eff_means_reactive = reactive({
    team_eff_reactive() %>%
      group_by(wks_back) %>%
      summarize_if(is.numeric, mean, ra.rm = T)
  })
  
  # plot outputs - win probability charts #####  
  output$win_prob <- renderPlotly({
   win_prob_reactive() %>%
      mutate(
        tooltip = paste0(matchup, '\n Possession: ', posteam,". \n",desc,"\n",
                         "Score: ", away_team_score, "-", home_team_score
                         ),
        game_remaining = round(game_seconds_remaining/3600,3)) %>%
      ggplot()+
      aes(x = game_remaining, y = wpa, color = team, text = tooltip, group = 1)+
      scale_x_reverse(breaks = c(1,0.5,0))+
      geom_point(size = .1)+geom_line(size = 1.5)+
      labs(
        title = paste0(unique(win_prob_reactive()$matchup),
                       ".\nFinal Score: ",
                       unique(win_prob_reactive()$away_final_score), "-",
                       unique(win_prob_reactive()$home_final_score), " ",
                        ifelse(unique(win_prob_reactive()$away_final_score) >
                               unique(win_prob_reactive()$home_final_score),
                               unique(win_prob_reactive()$away_team),
                               unique(win_prob_reactive()$home_team))
                       ),
        x = "Gametime Remaining",
        y = "Win Probability"
      )
  })
  
  # plot outputs - explore tab #####  
  output$explore_ep <- renderPlotly({
    p = explore_reactive() %>%
      group_by(play_type) %>%
      summarize_if(is.numeric, mean, na.rm = T) %>%
      mutate(play_type = reorder(play_type, ep, mean)) %>%
      ggplot()+
      aes(y = ep, x = play_type, group = play_type, fill = play_type)+
      geom_col(position = 'dodge')+
      coord_flip()+
      labs(x = "", y = "", title = "Average Expected Points by Play Type")
    hide_legend(p)
  })

  output$explore_wp <- renderPlotly({
    p = explore_reactive() %>%
      group_by(play_type) %>%
      summarize_if(is.numeric, mean, na.rm = T) %>%
      mutate(play_type = reorder(play_type, wp, mean)) %>%
      ggplot()+
      aes(y = wp, x = play_type, group = play_type, fill = play_type)+
      geom_col(position = 'dodge')+
      coord_flip()+
      labs(x = "", y = "", title = "Average Win Probability by Play Type")
    hide_legend(p)
  })
  
  output$explore_pass_and_rush <- renderPlotly({
    p = explore_reactive() %>%
      filter(play_type == "run" | play_type == "pass") %>%
      ggplot()+
      aes(x = epa, fill = play_type, group = play_type)+
      geom_density(alpha = .6)+
      scale_x_continuous(breaks = seq(-10,10,2.5))+
      labs(x = "Expected Points Added", y = "Percent of Total Plays",
           title = "Distribution  of Expected Points Added, Run Plays vs Pass Plays")
  })
  
  output$explore_pass_rush_scatter <- renderPlot({
    p = explore_reactive() %>%
      filter(play_type == "run" | play_type == "pass") %>%
      ggplot()+
      aes(x = wpa, y = epa, color = play_type)+
      geom_smooth()+
      geom_point(alpha = .3)+
      scale_x_continuous(limits = c(-0.5,0.5))+
      labs(x = "Win Probability Added",
           y = "Expected Points Added",
           title = paste0("Win Probability Added by Expected Points Added, Past ", input$explore_weeks_back, " Weeks."))
    
    ggExtra::ggMarginal(
      p = p,
      type = 'density',
      margins = 'both',
      size = 5,
      colour = '#A69191',
      fill = 'gray'
    )
  })
  # plot outputs - team efficiency tab #####  
    
  output$team_epa <- renderPlotly({
   team_eff_reactive() %>%
      ggplot()+
      aes(x = epa, y = def_epa, fill = defteam, frame = wks_back)+
      geom_point(size = 2)+
      scale_y_reverse()+
      labs(
        x = "Offense Expected Points Added",
        y = "Defensive Expected Points Allowed",
        title = paste0("Team Expected Points Added vs Allowed, Past ", input$weeks_back, " Weeks.")
      ) %>%
      animation_opts(frame = 50000, 
                     easing = "linear", redraw = TRUE, mode = "immediate")
  })
  
  output$team_pacr <- renderPlotly({
    team_eff_reactive() %>%
      ggplot()+
      aes(x = pacr, y = def_pacr, fill = defteam, frame = wks_back)+
      geom_point(size = 2)+
      scale_y_reverse()+
      labs(
        x = "Offense Passing Air Conversion Ratio",
        y = "Defensive Offense Passing Air Conversion Ratio Allowed",
        title = paste0("Team Passer Air Convertion Rate (PACR) Added vs Allowed, Past ", input$weeks_back, " Weeks.")
      )
  })
  
  output$team_apacr <- renderPlotly({
    team_eff_reactive() %>%
      ggplot()+
      aes(x = apacr, y = def_apacr, fill = defteam, frame = wks_back)+
      geom_point(size = 2)+
      scale_y_reverse()+
      labs(
        x = "Offense Adjusted Passing Air Conversion Ratio",
        y = "Defensive Offense Adjusted Passing Air Conversion Allowed",
        title = paste0("Team Adjusted Passer Air Convertion Rate (aPACR) Added vs Allowed, Past ", input$weeks_back, " Weeks.")
      )
  })
  
  output$team_anya <- renderPlotly({
    team_eff_reactive() %>%
      ggplot()+
      aes(x = anya, y = def_anya, fill = defteam, frame = wks_back)+
      geom_point(size = 2)+
      scale_y_reverse()+
      labs(
        x = "Offense Adjusted Net Yards per Attempt",
        y = "Defensive Offense Adjusted Net Yards per Attempt Allowed",
        title = paste0("Team Adjusted Net Yards Per Attempt Added vs Allowed, Past ", input$weeks_back, " Weeks.")
      )
  })
  
  output$team_wpa <- renderPlotly({
    team_eff_reactive() %>%
      ggplot()+
      aes(x = wpa, y = def_wpa, fill = defteam, frame = wks_back)+
      geom_point(size = 2)+
      scale_y_reverse()+
      labs(
        x = "Offense Adjusted Net Yards per Attempt",
        y = "Defensive Offense Adjusted Net Yards per Attempt Allowed"
      )
  })
  # plot outputs - qb efficeincy tab #####  
  
  output$qb_epa <- renderPlotly({
    p = qb_reactive() %>% 
      mutate(Player = reorder(Player, epa, mean)) %>%
      filter(epa_rk < 30) %>%
      arrange(epa) %>%
      ggplot()+
      aes(x = Player, y = epa, fill = Player)+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(x = "", y = "Expected Points Added",
           title = paste0("Expected Points Added, Past ", input$skill_weeks_back, " Weeks."))
    hide_legend(p)
  })
  
  output$qb_wpa <- renderPlotly({
    p = qb_reactive() %>% 
      mutate(Player = reorder(Player, wpa, mean)) %>%
      filter(wpa_rk < 30) %>%
      arrange(wpa) %>%
      ggplot()+
      aes(x = Player, y = wpa, fill = Player)+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(x = "", y = "Win Probability Added",
           title = paste0("Win Probability Added, Past ", input$skill_weeks_back, " Weeks."))
    
    hide_legend(p)
  })
  
  output$qb_pacr <- renderPlotly({
   p =  qb_reactive() %>% 
      mutate(Player = reorder(Player, pacr, mean)) %>%
      filter(epa_rk < 30) %>%
      arrange(pacr) %>%
      ggplot()+
      aes(x = Player, y = pacr, fill = Player)+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(x = "", y = "Passer Air Conversion Ratio (PACR)",
           title = paste0("Passer Air Conversion Ratio (RACR), Past ", input$qb_weeks_back, " Weeks."))
   hide_legend(p)
  })
  
  output$qb_apacr <- renderPlotly({
   p =  qb_reactive() %>% 
      mutate(Player = reorder(Player, apacr, mean)) %>%
      filter(apacr_rk < 30) %>%
      arrange(apacr) %>%
      ggplot()+
      aes(x = Player, y = apacr, fill = Player)+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(x = "", y = "Adjusted Passer Air Conversion Ratio",
           title = paste0("Adjusted Passer Air Conversion Ratio (RACR), Past ", input$qb_weeks_back, " Weeks."))
   
   hide_legend(p)
  })
  
  output$qb_anya <- renderPlotly({
   p = qb_reactive() %>% 
      mutate(Player = reorder(Player, anya, mean)) %>%
      filter(anya_rk < 30) %>%
      arrange(anya) %>%
      ggplot()+
      aes(x = Player, y = anya, fill = Player)+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(
        x = "",
        y = "Adjusted net Yards per Attempt",
        title = paste0("Adjusted Yards per Attempt by Air Yards, Past ", input$qb_weeks_back, " Weeks.")
      )
   
   hide_legend(p)
  })
  
  output$qb_total_yards <- renderPlotly({
    p = qb_reactive() %>% 
      mutate(Player = reorder(Player, total_yards, mean)) %>%
      filter(total_yards_rk < 30) %>%
      arrange(total_yards) %>%
      ggplot()+
      aes(x = Player, y = total_yards, fill = Player)+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(x = NULL,  y = "Total Yards Gained",
           title = paste0("Total Yards Gained (Passing + Rushing), Past ", input$qb_weeks_back," weeks"))
    hide_legend(p)
  })
  # plot outputs - qb individual tab #####  
  
  output$indiv_qb_epa <- renderPlotly({
    p = qb_indiv_reactive() %>%
      mutate(tooltip = paste0(desc,"\nAir Yards: ", round(opp_yds,3),"\nEPA: ",round(epa, 3))) %>%
      ggplot()+
      aes(x = opp_yds, y = epa)+
      geom_point(aes(fill = matchup, text = tooltip))+
      geom_smooth(se=F)+
      geom_smooth(data = qb_summary_df, aes(x = opp_yds, y = epa), color = NA, fill = "orange")+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Expected Points Added",
        title = paste0("Expected Points Added by Air Yards, Past ", input$skill_weeks_back, " Weeks.")
      )
    hide_legend(p)
  })
  
  output$indiv_qb_wpa <- renderPlotly({
    p = qb_indiv_reactive() %>%
      mutate(tooltip = paste0(desc,"\nAir Yards: ", round(opp_yds,3),"\nWPA: ",round(wpa, 3))) %>%
      ggplot()+
      aes(x = opp_yds, y = wpa)+
      geom_point(aes(fill = matchup, text = tooltip))+
      geom_smooth(se=F)+
      geom_smooth(data = qb_summary_df, aes(x = opp_yds, y = wpa), color =  NA, fill = "orange")+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Win Probability Added",
        title = paste0("Win Probability Added by Air Yards, Past ", input$skill_weeks_back, " Weeks.")
      )
    hide_legend(p)
  })
  
  output$indiv_qb_pacr <- renderPlotly({
   p = qb_indiv_reactive() %>%
      mutate(tooltip = paste0(desc,"\nAir Yards: ", round(opp_yds,3),"\nPACR: ",round(pacr, 3))) %>%
      ggplot()+
      aes(x = opp_yds, y = pacr)+
      geom_point(aes(fill = matchup, text =  tooltip))+
      geom_smooth(se = F)+
      geom_smooth(data = qb_summary_df, aes(x = opp_yds, y = pacr), color = NA, fill = "orange")+
      scale_x_continuous(limits = c(0,45))+
      scale_y_continuous(limits = c(-1,5))+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Yards Gained per Yards Ball Travelled in the Air (aPACR)",
        title = paste0("Passer Air Convertion Ratio by Air Yards, Past ", input$skill_weeks_back, " Weeks.")
      )
   hide_legend(p)
  })
  
  output$indiv_qb_apacr <- renderPlotly({
    p = qb_indiv_reactive() %>% 
      mutate(tooltip = paste0(desc,"\nAir Yards: ", round(opp_yds,3),"\naPACR: ",round(apacr, 3))) %>%
      ggplot()+
      aes(x = opp_yds, y = apacr)+
      geom_point(aes(fill = matchup, text =  tooltip))+
      geom_smooth(se = F)+
      geom_smooth(data = qb_summary_df, aes(x = opp_yds, y = apacr), color = NA, fill = "orange")+
      scale_x_continuous(limits = c(0,45))+
      scale_y_continuous(limits = c(-1,5))+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Yards Gained per Yards Ball Travelled in the Air (aPACR)",
        title = paste0("Adjusted Passer Air Convertion Ratio by Air Yards, Past ", input$skill_weeks_back, " Weeks.")
      )
    hide_legend(p)
  })
  
  output$indiv_qb_anya <- renderPlotly({
    p = qb_indiv_reactive() %>%
      mutate(tooltip = paste0(desc,"\nAir Yards: ", round(opp_yds,3),"\nAYA: ",round(anya, 3))) %>%
      ggplot()+
      aes(x = opp_yds, y = anya)+
      geom_point(aes(fill = matchup, text = tooltip))+
      geom_smooth(se = F)+
      geom_smooth(data = qb_summary_df, aes(x = opp_yds, y = anya), color = NA, fill = "orange")+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Adjusted net Yards per Attempt",
        title = paste0("Adjusted Net Yards Per Attempt by Air Yards, Past ", input$skill_weeks_back, " Weeks.")
      )
    hide_legend(p)
  })
  
  # plot outputs - skill position - efficiency tab #####
  
  output$skill_epa <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, epa, mean)) %>%
      filter(epa_rk < 30) %>%
      arrange(epa) %>%
      ggplot()+
      aes(x = Player, y = epa, fill = Player)+
      labs(x = "", y = "Expected Points Added",
           title = paste0("Expected Points Added, Past ", input$skill_weeks_back, " Weeks."))+
      geom_col(position = "dodge")+
      coord_flip()
    hide_legend(p)
  })
  
  output$skill_wpa <- renderPlotly({
   p =  skill_reactive() %>% 
      mutate(Player = reorder(Player, wpa, mean)) %>%
      filter(wpa_rk < 30) %>%
      arrange(wpa) %>%
      ggplot()+
      aes(x = Player, y = wpa, fill = Player)+
      labs(x = "", y = "Win Probability Added",
          title = paste0("Win Probability Added, Past ", input$skill_weeks_back, " Weeks."))+      
      geom_col(position = "dodge")+
      coord_flip()
   hide_legend(p)
  })
  
  output$skill_racr <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, racr, mean)) %>%
      filter(epa_rk < 30) %>%
      arrange(racr) %>%
      ggplot()+
      aes(x = Player, y = racr, fill = Player)+
      geom_col(position = "dodge")+
      labs(x = "", y = "Reciver Air Conversion Ratio",
           title = paste0("Reciver Air Conversion Ratio (RACR), Past ", input$skill_weeks_back, " Weeks."))+
      coord_flip()
    hide_legend(p)
  })
  
  output$skill_aracr <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, aracr, mean)) %>%
      filter(aracr_rk < 30) %>%
      arrange(aracr) %>%
      ggplot()+
      aes(x = Player, y = aracr, fill = Player)+
      geom_col(position = "dodge")+
      labs(x = "", y = "Adjusted Reciver Air Conversion Ratio",
           title = paste0("Adjusted Reciver Air Conversion Ratio (RACR), Past ", input$skill_weeks_back, " Weeks."))+
      coord_flip()
    hide_legend(p)
  })

  output$skill_anya <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, anya, mean)) %>%
      filter(anya_rk < 30) %>%
      arrange(anya) %>%
      ggplot()+
      aes(x = Player, y = anya, fill = Player)+
      labs(x = "", y = "Adjusted Net Yards per Opportunity",
           title = paste0("Adjusted Net Yards per Opportunity ", input$skill_weeks_back, " Weeks."))+
      geom_col(position = "dodge")+
      coord_flip()
    hide_legend(p)
  })
  
  # plot outputs - skill position - opportunity tab #####
  
  output$msopp <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, ms_team_opportunities, mean)) %>%
      filter(ms_team_opportunities_rk < 30) %>%
      arrange(ms_team_opportunities) %>%
      ggplot()+
      aes(x = Player, y = ms_team_opportunities, fill = Player)+
      labs(x = "", y = "Share of Team Opportunities",
           title = paste0("Player Share of Team Opportunities, Past ", input$skill_weeks_back, " Weeks."))+
      geom_col(position = "dodge")+
      coord_flip()
    hide_legend(p)
  })
  
  output$msrush <- renderPlotly({
    p =  skill_reactive() %>% 
      mutate(Player = reorder(Player, ms_team_rushes, mean)) %>%
      filter(ms_team_rushes_rk < 30) %>%
      arrange(ms_team_rushes) %>%
      ggplot()+
      aes(x = Player, y = ms_team_rushes, fill = Player)+
      labs(x = "", y = "Share of Team Rush Attempts",
           title = paste0("Player Share of Team Rush Attempts, Past ", input$skill_weeks_back, " Weeks."))+      
      geom_col(position = "dodge")+
      coord_flip()
    hide_legend(p)
  })
  
  output$mstrg <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, ms_team_targets, mean)) %>%
      filter(ms_team_targets_rk < 30) %>%
      arrange(ms_team_targets) %>%
      ggplot()+
      aes(x = Player, y = ms_team_targets, fill = Player)+
      geom_col(position = "dodge")+
      labs(x = "", y = "Share of Team Targets",
           title = paste0("Player Share of Team Targets, Past ", input$skill_weeks_back, " Weeks."))+
      coord_flip()
    hide_legend(p)
  })
  
  output$msair <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, ms_team_air_yards, mean)) %>%
      filter(ms_team_air_yards_rk < 30) %>%
      arrange(ms_team_air_yards) %>%
      ggplot()+
      aes(x = Player, y = ms_team_air_yards, fill = Player)+
      geom_col(position = "dodge")+
      labs(x = "", y = "Share of Team Air Yards (Yards Travelled in the Air)",
           title = paste0("Player Share of Team Air Yards, Past ", input$skill_weeks_back, " Weeks."))+
      coord_flip()
    hide_legend(p)
  })
  
  output$wopr <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, wopr, mean)) %>%
      filter(wopr_rk < 30) %>%
      arrange(wopr) %>%
      ggplot()+
      aes(x = Player, y = wopr, fill = Player)+
      labs(x = "", y = "Weighted Opportunity Rating",
           title = paste0("Player Weighted Opportunity Rating, Past ", input$skill_weeks_back, " Weeks."))+
      geom_col(position = "dodge")+
      coord_flip()
    hide_legend(p)
  })
  
  output$awopr <- renderPlotly({
    p = skill_reactive() %>% 
      mutate(Player = reorder(Player, awopr, mean)) %>%
      filter(awopr_rk < 30) %>%
      arrange(awopr) %>%
      ggplot()+
      aes(x = Player, y = awopr, fill = Player)+
      labs(x = "", y = "Adjusted/Weighted Opportunity Rating",
           title = paste0("Player Adjusted/Weighted Opportunity Rating, Past ", input$skill_weeks_back, " Weeks."))+
      geom_col(position = "dodge")+
      coord_flip()
    hide_legend(p)
  })
  
  # plot outputs - skill position individual tab #####  
  
  output$indiv_skill_epa <- renderPlotly({
    p = skill_indiv_reactive() %>% 
      mutate(tooltip = paste0(desc,"\nAir Yards: ", round(opp_yds,3),"\nEPA: ",round(epa, 3))) %>%
      ggplot()+
      aes(x = opp_yds, y = epa)+
      geom_point(aes(fill = matchup, text = tooltip))+
      geom_smooth(se=F)+
      geom_smooth(data = skill_summary_df, aes(x = opp_yds, y = epa), color = NA, fill = "orange")+
      scale_x_continuous(limits = c(0,45))+
      labs(
        x = "Yards Ball Travelled in the Air",
        y = "Expected Points Added",
        title = paste0("Expected Points Added by Yards Ball Travelled in the Air, Past ", input$skill_weeks_back, " weeks.")
      ) 
   hide_legend(p)
   
  })
  
  output$indiv_skill_wpa <- renderPlotly({
    p = skill_indiv_reactive() %>% 
      ggplot()+
      aes(x = opp_yds, y = wpa)+
      geom_smooth(se=F)+
      geom_point(aes(fill = matchup))+
      geom_smooth(data = skill_summary_df, aes(x = opp_yds, y = wpa), color = NA, fill = "orange")+
      scale_x_continuous(limits = c(0,45))+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Win Probability Added",
        title = paste0("Win Probability Added by Yards Ball Travelled in the Air, Past ", input$skill_weeks_back, " weeks.")
      )
    
    hide_legend(p)
  })
  
  output$indiv_skill_racr <- renderPlotly({
    p = skill_indiv_reactive() %>% 
      ggplot()+
      aes(x = opp_yds, y = racr)+
      geom_point(aes(fill = matchup))+
      geom_smooth(se=F)+
      geom_smooth(data = skill_summary_df, aes(x = opp_yds, y = racr), color = NA, fill = "orange")+
      scale_x_continuous(limits = c(0,45))+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Yards Gained per Yards Ball Travelled in the Air (RACR)",
        title = paste0("Receiver Air Conversion Ratio by Yards Ball Traveled in the Air, Past ", input$skill_weeks_back, " weeks.")
      )
    hide_legend(p)
  })
  
  output$indiv_skill_aracr <- renderPlotly({
    p = skill_indiv_reactive() %>% 
      ggplot()+
      aes(x = opp_yds, y = aracr)+
      geom_point(aes(fill = matchup))+
      geom_smooth(se=F)+
      geom_smooth(data = skill_summary_df, aes(x = opp_yds, y = aracr), color = NA, fill = "orange")+
      scale_x_continuous(limits = c(0,45))+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Adjusted Yards Gained per Yards Ball Travelled in the Air (aRACR)",
        title = paste0("Adjusted Receiver Air Conversion Ratio by Yards Ball Traveled in the Air, Past ", input$skill_weeks_back, " weeks.")
      )
    hide_legend(p)
  })
  
  output$indiv_skill_anya <- renderPlotly({
    p = skill_indiv_reactive() %>% 
      ggplot()+
      aes(x = opp_yds, y = anya)+
      geom_point(aes(fill = matchup))+
      geom_smooth(se=F)+
      geom_smooth(data = skill_summary_df, aes(x = opp_yds, y = anya), color = NA, fill = "orange")+
      labs(
        x = "Yards Ball Traveled in the Air",
        y = "Adjusted net Yards per Attempt",
        title = paste0("Adjusted Yards per Opportunity by Yards Ball Traveled in the Air, Past ", input$skill_weeks_back, " weeks.")
      )
    hide_legend(p)
  })
  
  # observe functions #####
  observe({
    wp_teams_observe =  
      sort(unique(win_prob_df %>% 
                    filter(year == input$wp_season, week == input$wp_week) %>%
                    .$posteam))
    
    qb_indiv_observe = 
      sort(unique(qb_df %>%
                    filter(wk_ovr_rk > (max_wk - as.numeric(input$qb_weeks_back))) %>%
                    filter( !(player_info %in%  input$qb_filter)) %>%
                    mutate(Player = reorder(Player, -qb_dropback, sum, na.rm=T)) %>%
                    .$Player))
    
    updateSelectInput(
      session, "qb_indiv",
      choices = qb_indiv_observe,
      selected = qb_indiv_observe[1]
    )
    
    skill_indiv_observe = 
      sort(unique(skill_df %>%
                    filter(wk_ovr_rk > (max_wk - as.numeric(input$skill_weeks_back))) %>%
                    filter(Pos %in% input$checkGroup) %>%
                    filter( !(player_info %in%  input$qb_filter)) %>%
                    mutate(Player = reorder(Player, -ms_team_opportunities, mean, na.rm=T)) %>%
                    .$Player))
    
    updateSelectInput(
      session, "skill_indiv",
      choices = skill_indiv_observe,
      selected = skill_indiv_observe[1]
    )
    
    updateSelectInput(
      session, "wp_team",
      choices = wp_teams_observe,
      selected = wp_teams_observe[1])
  })
  
  # scratchwork for future development
  # output$select_value <- renderPrint({ input$filter_select})
  # filtered <- reactiveValues(player_info = NULL)
  # observeEvent(input$filter_action , {filtered$player_info <- input$filter_select})
  # output$filter_value <- renderPrint({filtered})
    
  
  

  
})