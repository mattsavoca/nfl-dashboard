shinyUI(dashboardPage(
  # Header and Skin #####
  skin = "green",
  dashboardHeader(title = 'NFL Dashboard'),
  # SidebarMenu #####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Rewind", tabName = "winprob", icon = icon("line-chart")),
      menuItem("Team Efficiency Analysis", tabName = "team", icon = icon("bar-chart-o")),
      menuItem("Quarterback Analysis", tabName = "qb", icon = icon("users")),
      menuItem("Skill Position Analysis", tabName = "skill", icon = icon("child")),
      menuItem("Leaguewide Trend Explorer", tabName = "explore", icon = icon("binoculars")),
      selectizeInput('qb_filter', 'Filter a QB from Individual Analysis', 
                     choices = sort(unique(qb_df %>% filter(Pos == "QB") %>% .$player_info)), multiple = TRUE),
      selectizeInput('skill_filter', 'Filter a Skill Player from Individual Analysis', 
                     choices = sort(unique(skill_df %>% filter(Pos != "QB") %>% .$player_info)), multiple = TRUE)
    )
  ),
  # Body #####
  dashboardBody(
    tabItems(
      # Exploration Tab #####
      tabItem(tabName = "explore",
              selectInput("explore_weeks_back", h5("Past Weeks to Analyze"), selected = 8, choices = 1:max_wk),
              tabsetPanel(type = "tabs", 
                tabPanel("Expected Points",
                fluidRow(box(width = 10, height = "80%", plotlyOutput("explore_ep") %>% withSpinner(color="#0dc5c1")))),
                tabPanel("Win Probability",
                fluidRow(box(width = 10, height = "80%", plotlyOutput("explore_wp") %>% withSpinner(color="#0dc5c1")))),
                tabPanel("Passes vs Rushes - Density",
                         fluidRow(box(width = 10, height = "80%", plotlyOutput("explore_pass_and_rush") %>% withSpinner(color="#0dc5c1")))),
                tabPanel("Passes vs Rushes - Scatter",
                        fluidRow(box(width = 10, height = "80%", plotOutput("explore_pass_rush_scatter") %>% withSpinner(color="#0dc5c1"))))
              )),
      # Win Probability Tab #####
      tabItem(tabName = "winprob",
              fluidRow(
                column(2,selectInput("wp_season", h5("Choose Season"), choices = 2009:2018, selected = 2018)),
                column(3,selectInput("wp_week", h5("Choose Week"), choices = 1:17, selected = 16)),
                column(6, selectInput("wp_team", h5("Choose Team"), choices = teams))
                ),
              fluidRow(box(width = 10, height = "80%", plotlyOutput("win_prob") %>% 
                             withSpinner(color="#0dc5c1")
                           ))
              ),
      # Team Efficiency Tab #####
      tabItem(tabName = "team",
              selectInput("weeks_back", h5("Past Weeks to Analyze"), selected = 8, choices = 1:max_wk),
              tabsetPanel(type = "tabs", 
                  tabPanel("EPA",
                           h6(epa_detail, a("here.", href=epa_link)),
                    fluidRow(box(width = 10, plotlyOutput("team_epa")%>% 
                                   withSpinner(color="#0dc5c1")))),
                  tabPanel("WPA",
                            h6(wpa_detail, a("here.", href=wpa_link)),
                            fluidRow(box(width = 10, plotlyOutput("team_wpa")%>% 
                                          withSpinner(color="#0dc5c1")))),
                  tabPanel("PACR",
                            h6(pacr_detail, a("here.", href=pacr_link)),
                            fluidRow(box(width = 10, plotlyOutput("team_pacr")%>% 
                                   withSpinner(color="#0dc5c1")))),
                  tabPanel("aPACR",
                            h6(apacr_detail, a("here.", href=apacr_link)),
                            fluidRow(box(width = 10, plotlyOutput("team_apacr")%>% 
                                   withSpinner(color="#0dc5c1")))),
                  tabPanel("anYA",
                            h6(anya_detail, a("here.", href=anya_link)),
                            fluidRow(box(width = 10, plotlyOutput("team_anya")%>% 
                                   withSpinner(color="#0dc5c1"))))
                  )),
      # QB Tab #####
      tabItem(tabName = "qb",
              selectInput("qb_weeks_back", h5("Past Weeks to Analyze"), selected = 8, choices = 1:max_wk),
              tabsetPanel(type = "tabs", 
                 tabPanel("Efficiency", 
                    tabsetPanel(type = "tabs",
                      tabPanel("EPA",
                          h6(epa_detail, a("here.", href=epa_link)),
                          fluidRow(box(width = 10, plotlyOutput("qb_epa") %>% withSpinner(color="#0dc5c1")))),
                      tabPanel("WPA",
                          h6(wpa_detail, a("here.", href=wpa_link)),
                          fluidRow(box(width = 10, plotlyOutput("qb_wpa") %>% withSpinner(color="#0dc5c1")))),
                      tabPanel("PACR",
                          h6(pacr_detail, a("here.", href=pacr_link)),
                          fluidRow(box(width = 10, plotlyOutput("qb_pacr") %>% withSpinner(color="#0dc5c1")))),
                      tabPanel("aPACR",
                          h6(apacr_detail, a("here.", href=apacr_link)),
                          fluidRow(box(width = 10, plotlyOutput("qb_apacr") %>% withSpinner(color="#0dc5c1")))),
                      tabPanel("anYA",
                          h6(anya_detail, a("here.", href=anya_link)),
                          fluidRow(box(width = 10, height = "100%", plotlyOutput("qb_anya") %>% withSpinner(color="#0dc5c1"))))
                    )),
                 tabPanel("Total Yards",
                          fluidRow(box(width = 10, height = "100%", plotlyOutput("qb_total_yards")%>% 
                                         withSpinner(color="#0dc5c1")))),
                 # Individual QB Tab #####
                 tabPanel("Individual",
                          selectInput("qb_indiv", h5("Choose a QB"), selected = 8, choices = sort(unique(qb_df$Player))),
                          tabsetPanel(type = "tabs",
                                      tabPanel("EPA",
                                               fluidRow(box(width = 10, plotlyOutput("indiv_qb_epa")%>% 
                                                              withSpinner(color="#0dc5c1")))),
                                      tabPanel("WPA",
                                               fluidRow(box(width = 10, plotlyOutput("indiv_qb_wpa")%>% 
                                                              withSpinner(color="#0dc5c1")))),
                                      tabPanel("PACR",
                                               fluidRow(box(width = 10, plotlyOutput("indiv_qb_pacr")%>% 
                                                              withSpinner(color="#0dc5c1")))),
                                      tabPanel("aPACR",
                                               fluidRow(box(width = 10, plotlyOutput("indiv_qb_apacr")%>% 
                                                              withSpinner(color="#0dc5c1")))),
                                      tabPanel("anYA",
                                               fluidRow(box(width = 10, height = "100%", plotlyOutput("indiv_qb_anya")%>% 
                                                              withSpinner(color="#0dc5c1"))))
                                      )) 
                 )),
      # Skill Position Tab #####
      tabItem(tabName = "skill",
              selectInput("skill_weeks_back", h5("Past Weeks to Analyze"), selected = 8, choices = 1:max_wk),
              checkboxGroupInput("checkGroup", label = h3("Select Which Positions to Analyze"), choices = sort(unique(skill_df$Pos)), 
                                 inline = T, selected = c("RB","WR","TE")),
              tabsetPanel(type = "tabs",
                          tabPanel("Opportunity",
                                   tabsetPanel(type = "tabs",
                                      tabPanel("% of Team Opps",
                                        fluidRow(box(width = 10, plotlyOutput("msopp")%>% withSpinner(color="#0dc5c1")))),
                                      tabPanel("% of Team Rushes",
                                          fluidRow(box(width = 10, plotlyOutput("msrush")%>% withSpinner(color="#0dc5c1")))),
                                      tabPanel("% of Team Targets",
                                               fluidRow(box(width = 10, plotlyOutput("mstrg")%>% withSpinner(color="#0dc5c1")))),
                                      tabPanel("% of Team Air Yards",
                                               fluidRow(box(width = 10, plotlyOutput("msair")%>% withSpinner(color="#0dc5c1")))),
                                      tabPanel("WOPR",
                                               h6(wopr_detail, a("here.", href=wopr_link)),
                                               fluidRow(box(width = 10, plotlyOutput("wopr")%>% withSpinner(color="#0dc5c1")))),
                                      tabPanel("aWOPR",
                                               h6(awopr_detail, a("here.", href=awopr_link)),
                                               fluidRow(box(width = 10, plotlyOutput("awopr")%>% withSpinner(color="#0dc5c1")))))),
                          tabPanel("Efficiency",
                                   tabsetPanel(type = "tabs",
                                      tabPanel("EPA",
                                        h6(epa_detail, a("here.", href=epa_link)),
                                        fluidRow(box(width = 10, plotlyOutput("skill_epa")%>% 
                                                  withSpinner(color="#0dc5c1")))),
                                      tabPanel("WPA",
                                        h6(wpa_detail, a("here.", href=wpa_link)),
                                        fluidRow(box(width = 10, plotlyOutput("skill_wpa")%>% 
                                                  withSpinner(color="#0dc5c1")))),
                                      tabPanel("RACR",
                                        h6(racr_detail, a("here.", href=racr_link)),
                                        fluidRow(box(width = 10, plotlyOutput("skill_racr")%>% 
                                                  withSpinner(color="#0dc5c1")))),
                                      tabPanel("aRACR",
                                        h6(aracr_detail, a("here.", href=aracr_link)),
                                        fluidRow(box(width = 10, plotlyOutput("skill_aracr")%>% 
                                                  withSpinner(color="#0dc5c1")))),
                                      tabPanel("anYA",
                                        h6(anya_detail, a("here.", href=anya_link)),
                                        fluidRow(box(width = 10, height = "100%", plotlyOutput("skill_anya")%>% 
                                                  withSpinner(color="#0dc5c1"))))
                                   )),
                          # Individual Skill Position Tab #####
                          tabPanel("Individual",
                                   selectInput("skill_indiv", h5("Choose a Player"), selected = 8, choices = sort(unique(skill_df$Player))),
                                   tabsetPanel(type = "tabs",
                                               tabPanel("EPA",
                                                        fluidRow(box(width = 10, plotlyOutput("indiv_skill_epa")%>% 
                                                                       withSpinner(color="#0dc5c1")))),
                                               tabPanel("WPA",
                                                        fluidRow(box(width = 10, plotlyOutput("indiv_skill_wpa")%>% 
                                                                       withSpinner(color="#0dc5c1")))),
                                               tabPanel("RACR",
                                                        fluidRow(box(width = 10, plotlyOutput("indiv_skill_racr")%>% 
                                                                       withSpinner(color="#0dc5c1")))),
                                               tabPanel("aRACR",
                                                        fluidRow(box(width = 10, plotlyOutput("indiv_skill_aracr")%>% 
                                                                       withSpinner(color="#0dc5c1")))),
                                               tabPanel("anYA",
                                                        fluidRow(box(width = 10, height = "100%", plotlyOutput("indiv_skill_anya")%>% 
                                                                       withSpinner(color="#0dc5c1"))))
                                               )) 
              ))
    )
  )
)
)