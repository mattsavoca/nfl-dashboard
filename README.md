# nfl-dashboard
Visualizing Top NFL Performers and teams by a user-defined amount of Weeks in the past.

This shiny app visualizes the NFL's regular-season play-level data from the 2009 thru 2018 seasons, accessed from the NFL's using the (related) nflscrapR package. Understanding that the recent past provides much more value in predicting future NFL performance, the app includes myriad opportunities for the user to filter the data to only include results from the past few games, or if they desire, the past ten years.  The shiny app focuses on aggregating the play-level details into game-level, team-level, Quarterback-level, and skill position-level visualizations, which can be beneficial in Football proxy games, such as Fantasy Football and Player Proposition ("Prop") bets.

https://mattscottsavoca.shinyapps.io/nfl_dashboard/

Data is from: https://github.com/ryurko/nflscrapR-data

The play-by-play data includes 250+ unique variables per play, including:
	**Game state:** Offensive Team, Defensive Team, Team Scores, Yard Line, Down, Yards to Go, Time in remaining in quarter, half, and game.
	**Pre-pass Variables:** Play-Action (Fake-Run), QB Hit, QB Sack.
	**Play Result and Scoring Details:** Play Type, Penalty, Run Direction, Air Yards, Yards After Catch (YAC), Touchdown, Field Goal, Interception, Fumble.
	**Offensive names and NFL Global Stats and Info Services (GSIS) Player IDs:** each quarterback, ball-carrier, or intended receiver.
	**Defensive name sand NFL Global Stats and Info Services (GSIS) Player IDs:** each player involved in a sack, tackle, fumble, lateral or interception.

The data includes *Air Yards* (distance the ball travelled in the air before a receiver or defender attempted a play on the ball) information, `air_yards`, which serves as the basis for many popular player efficiency metrics, such as *Passer Air Conversion Ratio (PACR)* and *Weighted Opportunity Rating (WOPR),* used throughout the app. Links to additional details on these metrics are available within the app. 

Finally, each play includes nflscrapR's *Expected Points,* `ep`, model, along with *Expected Points Added,* `epa`, (the individual play's change in Expected Points,) as well as team-level Win Probability, `wp`, and *Win Probability Added,* `wpa` (the individual play's change in the team's Win Probability). These metrics are utilized pervasively within the app.

Each game's final score and team details were available in the repository's legacy files, and were added to the play-level details for this app. The legacy files also included team rosters, however the data only includes Quarterbacks (QB) and four additional positions: Running Backs (RB), Fullbacks (FB), Wide Receivers (WR), and Tight Ends (TE), collectively referred to as the skill-positions. This "limitation" was an influence on the project's scope.

The ggplot and plotly packages were used to visualize the play-level efficiency and opportunity for NFL players, grouped by team, play type, and position, using a variety of state of the art effeciency and opportunity metrics.

Additional. Notes: 
Only the 2016-2018 seasons include a unique  variable for whether the play resulted in a touchback. The `touchback` column for all plays 2009-2015 are NAs.
The data also includes pre-season and post-season (playoffs) data. It was excluded from this project.
