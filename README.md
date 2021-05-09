# reproducible-data-analysis-project_u3232147
Reproducible data analysis project 
This repository contains analysis of NBA player data in the 2018-19 season. The repository contains the following items: 
1. Raw Data - five files of raw NBA player and team statistics. Variable descriptions provided below
2. Processed Data - two files of team data and player data following data tidying and exploration activities
3. Figs - figures used as part of data exploration
4. Docs - R Markdown final output as well as raw markdown file
5. Rproject file - R script containing code for data analysis

Variable descriptions for each of the five source files:

2018-19_nba_player-statistics.csv  
This data file provides total statistics for individual NBA players during the 2018-19 season. The variables consist:
player_name : Player Name
Pos :  (PG = point guard, SG = shooting guard, SF = small forward, PF = power forward, C = center) 
Age : Age of Player at the start of February 1st of that season.
Tm : Team
G : Games
GS : Games Started
MP : Minutes Played
FG : Field Goals
FGA : Field Goal Attempts
FG% : Field Goal Percentage
3P : 3-Point Field Goals
3PA : 3-Point Field Goal Attempts
3P% : FG% on 3-Pt FGAs
2P : 2-Point Field Goals
2PA : 2-point Field Goal Attempts
2P% : FG% on 2-Pt FGAs
eFG% : Effective Field Goal Percentage
FT : Free Throws
FTA : Free Throw Attempts
FT% : Free Throw Percentage
ORB : Offensive Rebounds
DRB : Defensive Rebounds
TRB : Total Rebounds
AST : Assists
STL : Steals
BLK : Blocks
TOV : Turnovers
PF : Personal Fouls
PTS : Points

2018-19_nba_player-salaries.csv 
This data file contains the salary for individual players during the 2018-19 NBA season. The variables consist:
player_id : unique player identification number
player_name : player name
salary : year salary in $USD

2018-19_nba_team-payroll.csv 
This data file contains the team payroll budget for the 2019-20 NBA season. The variables consist:
team_id : unique team identification number
team : team name
salary : team payroll budget in 2019-20 in $USD

 2018-19_nba_team-statistics_1.csv 
This data file contains miscellaneous team statistics for the 2018-19 season. The variables consist:
Rk : Rank
Age : Mean Age of Player at the start of February 1st of that season.
W : Wins
L : Losses
PW : Pythagorean wins, i.e., expected wins based on points scored and allowed
PL : Pythagorean losses, i.e., expected losses based on points scored and allowed
MOV : Margin of Victory
SOS : Strength of Schedule; a rating of strength of schedule. The rating is denominated in points above/below average, where zero is average.
SRS : Simple Rating System; a team rating that takes into account average point differential and strength of schedule. The rating is denominated in points above/below average, where zero is average.
ORtg : Offensive Rating; An estimate of points produced (players) or scored (teams) per 100 possessions
DRtg : Defensive Rating; An estimate of points allowed per 100 possessions
NRtg : Net Rating; an estimate of point differential per 100 possessions.
Pace : Pace Factor: An estimate of possessions per 48 minutes
FTr : Free Throw Attempt Rate; Number of FT Attempts Per FG Attempt
3PAr : 3-Point Attempt Rate; Percentage of FG Attempts from 3-Point Range
TS% : True Shooting Percentage; A measure of shooting efficiency that takes into account 2-point field goals, 3-point field goals, and free throws.
eFG% : Effective Field Goal Percentage; This statistic adjusts for the fact that a 3-point field goal is worth one more point than a 2-point field goal.
TOV% : Turnover Percentage; An estimate of turnovers committed per 100 plays.
ORB% : Offensive Rebound Percentage; An estimate of the percentage of available offensive rebounds a player grabbed while he was on the floor.
FT/FGA : Free Throws Per Field Goal Attempt
DRB% : Defensive Rebound Percentage

2018-19_nba_team-statistics_2.csv 
This data file contains total team statistics for the 2018-19 NBA season. The variables consist:
Rk : Ranking
Team : Team name
G : Games
MP : Minutes Played
FG : Field Goals
FGA : Field Goal Attempts
FG% : Field Goal Percentage
3P : 3-Point Field Goals
3PA : 3-Point Field Goal Attempts
3P% : FG% on 3-Pt FGAs
2P : 2-Point Field Goals
2PA : 2-point Field Goal Attempts
2P% : FG% on 2-Pt FGAs
FT : Free Throws
FTA : Free Throw Attempts
FT% : Free Throw Percentage
ORB : Offensive Rebounds
DRB : Defensive Rebounds
TRB : Total Rebounds
AST : Assists
STL : Steals
BLK : Blocks
TOV : Turnovers
PF : Personal Fouls
PTS : Points
