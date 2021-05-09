install.packages("plyr")
install.packages("plotly")
library(tidyverse)
library(broom)
library(plyr)
library(plotly)
nba_player_salary <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
nba_player_stats <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
nba_team_stats1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
nba_team_stats2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")
nba_team_payroll <- read_csv("data/raw/2019-20_nba_team-payroll.csv")

#Tidying data
nba_player_stats <- dplyr::rename(nba_player_stats, FGp = "FG%", x3P = "3P", x3PA = "3PA", x3Pp = "3P%", x2P = "2P", x2PA = "2PA", x2Pp = "2P%", eFGp = "eFG%", FTp = "FT%")
#https://rforexcelusers.com/remove-currency-dollar-sign-r/
nba_team_payroll$salary <- as.numeric(gsub("[\\$,]", "", nba_team_payroll$salary))

nba_team_stats1 <- dplyr::rename(nba_team_stats1, 
                          x3PAr = '3PAr', TSp = 'TS%', eFGp = 'eFG%', TOVp = 'TOV%', ORBp = 'ORB%', DRBp = 'DRB%')
nba_team_stats2 <- dplyr::rename(nba_team_stats2,
                          FGp = 'FG%', x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', x2PA = '2PA', x2Pp = '2P%', FTp = 'FT%')

#Joining data
nba_team_stats2 <- nba_team_stats2 %>%
  full_join(x = nba_team_stats2, y = nba_team_stats1,by = "Team") %>%
  select(-Rk.y)

nba_team_stats2 <- dplyr::rename(nba_team_stats2, Rank = 'Rk.x')

nba_player_stats <- nba_player_salary %>%
  select(player_id, player_name, salary) %>%
  right_join(nba_player_stats, by = "player_name")

#Remove duplicates
nba_player_stats_draft <- transform(nba_player_stats, Age = as.character(Age), player_id = as.character(player_id), salary = as.character(salary))
nba_player_stats_draft <- plyr::ddply(nba_player_stats_draft, "player_name", numcolwise(sum)) #Sourced from https://stackoverflow.com/questions/10180132/consolidate-duplicate-rows
nba_player_stats <- select(nba_player_stats, player_id, player_name, salary, Pos, Age, Tm)
nba_player_stats <- nba_player_stats[!duplicated(nba_player_stats$player_name),]
nba_player_stats <- right_join(x = nba_player_stats_draft, y = nba_player_stats, 
                               by = "player_name")
nba_player_stats <- nba_player_stats[, c(27, 1, 28, 29, 30, 31, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)]
nba_player_stats <- arrange(nba_player_stats, player_id)
nba_player_stats <- transform(nba_player_stats, Age = as.numeric(Age), player_id = as.numeric(player_id), salary = as.numeric(salary))

#Treating Missing Values
sum(is.na(nba_player_stats))
sum(is.na(nba_team_stats1))
sum(is.na(nba_team_stats2))
which(is.na(nba_player_stats), arr.ind = TRUE)  
naniar::vis_miss(nba_player_stats)
naniar::vis_miss(nba_team_stats1)
nba_player_stats <- replace_na(nba_player_stats, list(FGp = 0, x3Pp = 0, x2Pp = 0, eFGp = 0, FTp = 0))
nba_team_stats2 <- select(nba_team_stats2, -X23, -X24, -X25)
nba_team_stats1 <- select(nba_team_stats1, -X23, -X24, -X25)
nba_player_stats <- drop_na(nba_player_stats)
sum(is.na(nba_player_stats))
sum(is.na(nba_team_stats1))
sum(is.na(nba_team_stats2))

#Summary statistics
summary(nba_team_stats2)
summary(nba_player_stats)

nba_team_stats2 %>%
  dplyr::summarise(mnFGp = mean(FGp),
          mnx3Pp = mean(x3Pp),
          mnx2Pp = mean(x2Pp),
          mneFGp = mean(eFGp),
          mnFTp = mean(FTp),
          sdFGp = sd(FGp),
          sdx3Pp = sd(x3Pp),
          sdx2Pp = sd(x2Pp),
          sdeFGp = sd(eFGp),
          sdFTp = sd(FTp))

nba_player_group <- nba_player_stats %>%
  dplyr::group_by(Pos) %>%
  dplyr::summarise(mnFGp = mean(FGp),
         mnx3Pp = mean(x3Pp),
         mnx2Pp = mean(x2Pp),
         mneFGp = mean(eFGp),
         mnFTp = mean(FTp),
         sdFGp = sd(FGp),
         sdx3Pp = sd(x3Pp),
         sdx2Pp = sd(x2Pp),
         sdeFGp = sd(eFGp),
         sdFTp = sd(FTp))

#Filter by position
center <- nba_player_stats %>%
  filter(Pos == "C") %>%
  mutate(comparison_FGp = if_else(FGp > mean(FGp), "better", "worse"), comparison_x3Pp = if_else(x3Pp > mean(x3Pp), "better", "worse"), comparison_x2Pp = if_else(x2Pp > mean(x2Pp), "better", "worse"), comparison_eFGp = if_else(eFGp > mean(eFGp), "better", "worse"), comparison_FTp = if_else(FTp > mean(FTp), "better", "worse")) 
point_guard <- nba_player_stats %>%
  filter(Pos == "PG") %>%
  mutate(comparison_FGp = if_else(FGp > mean(FGp), "better", "worse"), comparison_x3Pp = if_else(x3Pp > mean(x3Pp), "better", "worse"), comparison_x2Pp = if_else(x2Pp > mean(x2Pp), "better", "worse"), comparison_eFGp = if_else(eFGp > mean(eFGp), "better", "worse"), comparison_FTp = if_else(FTp > mean(FTp), "better", "worse"))
shooting_guard <- nba_player_stats %>%
  filter(Pos == "SG") %>%
  mutate(comparison_FGp = if_else(FGp > mean(FGp), "better", "worse"), comparison_x3Pp = if_else(x3Pp > mean(x3Pp), "better", "worse"), comparison_x2Pp = if_else(x2Pp > mean(x2Pp), "better", "worse"), comparison_eFGp = if_else(eFGp > mean(eFGp), "better", "worse"), comparison_FTp = if_else(FTp > mean(FTp), "better", "worse"))
small_forward <- nba_player_stats %>%
  filter(Pos == "SF") %>%
  mutate(comparison_FGp = if_else(FGp > mean(FGp), "better", "worse"), comparison_x3Pp = if_else(x3Pp > mean(x3Pp), "better", "worse"), comparison_x2Pp = if_else(x2Pp > mean(x2Pp), "better", "worse"), comparison_eFGp = if_else(eFGp > mean(eFGp), "better", "worse"), comparison_FTp = if_else(FTp > mean(FTp), "better", "worse"))
power_forward <- nba_player_stats %>%
  filter(Pos == "PF") %>%
  mutate(comparison_FGp = if_else(FGp > mean(FGp), "better", "worse"), comparison_x3Pp = if_else(x3Pp > mean(x3Pp), "better", "worse"), comparison_x2Pp = if_else(x2Pp > mean(x2Pp), "better", "worse"), comparison_eFGp = if_else(eFGp > mean(eFGp), "better", "worse"), comparison_FTp = if_else(FTp > mean(FTp), "better", "worse"))

#Normalising
nba_team_stats2 <- nba_team_stats2 %>%
  mutate(FG_per_game = FG / G,
         x3P_per_game = x3P / G,
         x2P_per_game = x2P / G,
         FT_per_game = FT / G,
         PTS_per_game = PTS / G,
         AST_per_game = AST / G,
         STL_per_game = STL / G,
         BLK_per_game = BLK / G,
         ORB_per_game = ORB / G,
         DRB_per_game = DRB / G,
         TOV_per_game = TOV / G,
         PF_per_game = PF / G,)

#Graphs
ggplot(nba_team_stats2, aes(x = x3P_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = x2P_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = FT_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = AST_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = STL_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = BLK_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = ORB_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = DRB_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = TOV_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
ggplot(nba_team_stats2, aes(x = PF_per_game, y = PTS_per_game)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")
fit <- lm(PTS_per_game ~ ORB_per_game + DRB_per_game + AST_per_game + STL_per_game + BLK_per_game + TOV_per_game + PF_per_game, data = nba_team_stats2)
tidy(fit, conf.int = TRUE)
#assumptions
car::avPlots(fit)
sqrt(car::vif(fit))

std_res <- rstandard(fit)
points <- 1:length(std_res)

ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

hats <- hatvalues(fit)
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

cook <- cooks.distance(fit)
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

cook_labels <- if_else(cook >= 0.015, paste(points), "")
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.001)

car::durbinWatsonTest(fit)

res <- residuals(fit)
fitted <- predict(fit)

ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = res)) +
  stat_qq() + stat_qq_line()

nba_team_stats2 <- mutate(nba_team_stats2, exp_PTS_per_game = predict(fit, newdata = nba_team_stats2))

ggplot(nba_team_stats2, aes(exp_PTS_per_game, PTS_per_game, label = Team)) +
  geom_point(colour = "dodgerblue") +
  geom_text(nudge_x = 0.1, cex = 3) +
  geom_abline(linetype = "dashed", colour = "magenta")

ggplot(nba_team_stats2, aes(x = W, y = exp_PTS_per_game, label = Team)) +
  geom_point(colour = "dodgerblue") +
  geom_text(nudge_x = 0.1, cex = 3)

#average number of minutes played by team
team_mins_per_game <- nba_team_stats2 %>%
  dplyr::group_by(Team) %>%
  dplyr::summarise(team_mins_per_game = MP/max(G)) %>%
  pull(team_mins_per_game) %>%
  mean
team_mins_per_game

#player minutes played
nba_player_stats <- nba_player_stats %>%
  mutate(G1 = MP / team_mins_per_game,
         FG_per_game = FG / G1,
                   x3P_per_game = x3P / G1,
                   x2P_per_game = x2P / G1,
                   FT_per_game = FT / G1,
                   PTS_per_game = PTS / G1,
                   AST_per_game = AST / G1,
                   STL_per_game = STL / G1,
                   BLK_per_game = BLK / G1,
                   ORB_per_game = ORB / G1,
                   DRB_per_game = DRB / G1,
                   TOV_per_game = TOV / G1,
                   PF_per_game = PF / G,1) %>%
  select(-G1)
nba_player_stats <- mutate(nba_player_stats, exp_PTS = predict(fit, newdata = nba_player_stats))

nba_player_stats %>%
  ggplot(aes(x = exp_PTS)) +
  geom_histogram(binwidth = 5, colour = "black", fill = "dodgerblue")

gg<- nba_player_stats %>%
  ggplot(aes(x = salary/1000000, y = exp_PTS, color = Pos)) +
  geom_point() + 
  xlab("Salary (Millions)")

gg <- nba_player_stats %>%
  ggplot(aes(x = salary/1000000, y = exp_PTS, color = Pos)) +
  geom_point() + 
  xlab("Salary (Millions)") + 
  ylab("Expected Points Scored") +
  ylim(90, 260) +
  xlim(0.1, 20)
gg <- gg +
  labs(title = "Points distribution based on salary by position", subtitle = "Players with higher salary tend to score more points", colour = "Position") +
  geom_text(aes(label = player_id), nudge_y = 5)
gg
ggplotly(gg)

write_csv(x = nba_player_stats, path = "data/processed/processed_player_stats.csv")
write_csv(x = nba_team_stats2, path = "data/processed/processed_team_stats.csv")