library(worldfootballR)
library(dplyr)
library(caret)
library(xgboost)
library(vip)
library(stringr)
library(soccerplotR)
library(rvest)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggpath)

stats <- data.frame() 

for (szn in 2018:2024) {
  if (szn != 2020) {
    stats_standard <- fb_big5_advanced_season_stats(season_end_year=szn,stat_type="standard",team_or_player="player") %>%
      select(Season = Season_End_Year, Squad, Player, Nation, Url, Mins_Per_90_Playing, xAG_Expected, Pos)
    stats_pos <- fb_big5_advanced_season_stats(season_end_year=szn,stat_type="possession",team_or_player="player") %>%
      select(Touches_Touches, Carries_Carries, PrgDist_Carries, PrgR_Receiving)
    stats_pass <- fb_big5_advanced_season_stats(season_end_year=szn,stat_type="passing",team_or_player="player") %>%
      select(PrgDist_Total, PrgP)
    stats_def <- fb_big5_advanced_season_stats(season_end_year=szn,stat_type="defense",team_or_player="player") %>%
      select(TklW_Tackles, Int, Clr)
    stats_gca <- fb_big5_advanced_season_stats(season_end_year=szn,stat_type="gca",team_or_player="player") %>%
      select(GCA_GCA)
    stats_each <- cbind(stats_standard, stats_pos, stats_pass, stats_def, stats_gca)
    stats <- rbind(stats, stats_each)
  }
}

stats <- stats %>% group_by(Season, Squad) %>% filter(Mins_Per_90_Playing >= 0.75 * max(Mins_Per_90_Playing))

stats <- stats %>% mutate(Pos = sapply(strsplit(as.character(Pos), ','), function(x) x[1]))

stats <- stats %>% filter(Pos != "GK")

stats$Pos <- as.factor(stats$Pos)

factor_data <- stats %>%
  ungroup() %>%
  select(Pos)

dummy <- dummyVars(" ~ .", data = factor_data)
pos_data <- data.frame(predict(dummy, newdata = factor_data))

stats <- cbind(stats, pos_data) %>%
  select(-Pos)

stats <- stats %>% filter(!is.na(GCA_GCA))

xgboost_train <- stats %>%
  filter(Season != 2024)

xgboost_test <- stats %>%
  filter(Season == 2024)

labels_train <- as.matrix(xgboost_train[,17])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(8:16, 18:20)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(8:16, 18:20)])

gcaoe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(gcaoe_model)
vi(gcaoe_model)

gca_predict <- predict(gcaoe_model, xgboost_testfinal)
gca_actual <- as.matrix(xgboost_test[,17])
postResample(gca_predict, gca_actual)

gca_predictions <- as.data.frame(
  matrix(predict(gcaoe_model, as.matrix(stats[,c(8:16, 18:20)])))
)

all_stats <- cbind(stats, gca_predictions)

all_stats <- all_stats %>%
  mutate(gca = GCA_GCA, xgca = V1, gcaoe = gca - xgca)

stats_2024 <- all_stats %>% filter(Season == 2024)

stats_2024$id <- str_extract(stats_2024$Url, "(?<=players/)\\w+")

stats_2024$headshot_url <- paste0("https://fbref.com/req/202302030/images/headshots/", stats_2024$id, "_2022.jpg")

teams_url <- 'https://fbref.com/en/comps/Big5/Big-5-European-Leagues-Stats'
teams_page <- read_html(teams_url)

team_elements <- teams_page %>%
  html_elements('table') %>%
  html_elements('td[data-stat="team"]')

logo_urls <- team_elements |> 
  html_elements('img') |> 
  html_attr('src')

team_names <- team_elements |> 
  html_elements('a') |> 
  html_text2()

teams <- data.frame(Squad = team_names, Logo = logo_urls)

teams$Logo <- sub("mini\\.", "", teams$Logo)

stats_2024 <- left_join(stats_2024, teams, by = "Squad")

gt_nice_stats <- stats_2024 %>%
  arrange(-xAG_Expected) %>%
  ungroup() %>%
  head(30) %>%
  arrange(-gcaoe) %>%
  mutate(gcaoe = round(gcaoe, 2)) %>%
  select(headshot_url, Player, Logo, xAG_Expected, gcaoe) 

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>worldfootballR (FBRef)</b>", "Amrit Vignesh")

nice_table <- gt_nice_stats %>% gt() %>%
  gt_img_rows(columns = Logo, height = 40) %>%
  gt_img_rows(columns = headshot_url, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_url, Player, Logo, xAG_Expected, gcaoe)
  ) %>%
  gt_hulk_col_numeric(c(xAG_Expected, gcaoe)) %>%
  cols_label(
    headshot_url = md(""),
    Player = md("**Player**"),
    Logo = md("**Club**"),
    xAG_Expected = md("**xAG**"),
    gcaoe = md("**GCAOE**")
  ) %>%
  tab_header(
    title = "2023/24 Big 5 Leagues GCAOE (Goal-Creating Actions Over Expected)",
    subtitle = md("*Players With ≥ **75%** of Maximum Time Played by Team, **Top 30** in xAG Displayed*")
  ) %>% 
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Player, gcaoe)
    )
  ) %>%
  cols_width(Player ~ px(220), headshot_url ~ px(110))

gtsave(nice_table, "nice_table.png", vwidth = 1000, vheight = 2500, zoom = 1)

plot <- gt_nice_stats %>%
  ggplot(aes(x = gcaoe, y = xAG_Expected)) +
  geom_hline(yintercept = mean(gt_nice_stats$xAG_Expected), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(gt_nice_stats$gcaoe), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  geom_from_path(aes(x = gcaoe, y = xAG_Expected, path = headshot_url), width = 0.1, height = 0.1) +
  labs(x = "GCAOE",
       y = "xAG",
       title = "Comparing Metrics Describing Goal Creation",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) 

ggsave("plot.png", plot, width = 12)
