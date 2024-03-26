library(tidyverse)
library(cbbdata)
library(hoopR)
library(httr)
library(rvest)
library(randomForest)
library(caret)
library(vip)
library(gt)
library(ROCR)
set.seed(1234)
# Create a year vector
yrs <- c(2019, 2021:2024)
target_yr <- 2024 # Would need to update this code annually based on the target yr
# Get CBBData
cbd_login(username = "jmarkman", password = "224488Jarrett!x")
# Get team data
cbd_teams <- cbd_teams() %>%
  summarise(common_team, color, alt_color, espn_logo, espn_dark_logo, logo, color, alt_color)
# Get preseason ap rankings
get_preseason <- function(yr) {
  print(paste("Getting data from: ", yr))
  url <- str_c(paste0("https://www.sports-reference.com/cbb/seasons/men/", yr, "-polls.html"))
  response <- GET(url)
  html_content <- read_html(response)
  df <- html_content %>% html_table(header = TRUE)
  if (yr %in% c(2019, 2021:2023)) {
    df <- df[[1]]
    df <- df[1:nrow(df), c(1, 3)]
    colnames(df) <- c("team", "preseason_ap_ranking")
    df <- df %>% mutate(year = yr) 
    df <- df %>% mutate(team = as.character(team), preseason_ap_ranking = as.numeric(preseason_ap_ranking)) %>%
      filter(!is.na(preseason_ap_ranking) & team != "") %>%
      arrange(preseason_ap_ranking) %>%
      as.data.frame() 
  } else if (yr == 2024) {
    df <- df[[2]]
    df <- df[1:nrow(df), c(1, 3)]
    colnames(df) <- c("team", "preseason_ap_ranking")
    df <- df %>% mutate(year = yr) 
    df <- df %>% mutate(team = as.character(team), preseason_ap_ranking = as.numeric(preseason_ap_ranking)) %>%
      filter(!is.na(preseason_ap_ranking) & team != "") %>%
      arrange(preseason_ap_ranking) %>%
      as.data.frame() 
  }
  return(df)
}
preseason_rankings <- map(yrs, get_preseason)
rankings <- do.call(rbind, preseason_rankings) %>%
  mutate(team = gsub("State", "St.", team),
         team = case_when(
           team == "UNC" ~ "North Carolina",
           team == "UConn" ~ "Connecticut",
           team == "Miami (FL)" ~ "Miami FL",
           TRUE ~ team
         ))
# Get tournament resume data
get_resumes <- function(yr) {
  print(paste("Getting data from:", yr))
  resumes <- cbd_torvik_resume_database(min_year = yr, max_year = yr) %>%
      mutate(year = as.numeric(year)) %>%
      arrange(year, team) %>%
      select(team, year, seed, type, rd) %>%
      filter(team != "Team")
  return(resumes)
}
resumes <- map(yrs, get_resumes)
resumes <- do.call(rbind, resumes)
# Get quad data
get_quad_data <- function(yr) {
  print(paste("Getting data from:", yr))
  url <- str_c(paste0("https://barttorvik.com/teamsheets.php?year=", yr))
  response <- GET(url)
  html_content <- read_html(response)
  data <- html_content %>% html_table(header = FALSE) %>% as.data.frame()
  cols <- data[2, ]
  colnames(data) <- cols
  df <- data[-1, ]
  colnames(df) <- make.unique(colnames(df))
  df <- df %>%
    filter(Team != "Team") %>%
    mutate(Team = trimws(gsub("\\bN4O\\b|\\bF4O\\b|[0-9]", "", Team)),
           year = yr) %>%
    select(team = Team, year, net = NET, quad_1a = Q1A, quad_1 = Q1, quad_2 = Q2, quad_3 = Q3, quad_4 = Q4)
  return(df)
}
quad_data <- map(yrs, get_quad_data)
quad_data <- do.call(rbind, quad_data)
# Get strength of schedule data
get_sos <- function(yr, num) {
  print(paste("Getting data from", yr, "for league", num))
  url <- str_c(paste0("https://www.espn.com/mens-college-basketball/bpi/_/view/resume/season/", yr, "/group/", num))
  response <- GET(url)
  html_content <- read_html(response)
  listed_df <- html_content %>% html_table(header = TRUE)
  names <- listed_df[[1]]
  data <- listed_df[[2]]
  df <- cbind(names, data) %>% as.data.frame() %>%
    mutate(year = yr) %>%
    select(team = Team, year, sor_rank = `SOR RK`, sos_rank = `SOS RK`, nc_sos_rank = `NC SOS`)
  return(df)
}
leagues <- c(1:14, 16, 18:27, 29:30, 44:46, 49, 62)
# Create combinations of yrs and leagues
combinations <- expand.grid(yr = yrs, num = leagues) %>%
  mutate(elim = paste0(yr, num)) %>%
  filter(elim != 202112) %>%
  select(-elim)
# Apply the function to each combination
sos_data <- pmap(combinations, get_sos)
sos <- do.call(rbind, sos_data)
# Get all team names to be able to join sos data by name (w/o nickname)
get_teams <- function(yr) {
  print(paste("Getting data from:", yr))
  teams <- espn_mbb_teams(yr) %>%
    mutate(year = yr) %>%
    select(display_name, other_name = team, year, conf = conference_short_name)
}
teams <- map(yrs, get_teams)
teams <- do.call(rbind, teams)
sos <- sos %>%
  left_join(teams, by = c("team" = "display_name", "year")) %>%
  rename(full_name = team, team = other_name) %>%
  mutate(nc_sos_rank = as.integer(ifelse(nc_sos_rank == "--", NA, nc_sos_rank)))
sos$team <- gsub("State", "St.", sos$team)
sos$team <- ifelse(sos$team == "UConn", "Connecticut", sos$team)
# Get team results
season_stats <- cbd_torvik_game_stats()
szn <- season_stats %>%
  filter(year %in% yrs & type != "post") %>%
  select(result, team, year, type, location) %>%
  mutate(win = ifelse(result == "W", 1, 0),
         location = case_when(
           location == "A" ~ "road",
           location == "H" ~ "home",
           location == "N" ~ "neutral"
         )
         )
overall <- szn %>%
  group_by(team, year) %>%
  summarise(
    wins = sum(win), gp = n(), losses = gp - wins, win_pct = wins/gp
  ) %>%
  ungroup()
splits <- szn %>%
  group_by(team, year, location) %>%
  summarise(
    wins = sum(win), gp = n(), win_pct = wins/gp
  ) %>% 
  ungroup() %>%
  pivot_wider(names_from = c(location),
              names_sep = "_",
              values_from = c(wins, gp, win_pct))
# Join all the data together
data <- resumes %>%
  left_join(cbd_teams, by = c("team" = "common_team")) %>%
  left_join(rankings, by = c("team", "year")) %>%
  left_join(overall, by = c("team", "year")) %>%
  left_join(splits, by = c("team", "year")) %>%
  left_join(quad_data, by = c("team", "year")) %>%
  left_join(sos, by = c("team", "year"))
# Create separate tournament data frames
# Data frame for predicting tournament appearance
tourney_prob_df <- data %>%
  filter(type != "Auto" & year != target_yr) %>%
  mutate(at_large = ifelse(type == "At-Large", 1, 0)) %>%
  # Pick columns for modeling
  summarise(at_large, team, conf, year, net, preseason_ap_ranking,
            wins, wins_neutral, wins_road, win_pct, win_pct_neutral, win_pct_road, 
            quad_1a, quad_1, quad_2, quad_3, quad_4, sor_rank, sos_rank, nc_sos_rank,
            color, alt_color, espn_logo, logo) %>%
  separate(quad_1a, c("quad_1a_wins", "quad_1a_losses"), "-") %>%
  separate(quad_1, c("quad_1_wins", "quad_1_losses"), "-") %>%
  separate(quad_2, c("quad_2_wins", "quad_2_losses"), "-") %>%
  separate(quad_3, c("quad_3_wins", "quad_3_losses"), "-") %>%
  separate(quad_4, c("quad_4_wins", "quad_4_losses"), "-") %>%
  mutate(
    net = as.numeric(net), p6 = ifelse(conf %in% c("ACC", "Big Ten", "Big 12", "Big East", "Pac-12", "SEC"), 1, 0),
    quad_1a_wins = as.numeric(quad_1a_wins), quad_1a_losses = as.numeric(quad_1a_losses),
    quad_1_wins = as.numeric(quad_1_wins), quad_1_losses = as.numeric(quad_1_losses),
    quad_2_wins = as.numeric(quad_2_wins), quad_2_losses = as.numeric(quad_2_losses),
    quad_3_wins = as.numeric(quad_3_wins), quad_3_losses = as.numeric(quad_3_losses),
    quad_4_wins = as.numeric(quad_4_wins), quad_4_losses = as.numeric(quad_4_losses),
    quad_1b_wins = quad_1_wins - quad_1a_wins,
    quad_1b_losses = quad_1_losses - quad_1a_losses
  ) %>%
  mutate(preseason_ranked = ifelse(is.na(preseason_ap_ranking), 0, 1))
prediction_tourney_df <- data %>%
  filter(year == target_yr) %>% # 2024 is this years desired predictions
  # Pick columns for modeling
  summarise(team, conf, year, net, preseason_ap_ranking,
            wins, wins_neutral, wins_road, win_pct, win_pct_neutral, win_pct_road,
            quad_1a, quad_1, quad_2, quad_3, quad_4, sor_rank, sos_rank, nc_sos_rank,
            color, alt_color, espn_logo, logo) %>%
  separate(quad_1a, c("quad_1a_wins", "quad_1a_losses"), "-") %>%
  separate(quad_1, c("quad_1_wins", "quad_1_losses"), "-") %>%
  separate(quad_2, c("quad_2_wins", "quad_2_losses"), "-") %>%
  separate(quad_3, c("quad_3_wins", "quad_3_losses"), "-") %>%
  separate(quad_4, c("quad_4_wins", "quad_4_losses"), "-") %>%
  mutate(
    net = as.numeric(net), p6 = ifelse(conf %in% c("ACC", "Big Ten", "Big 12", "Big East", "Pac-12", "SEC"), 1, 0),
    quad_1a_wins = as.numeric(quad_1a_wins), quad_1a_losses = as.numeric(quad_1a_losses),
    quad_1_wins = as.numeric(quad_1_wins), quad_1_losses = as.numeric(quad_1_losses),
    quad_2_wins = as.numeric(quad_2_wins), quad_2_losses = as.numeric(quad_2_losses),
    quad_3_wins = as.numeric(quad_3_wins), quad_3_losses = as.numeric(quad_3_losses),
    quad_4_wins = as.numeric(quad_4_wins), quad_4_losses = as.numeric(quad_4_losses),
    quad_1b_wins = quad_1_wins - quad_1a_wins,
    quad_1b_losses = quad_1_losses - quad_1a_losses
  ) %>%
  mutate(preseason_ranked = ifelse(is.na(preseason_ap_ranking), 0, 1))
# Variable significance
df <- tourney_prob_df
corr_df <- df %>%  
  # Remove character variables
  select(-c(team, conf, color, alt_color, espn_logo, logo, preseason_ap_ranking)) %>%
  na.omit() # Remove NAs
corr_vals <- cor(corr_df)[1, ]
cor_names <- c("At Large Bid", "Year", "NET", "Wins", "Neutral Wins", "Road Wins",
               "Win Percentage", "Neutral Win Percentage", "Road Win Percentage",
               "Quad 1a Wins", "Quad 1a Losses", "Quad 1 Wins", "Quad 1 Losses",
               "Quad 2 Wins", "Quad 2 Losses", "Quad 3 Wins", "Quad 3 Losses", 
               "Quad 4 Wins", "Quad 4 Losses", 
               "Strength of Record Rank", "Strength of Schedule Rank", "Non-Conference Strength of Schedule Rank",
               "P6 Team", "Quad 1b Wins", "Quad 1b Losses",
               "Preseason Ranked")
correlation_df <- data.frame(variable = cor_names,
                             `Correlation Coefficient` = corr_vals) %>%
  rename("Correlation Coefficient" = Correlation.Coefficient)
correlation_df <- correlation_df[-1, ]
# Plot with color gradient bars
corr_plot <- ggplot(correlation_df, aes(x = reorder(variable, `Correlation Coefficient`), y = `Correlation Coefficient`, fill = `Correlation Coefficient`)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "cividis") +
  labs(subtitle = "Variables most correlated with getting an at-large bid",
       title = "What Gets you an At-Large Bid?",
       x = "Variables",
       y = "Correlation Coefficient",
       caption = "Jarrett Markman | Data: cbbdata, hoopR, ESPN, torvik") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5))
print(corr_plot)
ggsave("Selection Committee Tendencies.png", corr_plot)
# Model for making tourney
model_df <- df %>%
  select(-c(conf, preseason_ap_ranking)) %>%
  na.omit()
model_df$at_large <- as.factor(model_df$at_large)
get_accuracy <- function(part, folds) {
  print(paste("Partition:", part, "Folds:", folds))
  ind <- sample(1:nrow(model_df), part * nrow(model_df))
  train_data <- model_df[ind, ] %>%
    select(-c(team, color, alt_color, espn_logo, logo))
  ctrl <- trainControl(method = "cv", number = folds)
  rf_model <- train(at_large ~ ., data = train_data, method = "rf", trControl = ctrl)
  accuracy <- rf_model$results %>%
    select(Accuracy) %>%
    slice(1) %>% unlist()
  return(accuracy)
}
parts <- seq(0.6, 0.8, by = 0.01)
folds <- 5:10
part_folds_combinations <- expand.grid(partition = parts, folds = folds)
results <- part_folds_combinations %>%
  mutate(accuracy = pmap_dbl(list(partition, folds), get_accuracy))
vals <- results %>%
  arrange(desc(accuracy)) %>%
  head(1)
partition <- unlist(vals$partition)
optimal_folds <- unlist(vals$folds)
ind <- sample(1:nrow(model_df), partition * nrow(model_df))
train_data <- model_df[ind, ] %>%
  select(-c(team, color, alt_color, espn_logo, logo))
validation_df <- model_df[-ind, ]
test_data <- validation_df %>%
  select(-c(team, color, alt_color, espn_logo, logo))
# Set up cross-validation
ctrl <- trainControl(method = "cv", number = optimal_folds)
# Train the random forest model using cross-validation
rf_model <- train(at_large ~ ., data = train_data, method = "rf", trControl = ctrl)
# Make predictions on the test data
predictions <- predict(rf_model, newdata = test_data)
# 1. Confusion Matrix
conf_matrix <- confusionMatrix(predictions, test_data$at_large)
mod_preds_matrix <- conf_matrix$table
# Convert matrix to data frame
no_tournament_both <- mod_preds_matrix[1]
predicted_make_but_no <- mod_preds_matrix[2]
predicted_no_make_but_make <- mod_preds_matrix[3]
tournament_both <- mod_preds_matrix[4]
conf_data <- data.frame(c = c("Predicted to Miss Tournament", "Predicted to Make Tournament"), a = c(no_tournament_both, predicted_make_but_no), b = c(predicted_no_make_but_make, tournament_both))
colnames(conf_data) <- c(" ", "Missed Tournament", "Made Tournament")
acc <- round((conf_data[1, 2] + conf_data[2, 3])/(conf_data[1, 2] + conf_data[2, 3] + conf_data[1, 3] + conf_data[2, 2]), digits = 4) * 100
conf_data %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_header(title = "Model Predictive Accuracy", subtitle = paste0("Confusion Matrix: ", acc, "% Accuracy"))
# 2 Bid evaluation
viz_df <- cbind(validation_df, predictions)
# 2a. Correctly chosen bids
viz_df %>%
  filter(at_large == 1 & predictions == 1) %>%
  select(team, espn_logo, year, net, at_large, predictions) %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_label(team = "Team",
             year = "Year",
             net = "NET Ranking",
             espn_logo = "",
             at_large = "At Large Bid",
             predictions = "Predicted Bid") %>%
  gtExtras::gt_img_rows(espn_logo) %>%
  tab_header(title = "Teams Accurately Predicted to Make the Tournament")
# 2b. Incorrectly chosen bids
viz_df %>%
  filter(at_large != predictions) %>%
  select(team, espn_logo, year, net, at_large, predictions) %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_label(
    team = "Team",
    year = "Year",
    espn_logo = "",
    net = "NET Ranking",
    at_large = "At Large Bid",
    predictions = "Predicted Bid"
  ) %>%
  gtExtras::gt_img_rows(espn_logo) %>%
  tab_header(title = "Teams Incorrectly Predicted to Make/Miss the Tournament")
# 3. Feature Importance Plot
importance <- varImp(rf_model)
plot(importance, main = "Variable Importance Plot")
# Make predictions for this years data
prediction_df <- prediction_tourney_df %>%
  select(-c(conf, preseason_ap_ranking)) %>%
  na.omit()
newdata <- prediction_df %>%
  select(-c(team, color, alt_color, espn_logo, logo))
tourney_preds <- predict(rf_model, newdata = newdata, type = "prob")[, "1"]
preds <- cbind(prediction_df, tourney_preds)
# Visualize this years predictions
at_large_preds <- preds %>% 
  # Remove auto-bids
  filter(!team %in% c("Drake", "Illinois", "Saint Mary's", "Connecticut",
                      "New Mexico", "North Carolina St.", "Oregon", "Iowa St.", "Auburn"))
proj_table <- at_large_preds %>%
  arrange(desc(tourney_preds)) %>%
  mutate(index = 1:nrow(at_large_preds)) %>%
  head(44) %>%
  select(index, team, espn_logo, net, tourney_preds) %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_label(
    index = "Team Number",
    team = "Team",
    espn_logo = "",
    net = "NET Ranking",
    tourney_preds = "Tourney Probability"
  ) %>%
  gtExtras::gt_img_rows(espn_logo) %>%
  tab_header(title = "Tournament Projections")
gtsave(proj_table, "NCAA 2024 Projections.png")