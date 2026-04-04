# global.R


# load in packages
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(pheatmap)
library(tibble)
library(purrr)
library(BradleyTerry2)


# read in data... haha nice pun ;)
raw_potato_data <- read_csv("app/data/Potato Ranking Form.csv") |>
  pivot_longer(
    cols = 3:36,
    names_to = "food_type",
    values_to = "score"
  )

raw_potato_data_summarise <- raw_potato_data |>
  group_by(food_type) |>
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE)
  ) |>
  arrange(desc(mean_score))

# box and whisker plot of results
ggplot(
  data = raw_potato_data,
  aes(x = score, y = reorder(food_type, score, mean))
) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none")

# heatmap of results
# individual scores
ggplot(
  data = raw_potato_data,
  aes(x = food_type, y = Name, fill = score)
) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  )

# clustered heatmap
heatmap_data <- raw_potato_data |>
  pivot_wider(names_from = food_type, values_from = score) |>
  select(-c("Timestamp", "Name"))

pheatmap(heatmap_data, cluster_rows = TRUE, cluster_cols = TRUE)

# stacked bar chart of results
ggplot(
  data = raw_potato_data,
  aes(x = food_type, fill = factor(score))
) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  )


# number of times each food type was ranked at the top (9 or 10)
top_rank_counts <- raw_potato_data |>
  filter(score >= 9) |>
  group_by(food_type) |>
  summarise(count = n()) |>
  arrange(desc(count))

# bar chart of top rank counts
ggplot(
  data = top_rank_counts,
  aes(x = reorder(food_type, count), y = count)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Food Type", y = "Count of Top Ranks (9 or 10)") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  )

# average vs variability plot
ggplot(
  data = raw_potato_data_summarise,
  aes(x = mean_score, y = sd_score, label = food_type)
) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5) +
  theme_minimal() +
  labs(x = "Average Score", y = "Standard Deviation of Scores") +
  theme(legend.position = "none")


# PCA plot of results
# pca_data <- raw_potato_data |>
#   pivot_wider(names_from = food_type, values_from = score) |>
#   select(-c("Timestamp", "Name"))

# library(FactoMineR)
# library(factoextra)
# pca_result <- PCA(pca_data, graph = FALSE)
# fviz_pca_var(pca_result, label = "none", habillage = pca_data$Name) +
#   theme_minimal() +
#   labs(title = "PCA of Potato Rankings") +
#   theme(legend.position = "right")


# google fight style functionality

# create a function to compare two food types
fight <- function(df, prod1, prod2) {
  r1 <- df[[prod1]]
  r2 <- df[[prod2]]

  wins1 <- sum(r1 > r2, na.rm = TRUE)
  wins2 <- sum(r2 > r1, na.rm = TRUE)
  draws <- sum(r1 == r2, na.rm = TRUE)

  data.frame(
    product1 = prod1,
    product2 = prod2,
    wins1 = wins1,
    wins2 = wins2,
    draws = draws
  )
}

fight(heatmap_data, "Mashed Potatoes", "Thrice-Cooked Chips")

# run all matchups against each other
# remove person and timestamp columns
matchups <- expand.grid(
  prod1 = unique(raw_potato_data$food_type),
  prod2 = unique(raw_potato_data$food_type)
) |>
  filter(prod1 != prod2)

# Use original data for win percentage calculations
results <- matchups |>
  rowwise() |>
  mutate(fight_result = list(fight(heatmap_data, prod1, prod2))) |>
  unnest(cols = c(fight_result)) |>
  mutate(win_percentage_match = (wins1 / (wins1 + wins2 + draws)) * 100)

# create league table
league_table <- results |>
  group_by(product1) |>
  summarise(
    wins = sum(wins1),
    losses = sum(wins2),
    draws = sum(draws),
    total_matches = wins + losses + draws,
    win_percentage = (wins / total_matches) * 100
  ) |>
  arrange(desc(win_percentage))

# visualise league table
ggplot(league_table, aes(x = reorder(product1, win_percentage), y = win_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Food Type", y = "Win Percentage") +
  theme(legend.position = "none")

# create a heatmap of the fight results
fight_matrix <- results |>
  select(product1, product2, win_percentage_match) |>
  pivot_wider(names_from = product2, values_from = win_percentage_match, values_fill = 0)

# move the last row to be first
fight_matrix1 <- fight_matrix |>
  slice(c(n(), 1:(n() - 1)))

# turn the first column into row names and remove it from the data frame
fight_matrix1 <- fight_matrix1 |>
  column_to_rownames(var = "product1")

# order the columns and rows to match the league table
fight_matrix1 <- fight_matrix1[league_table$product1, league_table$product1]

pheatmap(fight_matrix1, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE, number_format = "%.0f", main = "Win Percentage Matchup Heatmap")


# BRADLEY-TERRY MODEL for sophisticated ranking
cat("Fitting Bradley-Terry model...\n")

# Create Bradley-Terry comparison matrix
bt_data <- results |>
  mutate(
    player1 = product1,
    player2 = product2,
    wins1 = wins1,
    wins2 = wins2
  ) |>
  select(player1, player2, wins1, wins2)

# Fit Bradley-Terry model
bt_model <- BTm(cbind(wins1, wins2), player1, player2, data = bt_data)
bt_abilities <- BTabilities(bt_model)

# Convert to rankings
bt_rankings <- data.frame(
  product = names(bt_abilities[, 1]),
  bt_ability = bt_abilities[, 1],
  bt_std_error = bt_abilities[, 2],
  stringsAsFactors = FALSE
) |>
  arrange(desc(bt_ability)) |>
  left_join(league_table, by = c("product" = "product1")) |>
  mutate(
    bt_rank = row_number(),
    win_pct_rank = rank(desc(win_percentage), ties.method = "min")
  )

# Display top 10 Bradley-Terry rankings
cat("\n=== TOP 10 BRADLEY-TERRY RANKINGS ===\n")
print(bt_rankings[1:10, c("bt_rank", "product", "bt_ability", "win_percentage")])

# Correlation between Bradley-Terry and win percentage
bt_correlation <- cor(bt_rankings$bt_ability, bt_rankings$win_percentage, use = "complete.obs")
cat("\nBradley-Terry correlation with win %:", round(bt_correlation, 3), "\n")

# Bradley-Terry vs Win Percentage comparison plot
ggplot(bt_rankings, aes(x = bt_ability, y = win_percentage)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = product), vjust = -0.5, hjust = 0.5) +
  theme_minimal() +
  labs(
    x = "Bradley-Terry Ability",
    y = "Win Percentage",
    title = "Bradley-Terry Model vs Win Percentage",
    subtitle = paste("Correlation:", round(bt_correlation, 3))
  ) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", se = FALSE, color = "red", alpha = 0.5)

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat("Number of food items:", nrow(bt_rankings), "\n")
cat("Bradley-Terry model fit successful\n")
cat("Use bt_rankings for final potato rankings\n")
