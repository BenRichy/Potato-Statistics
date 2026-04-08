# global.R

# load in packages
library(shiny)
library(shinydashboard)
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
library(reshape2)
library(ggridges)
library(DT)
library(visNetwork)
library(plotly)
library(scales)
library(httr)

# GitHub repository information
github_repo <- "BenRichy/Potato-Statistics" # Update this to match your actual GitHub username/repo
github_branch <- "main" # or "master" depending on your default branch

# Function to load data from GitHub
load_data_from_github <- function(filename) {
  base_url <- paste0("https://github.com/", github_repo, "/raw/", github_branch, "/app/data/", filename)
  temp_file <- tempfile(fileext = ".RData")

  tryCatch(
    {
      download.file(base_url, temp_file, mode = "wb", quiet = TRUE)
      load(temp_file)
      return(get(ls()[1])) # Return the first (and presumably only) object
    },
    error = function(e) {
      # Fallback to local files if GitHub download fails
      warning(paste("Failed to load", filename, "from GitHub, trying local file:", e$message))
      local_path <- paste0("data/", filename)
      if (file.exists(local_path)) {
        load(local_path)
        return(get(ls()[1]))
      } else {
        stop(paste("Could not load", filename, "from GitHub or locally"))
      }
    }
  )
}

# load in data from GitHub (with local fallback)
raw_potato_data <- load_data_from_github("raw_potato_data.RData")
heatmap_data <- load_data_from_github("heatmap_data.RData")
results <- load_data_from_github("fight_results.RData")
bt_abilities <- load_data_from_github("bt_abilities.RData")


# summarise data by food type
raw_potato_data_summarise <- raw_potato_data |>
  group_by(food_type) |>
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE)
  ) |>
  arrange(desc(mean_score))


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
pheatmap(heatmap_data, cluster_rows = FALSE, cluster_cols = FALSE)


# number of times each food type was ranked at the top (9 or 10)
top_rank_counts <- raw_potato_data |>
  filter(score >= 9) |>
  group_by(food_type) |>
  summarise(count = n()) |>
  arrange(desc(count))


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



# ===== ADVANCED VISUALIZATIONS FOR RANKING DATA =====

# 1. RIDGE PLOTS - Much better than violin plots for discrete ratings



# 4. SLOPE GRAPH - Compare different ranking methods
ranking_comparison <- bt_rankings |>
  select(product, bt_rank, win_pct_rank) |>
  pivot_longer(cols = c(bt_rank, win_pct_rank), names_to = "method", values_to = "rank") |>
  mutate(method = case_when(
    method == "bt_rank" ~ "Bradley-Terry",
    method == "win_pct_rank" ~ "Win Percentage"
  ))

ggplot(ranking_comparison, aes(x = method, y = -rank, group = product)) +
  geom_line(alpha = 0.6, color = "gray60") +
  geom_point(aes(color = method), size = 2) +
  geom_text(
    data = ranking_comparison |> filter(method == "Bradley-Terry"),
    aes(label = product), hjust = 1, nudge_x = -0.1, size = 3
  ) +
  geom_text(
    data = ranking_comparison |> filter(method == "Win Percentage"),
    aes(label = paste("#", rank)), hjust = 0, nudge_x = 0.1, size = 3
  ) +
  scale_color_manual(values = c("Bradley-Terry" = "steelblue", "Win Percentage" = "orange")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = "Ranking Method Comparison",
    subtitle = "How do Bradley-Terry vs Win % rankings differ?",
    x = "Ranking Method",
    y = "Rank (higher = better)"
  )
