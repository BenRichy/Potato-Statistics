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

results <- matchups |>
  rowwise() |>
  mutate(fight_result = list(fight(heatmap_data, prod1, prod2))) |>
  unnest(cols = c(fight_result))
