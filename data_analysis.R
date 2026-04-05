# Pre-Processing for Shiny App

# Load necessary libraries
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

# read in raw data
raw_potato_data <- read_csv("app/data/Potato Ranking Form.csv") |>
    pivot_longer(
        cols = 3:36,
        names_to = "food_type",
        values_to = "score"
    )

# save the raw data for use in the app
save(raw_potato_data, file = "app/data/raw_potato_data.RData")

# create data for heatmap
heatmap_data <- raw_potato_data |>
    pivot_wider(names_from = food_type, values_from = score) |>
    select(-c("Timestamp", "Name"))

# save heatmap data for use in the app
save(heatmap_data, file = "app/data/heatmap_data.RData")

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

# fight(heatmap_data, "Mashed Potatoes", "Thrice-Cooked Chips")

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

# save results for use in the app
save(results, file = "app/data/fight_results.RData")




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

# save Bradley-Terry abilities for use in the app
save(bt_abilities, file = "app/data/bt_abilities.RData")
