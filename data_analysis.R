# Pre-Processing for Shiny App
# This script is designed to run in GitHub Actions

# Load necessary libraries
suppressMessages({
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
    library(googlesheets4)
})

# Configure Google Sheets authentication (for public sheets, no auth needed)
gs4_deauth()

cat("Starting data processing...\n")

# read in raw data from Google Sheets
google_sheet_url <- "https://docs.google.com/spreadsheets/d/1XKThbLwFV3W1njaK7lmY_SDTLs8urt78jFYkreXa7NA/edit?usp=sharing"

try(
    {
        cat("Reading Google Sheets data...\n")
        raw_potato_data <- read_sheet(google_sheet_url) |>
            pivot_longer(
                cols = 3:36,
                names_to = "food_type",
                values_to = "score"
            )

        cat("Data successfully read from Google Sheets\n")
        cat(paste("Rows:", nrow(raw_potato_data), "\n"))

        # Create directory if it doesn't exist
        if (!dir.exists("app/data")) {
            dir.create("app/data", recursive = TRUE)
        }

        # save the raw data for use in the app
        save(raw_potato_data, file = "app/data/raw_potato_data.RData")
        cat("Saved raw_potato_data.RData\n")

        # create data for heatmap
        heatmap_data <- raw_potato_data |>
            pivot_wider(names_from = food_type, values_from = score) |>
            select(-c("Timestamp", "Name"))

        # save heatmap data for use in the app
        save(heatmap_data, file = "app/data/heatmap_data.RData")
        cat("Saved heatmap_data.RData\n")

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

        # run all matchups against each other
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
        cat("Saved fight_results.RData\n")

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
        cat("Saved bt_abilities.RData\n")

        cat("All data processing completed successfully!\n")
    },
    error = function(e) {
        cat("Error in data processing:", conditionMessage(e), "\n")
        quit(status = 1)
    }
)
