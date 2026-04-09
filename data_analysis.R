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

tryCatch(
    {
        cat("Reading Google Sheets data...\n")
        raw_data <- read_sheet(google_sheet_url)

        # Debug: Print column names to understand structure
        cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
        cat("Total columns:", ncol(raw_data), "\n")

        # Automatically detect food rating columns (typically numeric columns after Timestamp and Name)
        # Skip first 2 columns (Timestamp, Name) and find columns that contain numeric ratings
        food_cols <- names(raw_data)[3:ncol(raw_data)]

        # Filter out any non-food columns (e.g., demographic questions, text responses)
        # Look for columns that contain mostly numeric values between 1-10
        numeric_cols <- c()
        for (col in food_cols) {
            col_data <- raw_data[[col]]
            # Check if column contains mostly numeric values in rating range (1-10)
            numeric_values <- as.numeric(col_data)
            if (sum(!is.na(numeric_values)) > 0) {
                # Check if the numeric values are in a reasonable rating range
                valid_ratings <- numeric_values[!is.na(numeric_values)]
                if (length(valid_ratings) > 0 && min(valid_ratings) >= 1 && max(valid_ratings) <= 10) {
                    numeric_cols <- c(numeric_cols, col)
                }
            }
        }

        cat("Found", length(numeric_cols), "food rating columns\n")
        cat(
            "Food columns:", paste(numeric_cols[1:min(5, length(numeric_cols))], collapse = ", "),
            if (length(numeric_cols) > 5) "..." else "", "\n"
        )

        # Create the long format data using only the detected food columns
        raw_potato_data <- raw_data |>
            select(Timestamp, Name, all_of(numeric_cols)) |>
            pivot_longer(
                cols = all_of(numeric_cols),
                names_to = "food_type",
                values_to = "score"
            ) |>
            filter(!is.na(score)) |> # Remove any missing ratings
            mutate(score = as.numeric(score)) |> # Ensure scores are numeric
            filter(score >= 1 & score <= 10) # Filter valid ratings only

        # Validate we have sufficient data
        if (length(numeric_cols) == 0) {
            stop("No valid food rating columns found. Check your Google Sheet structure.")
        }

        if (nrow(raw_potato_data) == 0) {
            stop("No valid rating data found after filtering.")
        }

        cat("Data successfully read from Google Sheets\n")
        cat(paste("Rows:", nrow(raw_potato_data), "\n"))
        cat(paste("Unique foods:", length(unique(raw_potato_data$food_type)), "\n"))
        cat(paste("Unique respondents:", length(unique(raw_potato_data$Name)), "\n"))

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
