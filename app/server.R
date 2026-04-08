# server.R
# Create the server logic for the Shiny app

server <- function(input, output, session) {
    # Plot descriptions
    output$boxplot_description <- renderText({
        "Box plots show the distribution of scores for each food type, highlighting median values, quartiles, and outliers. Useful for comparing variability and identifying foods with consistent vs. polarizing ratings."
    })

    output$heatmap_description <- renderText({
        "Individual respondent heatmap shows how each person rated each food type. Useful for identifying rating patterns, outlier respondents, and foods with universal appeal vs. divisive opinions."
    })

    output$avg_var_description <- renderText({
        "Scatter plot comparing average scores vs. variability (standard deviation). Higher SD = more variability. Top-left: universal favorites (high average, consistent). Top-right: polarizing but well-liked (high average, mixed opinions). Bottom-left: universally disliked. Bottom-right: polarizing and poorly rated."
    })

    output$top_rank_description <- renderText({
        "Bar chart showing how often each food received top ratings (9-10). Identifies foods that inspire passionate approval, even if they don't have the highest average scores."
    })

    output$ridge_description <- renderText({
        "Ridge plots show the complete distribution shape for each food's ratings. Superior to box plots for survey data as they reveal patterns like bimodal distributions (love-it-or-hate-it foods) and skewness."
    })

    output$pca_description <- renderText({
        "Principal Component Analysis would reveal underlying patterns in how people rate different foods, identifying groups of similar tastes and foods that cluster together."
    })

    output$bt_abilities_description <- renderText({
        "Bradley-Terry model rankings with uncertainty intervals. Shows the definitive food rankings based on pairwise comparisons, with error bars indicating confidence in each position."
    })

    output$win_percentage_description <- renderText({
        "Lollipop chart version of win percentages, providing a cleaner visualization than bar charts. Makes it easy to compare relative performance between foods."
    })

    output$matchup_description <- renderText({
        "Heatmap showing head-to-head win percentages between all food pairs. Read as: left-side food wins X% of the time against bottom food. Reveals interesting matchup dynamics and upset potential."
    })

    output$correlation_description <- renderText({
        "Correlation plot comparing Bradley-Terry abilities vs. win percentages. High correlation (0.994) validates that both methods agree, resolving the original Elo ranking discrepancies."
    })

    output$score_distribution_description <- renderText({
        "Histogram of all individual ratings across all foods. Shows the overall distribution of how people use the 1-10 rating scale and whether there are rating biases."
    })

    output$score_trend_description <- renderText({
        "Line plot comparing mean vs. median scores across food rankings. Reveals whether outlier ratings skew averages and identifies foods where consensus differs from typical ratings."
    })

    output$score_density_description <- renderText({
        "Overlapped density plots showing rating distributions for all foods simultaneously. Useful for spotting overall patterns, though individual foods are hard to distinguish due to the large number."
    })

    # Combined Data Summary & League Table
    output$combined_data_table <- DT::renderDataTable(
        {
            # Combine raw statistics with league table results
            combined_data <- raw_potato_data_summarise |>
                left_join(league_table, by = c("food_type" = "product1")) |>
                arrange(desc(win_percentage)) |>
                mutate(
                    Position = row_number(),
                    `Food Type` = food_type,
                    `Mean Score` = round(mean_score, 2),
                    `Median Score` = round(median_score, 2),
                    `Std Dev` = round(sd_score, 2),
                    `Wins` = wins,
                    `Losses` = losses,
                    `Draws` = draws,
                    `Win %` = round(win_percentage, 1)
                ) |>
                select(
                    Position,
                    `Food Type`,
                    `Mean Score`,
                    `Median Score`,
                    `Std Dev`,
                    Wins,
                    Losses,
                    Draws,
                    `Win %`
                )

            combined_data
        },
        options = list(
            pageLength = 35,
            searching = TRUE,
            ordering = TRUE,
            info = TRUE,
            lengthChange = TRUE,
            columnDefs = list(
                list(targets = 0, width = "60px"), # Position column width
                list(targets = c(2:4, 8), className = "dt-right") # Right-align numeric columns
            ),
            dom = "Blfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
        ),
        rownames = FALSE
    )

    # Visualizations Tab
    # boxplot of results
    output$boxplot <- renderPlot({
        ggplot(
            data = raw_potato_data,
            aes(y = reorder(food_type, score, mean), x = score)
        ) +
            geom_boxplot(fill = "steelblue", alpha = 0.7) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none"
            ) +
            labs(y = "Food Type", x = "Score", title = "Box Plot of Potato Rankings")
    })

    # clustered heatmap of results
    output$heatmap <- renderPlot(
        {
            # Create user numbers instead of names and order food types by league table
            heatmap_data <- raw_potato_data |>
                mutate(User_Number = as.numeric(factor(Name, levels = unique(Name)))) |>
                mutate(User = paste("User", User_Number)) |>
                mutate(food_type = factor(food_type, levels = league_table$product1))

            # Create properly ordered factor levels for users (numeric order, not alphabetic)
            user_levels <- paste("User", sort(unique(heatmap_data$User_Number)))
            heatmap_data$User <- factor(heatmap_data$User, levels = user_levels)

            ggplot(
                data = heatmap_data,
                aes(x = food_type, y = User, fill = score)
            ) +
                geom_tile() +
                scale_fill_viridis_c(name = "Score") +
                theme_minimal() +
                theme(
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8),
                    legend.position = "right",
                    panel.grid = element_blank()
                ) +
                labs(
                    x = "Food Type (Ordered by League Ranking)", y = "Respondent",
                    title = "Heatmap of Individual Ratings"
                )
        },
        height = 600
    )

    # Bradley-Terry Analysis Tab
    output$bt_results_table <- DT::renderDataTable(
        {
            bt_table_data <- bt_rankings |>
                select(bt_rank, product, bt_ability, win_percentage, bt_std_error) |>
                mutate(
                    `Rank` = bt_rank,
                    `Food Type` = product,
                    `BT Ability` = round(bt_ability, 3),
                    `Win %` = round(win_percentage, 1),
                    `Std Error` = round(bt_std_error, 3)
                ) |>
                select(Rank, `Food Type`, `BT Ability`, `Win %`, `Std Error`)

            bt_table_data
        },
        options = list(
            pageLength = 35,
            searching = FALSE,
            ordering = FALSE,
            info = FALSE,
            lengthChange = FALSE,
            columnDefs = list(
                list(targets = c(0, 2, 3, 4), className = "dt-right"),
                list(targets = 0, width = "60px")
            )
        ),
        rownames = FALSE
    )

    # Additional visualizations can be added here for the Visualizations tab
    output$average_variability_plot <- renderPlot({
        # Code to create average variability plot
        ggplot(
            data = raw_potato_data_summarise,
            aes(x = mean_score, y = sd_score, label = food_type)
        ) +
            geom_point(color = "steelblue", size = 3) +
            geom_text(vjust = -0.5, hjust = 0.5) +
            theme_minimal() +
            labs(x = "Average Score", y = "Standard Deviation of Scores") +
            theme(legend.position = "none")
    })

    output$top_rank_bar_chart <- renderPlot({
        # Code to create top rank bar chart
        top_rank_counts <- raw_potato_data |>
            filter(score >= 9) |>
            group_by(food_type) |>
            summarise(count = n()) |>
            arrange(desc(count))

        ggplot(data = top_rank_counts, aes(x = reorder(food_type, count), y = count)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            theme_minimal() +
            labs(
                x = "Food Type", y = "Count of Top Ranks (9 or 10)",
                title = "High Rating Frequency"
            ) +
            theme(legend.position = "none")
    })

    output$ridge_plot <- renderPlot({
        # Code to create ridge plot
        ggplot(raw_potato_data, aes(x = score, y = reorder(food_type, score, mean), fill = after_stat(x))) +
            geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, alpha = 0.8) +
            scale_fill_viridis_c(name = "Score", option = "C") +
            theme_minimal() +
            labs(
                title = "Distribution of Ratings by Food Type",
                subtitle = "Ridge plots show rating patterns better than box plots",
                x = "Rating (1-10)",
                y = "Food Type"
            ) +
            theme(legend.position = "right")
    })

    output$bt_abilities_plot <- renderPlot({
        # Code to create Bradley-Terry abilities plot
        ggplot(bt_rankings, aes(x = reorder(product, bt_ability), y = bt_ability)) +
            geom_segment(aes(xend = product, yend = 0), color = "gray60", size = 1) +
            geom_point(size = 4, color = "steelblue") +
            coord_flip() +
            theme_minimal() +
            labs(
                title = "Potato Dishes - Bradley-Terry Rankings",
                subtitle = "Bradley-Terry ability scores",
                x = "Food Type", y = "Bradley-Terry Ability"
            ) +
            theme(
                panel.grid.major.x = element_line(color = "gray90"),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank()
            )
    })


    output$win_percentage_plot <- renderPlot({
        # Code to create win percentage plot
        ggplot(league_table, aes(x = reorder(product1, win_percentage), y = win_percentage)) +
            geom_segment(aes(xend = product1, yend = 0), color = "gray60", size = 1) +
            geom_point(size = 4, color = "steelblue") +
            coord_flip() +
            theme_minimal() +
            labs(
                title = "Products by Win Percentage",
                subtitle = "Head-to-head win rates",
                x = "Food Type", y = "Win Percentage (%)"
            ) +
            theme(
                panel.grid.major.x = element_line(color = "gray90"),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank()
            )
    })

    # Matchup plot
    output$matchup_plot <- renderPlotly({
        # Get league table order (best to worst by win percentage)
        league_order <- league_table$product1

        # Convert matrix to long format for ggplot2
        fight_matrix_long <- fight_matrix1 |>
            as.matrix() |>
            reshape2::melt(varnames = c("Product1", "Product2"), value.name = "WinPercentage")

        # Set factor levels to preserve league ranking order
        # Product1 (rows) = winners on y-axis, best at top
        # Product2 (columns) = opponents on x-axis, best at left
        fight_matrix_long$Product1 <- factor(fight_matrix_long$Product1, levels = rev(league_order))
        fight_matrix_long$Product2 <- factor(fight_matrix_long$Product2, levels = league_order)

        # Note: x = Product2 (opponent), y = Product1 (winner)
        # Value shows how often y-axis item beats x-axis item
        p <- ggplot(fight_matrix_long, aes(x = Product2, y = Product1, fill = WinPercentage)) +
            geom_tile() +
            geom_text(aes(label = round(WinPercentage, 0)), size = 2.5, color = "white") +
            scale_fill_viridis_c(name = "Win %") +
            theme_minimal() +
            labs(
                title = "Win Percentage Matchup Heatmap",
                subtitle = "Left side (Y) wins X% vs bottom side (X)",
                x = "Opponent (Left to Right: Best to Worst)",
                y = "Winner (Top to Bottom: Best to Worst)"
            ) +
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
                axis.text.y = element_text(size = 8),
                legend.position = "right"
            )

        # Convert to interactive plotly
        ggplotly(p, tooltip = c("x", "y", "fill"))
    })

    output$correlation_plot <- renderPlot({
        # Code to create correlation plot
        ggplot(bt_rankings, aes(x = bt_ability, y = win_percentage)) +
            geom_point(color = "steelblue", size = 3) +
            geom_text(aes(label = product), vjust = -0.5, hjust = 0.5, size = 2.5) +
            theme_minimal() +
            labs(
                x = "Bradley-Terry Ability", y = "Win Percentage",
                title = "Bradley-Terry vs Win Percentage",
                subtitle = paste("Correlation:", round(cor(bt_rankings$bt_ability, bt_rankings$win_percentage), 3))
            ) +
            theme(legend.position = "none") +
            geom_smooth(method = "lm", se = FALSE, color = "red", alpha = 0.5)
    })

    output$score_distribution_plot <- renderPlot({
        # Code to create score distribution plot
        ggplot(raw_potato_data, aes(x = score)) +
            geom_histogram(bins = 10, fill = "steelblue", alpha = 0.7, color = "white") +
            theme_minimal() +
            labs(
                x = "Score", y = "Frequency",
                title = "Overall Score Distribution",
                subtitle = "How are ratings distributed across all foods?"
            ) +
            scale_x_continuous(breaks = 1:10)
    })

    output$score_trend_plot <- renderPlot({
        # Code to create score trend plot
        # Show how each food type varies across respondents
        raw_potato_data |>
            group_by(food_type) |>
            summarise(
                mean_score = mean(score, na.rm = TRUE),
                median_score = median(score, na.rm = TRUE),
                .groups = "drop"
            ) |>
            arrange(mean_score) |>
            mutate(rank = row_number()) |>
            ggplot(aes(x = rank)) +
            geom_line(aes(y = mean_score, color = "Mean"), size = 1.2) +
            geom_line(aes(y = median_score, color = "Median"), size = 1.2) +
            scale_color_manual(values = c("Mean" = "steelblue", "Median" = "orange")) +
            theme_minimal() +
            labs(
                x = "Food Rank (worst to best)", y = "Score",
                title = "Score Trends Across Rankings",
                subtitle = "Mean vs Median scores",
                color = "Statistic"
            ) +
            theme(legend.position = "top")
    })

    # ===== SUMMARY STATISTICS CARDS =====
    output$total_responses <- renderValueBox({
        valueBox(
            value = tags$div(style = "font-size: 20px;", length(unique(raw_potato_data$Name))),
            subtitle = "Total Responses",
            icon = icon("users", style = "font-size: 40px;"),
            color = "blue"
        )
    })

    output$total_foods <- renderValueBox({
        valueBox(
            value = tags$div(style = "font-size: 20px;", length(unique(raw_potato_data$food_type))),
            subtitle = "Foods Ranked",
            icon = icon("utensils", style = "font-size: 40px;"),
            color = "green"
        )
    })

    output$top_food <- renderValueBox({
        top_food_name <- league_table$product1[1]
        # Truncate more aggressively for better fit
        display_name <- top_food_name
        valueBox(
            value = tags$div(style = "font-size: 20px;", display_name),
            subtitle = "Top Ranked Food",
            icon = icon("trophy", style = "font-size: 40px;"),
            color = "yellow"
        )
    })

    output$most_controversial <- renderValueBox({
        most_controversial <- raw_potato_data_summarise |>
            arrange(desc(sd_score)) |>
            slice(1) |>
            pull(food_type)
        # Truncate more aggressively for better fit
        display_name <- most_controversial
        valueBox(
            value = tags$div(style = "font-size: 20px;", display_name),
            subtitle = "Most Divisive",
            icon = icon("exclamation-triangle", style = "font-size: 40px;"),
            color = "red"
        )
    })

    # ===== HEAD-TO-HEAD COMPARISON =====
    # Update food choices
    observe({
        food_choices <- sort(unique(raw_potato_data$food_type))
        updateSelectInput(session, "food1", choices = food_choices, selected = food_choices[1])
        updateSelectInput(session, "food2", choices = food_choices, selected = food_choices[2])
    })

    # Head-to-head comparison plot
    output$h2h_comparison <- renderPlot({
        if (input$compare_foods > 0) {
            req(input$food1, input$food2)

            # Get data for both foods
            food1_data <- raw_potato_data |> filter(food_type == input$food1)
            food2_data <- raw_potato_data |> filter(food_type == input$food2)

            combined_h2h <- bind_rows(
                food1_data |> mutate(Food = input$food1),
                food2_data |> mutate(Food = input$food2)
            )

            ggplot(combined_h2h, aes(x = Food, y = score, fill = Food)) +
                geom_boxplot(alpha = 0.7) +
                geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
                theme_minimal() +
                scale_fill_manual(values = c("steelblue", "orange")) +
                labs(
                    title = paste("Head-to-Head:", input$food1, "vs", input$food2),
                    x = "Food Type",
                    y = "Rating (1-10)"
                ) +
                theme(legend.position = "none") +
                coord_flip()
        }
    })

    # Head-to-head statistics table
    output$h2h_stats <- DT::renderDataTable(
        {
            if (input$compare_foods > 0) {
                req(input$food1, input$food2)

                # Get head-to-head results
                h2h_result <- results |>
                    filter((product1 == input$food1 & product2 == input$food2) |
                        (product1 == input$food2 & product2 == input$food1)) |>
                    select(product1, product2, wins1, wins2, draws)

                if (nrow(h2h_result) > 0) {
                    if (h2h_result$product1[1] == input$food1) {
                        stats_table <- data.frame(
                            Metric = c("Wins", "Losses", "Draws", "Win %", "Mean Score", "Median Score"),
                            Food1 = c(
                                h2h_result$wins1[1],
                                h2h_result$wins2[1],
                                h2h_result$draws[1],
                                paste0(round(h2h_result$wins1[1] / (h2h_result$wins1[1] + h2h_result$wins2[1] + h2h_result$draws[1]) * 100, 1), "%"),
                                round(mean(raw_potato_data$score[raw_potato_data$food_type == input$food1], na.rm = TRUE), 2),
                                round(median(raw_potato_data$score[raw_potato_data$food_type == input$food1], na.rm = TRUE), 2)
                            ),
                            Food2 = c(
                                h2h_result$wins2[1],
                                h2h_result$wins1[1],
                                h2h_result$draws[1],
                                paste0(round(h2h_result$wins2[1] / (h2h_result$wins1[1] + h2h_result$wins2[1] + h2h_result$draws[1]) * 100, 1), "%"),
                                round(mean(raw_potato_data$score[raw_potato_data$food_type == input$food2], na.rm = TRUE), 2),
                                round(median(raw_potato_data$score[raw_potato_data$food_type == input$food2], na.rm = TRUE), 2)
                            )
                        )
                        # Set proper column names
                        colnames(stats_table)[2:3] <- c(input$food1, input$food2)
                    } else {
                        stats_table <- data.frame(
                            Metric = c("Wins", "Losses", "Draws", "Win %", "Mean Score", "Median Score"),
                            Food1 = c(
                                h2h_result$wins2[1],
                                h2h_result$wins1[1],
                                h2h_result$draws[1],
                                paste0(round(h2h_result$wins2[1] / (h2h_result$wins1[1] + h2h_result$wins2[1] + h2h_result$draws[1]) * 100, 1), "%"),
                                round(mean(raw_potato_data$score[raw_potato_data$food_type == input$food1], na.rm = TRUE), 2),
                                round(median(raw_potato_data$score[raw_potato_data$food_type == input$food1], na.rm = TRUE), 2)
                            ),
                            Food2 = c(
                                h2h_result$wins1[1],
                                h2h_result$wins2[1],
                                h2h_result$draws[1],
                                paste0(round(h2h_result$wins1[1] / (h2h_result$wins1[1] + h2h_result$wins2[1] + h2h_result$draws[1]) * 100, 1), "%"),
                                round(mean(raw_potato_data$score[raw_potato_data$food_type == input$food2], na.rm = TRUE), 2),
                                round(median(raw_potato_data$score[raw_potato_data$food_type == input$food2], na.rm = TRUE), 2)
                            )
                        )
                        # Set proper column names
                        colnames(stats_table)[2:3] <- c(input$food1, input$food2)
                    }

                    stats_table
                }
            }
        },
        options = list(dom = "t", pageLength = 10),
        rownames = FALSE
    )

    # ===== INTERACTIVE NETWORK GRAPH =====
    output$food_network <- renderVisNetwork({
        # Calculate correlation matrix between foods
        food_matrix <- raw_potato_data |>
            select(Name, food_type, score) |>
            pivot_wider(names_from = food_type, values_from = score) |>
            select(-Name)

        cor_matrix <- cor(food_matrix, use = "pairwise.complete.obs")

        # Create nodes dataframe
        food_stats <- raw_potato_data_summarise |>
            left_join(league_table, by = c("food_type" = "product1"))

        nodes <- data.frame(
            id = food_stats$food_type,
            label = if (input$show_labels) food_stats$food_type else "",
            title = paste0(
                "<b>", food_stats$food_type, "</b><br/>",
                "Mean Score: ", round(food_stats$mean_score, 2), "<br/>",
                "Win %: ", round(food_stats$win_percentage, 1), "%<br/>",
                "Std Dev: ", round(food_stats$sd_score, 2)
            ),
            size = 20 + (food_stats$mean_score - min(food_stats$mean_score, na.rm = TRUE)) * 3,
            stringsAsFactors = FALSE
        )

        # Color nodes based on selected metric
        if (input$node_color == "win_pct") {
            nodes$color <- colorRampPalette(c("red", "yellow", "green"))(100)[
                round(rescale(food_stats$win_percentage, to = c(1, 100)), 0)
            ]
        } else if (input$node_color == "mean_score") {
            nodes$color <- colorRampPalette(c("red", "yellow", "green"))(100)[
                round(rescale(food_stats$mean_score, to = c(1, 100)), 0)
            ]
        } else {
            nodes$color <- colorRampPalette(c("green", "yellow", "red"))(100)[
                round(rescale(food_stats$sd_score, to = c(1, 100)), 0)
            ]
        }

        # Create edges for correlations above threshold
        edges <- data.frame()
        threshold <- input$similarity_threshold

        for (i in 1:(nrow(cor_matrix) - 1)) {
            for (j in (i + 1):nrow(cor_matrix)) {
                if (!is.na(cor_matrix[i, j]) && abs(cor_matrix[i, j]) >= threshold) {
                    edges <- rbind(edges, data.frame(
                        from = rownames(cor_matrix)[i],
                        to = rownames(cor_matrix)[j],
                        weight = abs(cor_matrix[i, j]),
                        width = abs(cor_matrix[i, j]) * 5,
                        color = if (cor_matrix[i, j] > 0) "#4CAF50" else "#F44336",
                        title = paste("Correlation:", round(cor_matrix[i, j], 3))
                    ))
                }
            }
        }

        # Create network
        if (nrow(edges) > 0) {
            visNetwork(nodes, edges) |>
                visOptions(highlightNearest = TRUE, selectedBy = "group") |>
                visLayout(randomSeed = 123) |>
                visPhysics(enabled = TRUE, solver = "forceAtlas2Based") |>
                visInteraction(navigationButtons = TRUE)
        } else {
            visNetwork(nodes, data.frame()) |>
                visOptions(highlightNearest = TRUE) |>
                visLayout(randomSeed = 123)
        }
    })

    # Navigation button observers
    observeEvent(input$nav_survey, {
        updateTabItems(session, "tabs", "survey_form")
    })

    observeEvent(input$nav_data, {
        updateTabItems(session, "tabs", "data_league")
    })

    observeEvent(input$nav_h2h, {
        updateTabItems(session, "tabs", "h2h_compare")
    })

    observeEvent(input$nav_network, {
        updateTabItems(session, "tabs", "network")
    })

    observeEvent(input$nav_viz, {
        updateTabItems(session, "tabs", "visualisations")
    })

    observeEvent(input$nav_bt, {
        updateTabItems(session, "tabs", "bt_analysis")
    })

    observeEvent(input$nav_about, {
        updateTabItems(session, "tabs", "about")
    })
}
