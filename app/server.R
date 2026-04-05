# server.R
# Create the server logic for the Shiny app

server <- function(input, output) {
    # Data Summary Tab
    output$data_summary_table <- renderTable({
        raw_potato_data_summarise
    })

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
    output$heatmap <- renderPlot({
        pheatmap(heatmap_data,
            cluster_rows = FALSE,
            cluster_cols = FALSE
        )
    })

    # League Table Tab
    output$league_table <- renderTable({
        league_table
    })

    # Bradley-Terry Analysis Tab
    output$bt_results_table <- renderTable({
        bt_rankings |>
            select(bt_rank, product, bt_ability, win_percentage, bt_std_error) |>
            slice(1:20) # Top 20 for table display
    })

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
            geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, alpha = 0.8) +
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

    output$pca_plot <- renderPlot({
        # Code to create PCA plot
    })

    output$bt_abilities_plot <- renderPlot({
        # Code to create Bradley-Terry abilities plot
        ggplot(bt_rankings[1:15, ], aes(x = reorder(product, bt_ability), y = bt_ability)) +
            geom_segment(aes(xend = product, yend = 0), color = "gray60", size = 1) +
            geom_point(size = 4, color = "steelblue") +
            coord_flip() +
            theme_minimal() +
            labs(
                title = "Top 15 Potato Dishes - Bradley-Terry Rankings",
                subtitle = "Bradley-Terry ability scores",
                x = "Food Type", y = "Bradley-Terry Ability"
            ) +
            theme(
                panel.grid.major.x = element_line(color = "gray90"),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank()
            )
    })

    output$league_table_plot <- renderPlot({
        # Code to create league table plot
        ggplot(league_table[1:15, ], aes(x = reorder(product1, win_percentage), y = win_percentage)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            theme_minimal() +
            labs(
                x = "Food Type", y = "Win Percentage",
                title = "Top 15 by Win Percentage"
            ) +
            theme(legend.position = "none")
    })

    output$win_percentage_plot <- renderPlot({
        # Code to create win percentage plot
        ggplot(league_table[1:15, ], aes(x = reorder(product1, win_percentage), y = win_percentage)) +
            geom_segment(aes(xend = product1, yend = 0), color = "gray60", size = 1) +
            geom_point(size = 4, color = "steelblue") +
            coord_flip() +
            theme_minimal() +
            labs(
                title = "Top 15 by Win Percentage",
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
    output$matchup_plot <- renderPlot({
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
        ggplot(fight_matrix_long, aes(x = Product2, y = Product1, fill = WinPercentage)) +
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

    output$score_density_plot <- renderPlot({
        # Code to create score density plot
        ggplot(raw_potato_data, aes(x = score, fill = food_type)) +
            geom_density(alpha = 0.3) +
            theme_minimal() +
            labs(
                x = "Score", y = "Density",
                title = "Score Density by Food Type"
            ) +
            theme(legend.position = "none") + # Too many food types for legend
            scale_x_continuous(breaks = 1:10)
    })
}
