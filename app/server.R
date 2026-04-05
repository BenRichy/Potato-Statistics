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
        pheatmap(
            heatmap_data,
            cluster_rows = FALSE,
            cluster_cols = FALSE,
            color = viridis(100),
            show_rownames = FALSE,
            show_colnames = TRUE,
            main = "Clustered Heatmap of Potato Rankings"
        )
    })

    # League Table Tab
    output$league_table <- renderTable({
        league_table
    })

    # Bradley-Terry Analysis Tab
    output$bt_results_table <- renderTable({
        bt_abilities
    })

    # Additional visualizations can be added here for the Visualizations tab
    output$average_variability_plot <- renderPlot({
        # Code to create average variability plot
    })

    output$top_rank_bar_chart <- renderPlot({
        # Code to create top rank bar chart
    })

    output$ridge_plot <- renderPlot({
        # Code to create ridge plot
    })

    output$pca_plot <- renderPlot({
        # Code to create PCA plot
    })

    output$bt_abilities_plot <- renderPlot({
        # Code to create Bradley-Terry abilities plot
    })

    output$league_table_plot <- renderPlot({
        # Code to create league table plot
    })

    output$win_percentage_plot <- renderPlot({
        # Code to create win percentage plot
    })

    output$matchup_plot <- renderPlot({
        # Code to create matchup plot
    })

    output$correlation_plot <- renderPlot({
        # Code to create correlation plot
    })

    output$score_distribution_plot <- renderPlot({
        # Code to create score distribution plot
    })

    output$score_trend_plot <- renderPlot({
        # Code to create score trend plot
    })

    output$score_density_plot <- renderPlot({
        # Code to create score density plot
    })
}
