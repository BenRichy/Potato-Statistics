# ui.R

# Create the user interface for the Shiny app
# Dashboard layout with a sidebar for navigation and a main panel for displaying content


ui <- dashboardPage(
    dashboardHeader(title = "Potato Rankings"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("League Table", tabName = "league_table", icon = icon("trophy")),
            menuItem("Data Summary", tabName = "data_summary", icon = icon("table")),
            menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
            menuItem("Bradley-Terry Analysis", tabName = "bt_analysis", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "home",
                h2("Welcome to the Potato Rankings App"),
                p("This app allows you to explore the results of our potato ranking survey. Use the sidebar to navigate through different sections of the app.")
            ),
            # tab for league table
            tabItem(
                tabName = "league_table",
                h2("League Table"),
                tableOutput("league_table")
            ),
            tabItem(
                tabName = "data_summary",
                h2("Data Summary"),
                tableOutput("data_summary_table")
            ),
            tabItem(
                tabName = "visualisations",
                h2("Visualisations"),
                # different sub tabs for different visualisations
                tabsetPanel(
                    tabPanel("Box Plot", plotOutput("boxplot")),
                    tabPanel("Heatmap", plotOutput("heatmap")),
                    tabPanel("Average vs Variability", plotOutput("average_variability_plot")),
                    tabPanel("Top Rank Bar Chart", plotOutput("top_rank_bar_chart")),
                    tabPanel("Ridge Plot", plotOutput("ridge_plot")),
                    tabPanel("PCA Plot", plotOutput("pca_plot")),
                    tabPanel("Bradley-Terry Abilities", plotOutput("bt_abilities_plot")),
                    tabPanel("League Table Plot", plotOutput("league_table_plot")),
                    tabPanel("Win Percentage Plot", plotOutput("win_percentage_plot")),
                    tabPanel("Matchup Plot", plotOutput("matchup_plot")),
                    tabPanel("Correlation Plot", plotOutput("correlation_plot")),
                    tabPanel("Score Distribution", plotOutput("score_distribution_plot")),
                    tabPanel("Score Trend", plotOutput("score_trend_plot")),
                    tabPanel("Score Density", plotOutput("score_density_plot"))
                )
            ),
            tabItem(
                tabName = "bt_analysis",
                h2("Bradley-Terry Analysis"),
                tableOutput("bt_results_table")
            )
        )
    )
)
