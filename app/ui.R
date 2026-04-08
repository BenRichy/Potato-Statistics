# ui.R

# Create the user interface for the Shiny app
# Dashboard layout with a sidebar for navigation and a main panel for displaying content


ui <- dashboardPage(
    dashboardHeader(title = "Potato Rankings"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Survey Form", tabName = "survey_form", icon = icon("edit")),
            menuItem("Data Summary & League", tabName = "data_league", icon = icon("table")),
            menuItem("Head-to-Head Compare", tabName = "h2h_compare", icon = icon("balance-scale")),
            menuItem("Network Graph", tabName = "network", icon = icon("project-diagram")),
            menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
            menuItem("Bradley-Terry Analysis", tabName = "bt_analysis", icon = icon("chart-line")),
            menuItem("About", tabName = "about", icon = icon("user"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "home",
                h2("Welcome to the Potato Rankings App"),
                p("This app allows you to explore the results of our potato ranking survey. Choose a section below to get started:"),

                # Data freshness notice
                div(
                    class = "alert alert-warning",
                    style = "background-color: #fff3cd; border: 1px solid #ffeaa7; color: #856404; padding: 10px; border-radius: 5px; margin: 15px 0;",
                    p(icon("info-circle"),
                        strong("Data Updates:"),
                        "Survey responses are processed every 4 hours. New submissions may take up to 6 hours to appear in the results.",
                        style = "margin: 0; font-size: 14px;"
                    )
                ),

                # Summary Statistics Cards
                fluidRow(
                    valueBoxOutput("total_responses", width = 3),
                    valueBoxOutput("total_foods", width = 3),
                    valueBoxOutput("top_food", width = 3),
                    valueBoxOutput("most_controversial", width = 3)
                ),
                br(),
                fluidRow(
                    column(
                        6,
                        div(
                            style = "text-align: center; margin: 10px;",
                            actionButton("nav_survey",
                                label = div(
                                    icon("edit", style = "font-size: 1.5em;"),
                                    br(),
                                    h4("Take the Survey", style = "font-size: 14px; margin: 5px 0;"),
                                    p("Add your preferences", style = "font-size: 11px; margin: 0;")
                                ),
                                style = "width: 100%; height: 120px; background-color: #007bff; color: white; border: none; border-radius: 10px; font-size: 12px;"
                            )
                        )
                    ),
                    column(
                        6,
                        div(
                            style = "text-align: center; margin: 10px;",
                            actionButton("nav_data",
                                label = div(
                                    icon("table", style = "font-size: 1.5em;"),
                                    br(),
                                    h4("Data & Rankings", style = "font-size: 14px; margin: 5px 0;"),
                                    p("Statistics & league", style = "font-size: 11px; margin: 0;")
                                ),
                                style = "width: 100%; height: 120px; background-color: #28a745; color: white; border: none; border-radius: 10px; font-size: 12px;"
                            )
                        )
                    )
                ),
                br(),
                fluidRow(
                    column(
                        6,
                        div(
                            style = "text-align: center; margin: 10px;",
                            actionButton("nav_h2h",
                                label = div(
                                    icon("balance-scale", style = "font-size: 1.5em;"),
                                    br(),
                                    h4("Head-to-Head", style = "font-size: 14px; margin: 5px 0;"),
                                    p("Direct comparison", style = "font-size: 11px; margin: 0;")
                                ),
                                style = "width: 100%; height: 120px; background-color: #6f42c1; color: white; border: none; border-radius: 10px; font-size: 12px;"
                            )
                        )
                    ),
                    column(
                        6,
                        div(
                            style = "text-align: center; margin: 10px;",
                            actionButton("nav_network",
                                label = div(
                                    icon("project-diagram", style = "font-size: 1.5em;"),
                                    br(),
                                    h4("Network Graph", style = "font-size: 14px; margin: 5px 0;"),
                                    p("Food relationships", style = "font-size: 11px; margin: 0;")
                                ),
                                style = "width: 100%; height: 120px; background-color: #20c997; color: white; border: none; border-radius: 10px; font-size: 12px;"
                            )
                        )
                    )
                ),
                br(),
                fluidRow(
                    column(
                        6,
                        div(
                            style = "text-align: center; margin: 10px;",
                            actionButton("nav_viz",
                                label = div(
                                    icon("chart-bar", style = "font-size: 1.5em;"),
                                    br(),
                                    h4("Visualizations", style = "font-size: 14px; margin: 5px 0;"),
                                    p("Charts & plots", style = "font-size: 11px; margin: 0;")
                                ),
                                style = "width: 100%; height: 120px; background-color: #ffc107; color: black; border: none; border-radius: 10px; font-size: 12px;"
                            )
                        )
                    ),
                    column(
                        6,
                        div(
                            style = "text-align: center; margin: 10px;",
                            actionButton("nav_bt",
                                label = div(
                                    icon("chart-line", style = "font-size: 1.5em;"),
                                    br(),
                                    h4("Bradley-Terry", style = "font-size: 14px; margin: 5px 0;"),
                                    p("Statistical rankings", style = "font-size: 11px; margin: 0;")
                                ),
                                style = "width: 100%; height: 120px; background-color: #dc3545; color: white; border: none; border-radius: 10px; font-size: 12px;"
                            )
                        )
                    )
                )
            ),
            # Survey form page
            tabItem(
                tabName = "survey_form",
                h2("Join the Potato Rankings Survey!"),
                p("Want to add your potato preferences to our analysis? Fill out our survey form below:"),
                br(),
                # Important notice about data processing
                div(
                    class = "alert alert-info",
                    style = "background-color: #d1ecf1; border: 1px solid #bee5eb; color: #0c5460; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                    h5(icon("clock"), " Data Processing Notice", style = "margin-top: 0; color: #0c5460;"),
                    p(strong("Important:"), "After completing the survey, your responses won't appear in the app immediately.", style = "margin-bottom: 8px;"),
                    p("• Data is automatically processed every 4 hours", style = "margin-bottom: 5px;"),
                    p("• It may take up to 6 hours for your results to appear in the visualizations", style = "margin-bottom: 5px;"),
                    p("• The app updates automatically - no need to refresh manually", style = "margin-bottom: 0;")
                ),
                div(
                    style = "text-align: center; margin: 20px;",
                    a(
                        href = "https://forms.gle/1oUJ9WcX9iqSVoxAA", # Replace with actual form URL
                        target = "_blank",
                        class = "btn btn-primary btn-lg",
                        icon("external-link-alt"),
                        " Take the Potato Rankings Survey"
                    )
                ),
                br(),
                h4("About the Survey:"),
                p("• Rate 34 different potato-based foods on a scale of 1-10"),
                p("• Takes approximately 5-10 minutes to complete"),
                p("• Your responses will be included in future analysis updates"),
                p("• All responses are anonymous"),
                br(),
                h4("Instructions:"),
                p("1. Click the survey link above"),
                p("2. Rate each potato dish from 1 (terrible) to 10 (amazing)"),
                p("3. Submit your responses"),
                p("4. Check back later to see how your preferences compare!"),
                br(),
                div(
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;"
                )
            ),
            # Head-to-Head Comparison Tool
            tabItem(
                tabName = "h2h_compare",
                h2("Head-to-Head Food Comparison"),
                p("Compare any two potato dishes directly to see detailed matchup statistics."),
                br(),
                fluidRow(
                    column(
                        4,
                        selectInput("food1", "Select First Food:", choices = NULL, width = "100%")
                    ),
                    column(
                        4,
                        selectInput("food2", "Select Second Food:", choices = NULL, width = "100%")
                    ),
                    column(
                        4,
                        br(),
                        actionButton("compare_foods", "Compare!", class = "btn-primary", width = "100%")
                    )
                ),
                br(),
                conditionalPanel(
                    condition = "input.compare_foods > 0",
                    fluidRow(
                        column(6, plotOutput("h2h_comparison")),
                        column(6, DT::dataTableOutput("h2h_stats"))
                    )
                )
            ),
            # Interactive Network Graph
            tabItem(
                tabName = "network",
                h2("Food Similarity Network"),
                p("Interactive network showing relationships between foods based on rating patterns. Foods that are rated similarly by respondents are connected and clustered together."),
                br(),
                fluidRow(
                    column(
                        3,
                        sliderInput("similarity_threshold", "Similarity Threshold:",
                            min = 0.3, max = 0.9, value = 0.6, step = 0.05
                        ),
                        checkboxInput("show_labels", "Show Food Labels", value = TRUE),
                        selectInput("node_color", "Color Nodes By:",
                            choices = c("Win Percentage" = "win_pct", "Mean Score" = "mean_score", "Variability" = "std_dev"),
                            selected = "win_pct"
                        )
                    ),
                    column(
                        9,
                        visNetworkOutput("food_network", height = "600px")
                    )
                )
            ),
            # tab for combined data summary and league table
            tabItem(
                tabName = "data_league",
                h2("Data Summary & League Table"),
                p("Combined view showing basic statistics and head-to-head performance for each food type."),
                p("Click column headers to sort. Use the search box to filter results."),
                DT::dataTableOutput("combined_data_table")
            ),
            tabItem(
                tabName = "visualisations",
                h2("Visualisations"),
                # different sub tabs for different visualisations
                tabsetPanel(
                    tabPanel(
                        "Matchup Plot",
                        textOutput("matchup_description"),
                        br(),
                        plotOutput("matchup_plot")
                    ),
                    tabPanel(
                        "Box Plot",
                        textOutput("boxplot_description"),
                        br(),
                        plotOutput("boxplot")
                    ),
                    tabPanel(
                        "Response Heatmap",
                        textOutput("heatmap_description"),
                        br(),
                        plotOutput("heatmap")
                    ),
                    tabPanel(
                        "Average vs Variability",
                        textOutput("avg_var_description"),
                        br(),
                        plotOutput("average_variability_plot")
                    ),
                    tabPanel(
                        "Top Rank Bar Chart",
                        textOutput("top_rank_description"),
                        br(),
                        plotOutput("top_rank_bar_chart")
                    ),
                    tabPanel(
                        "Ridge Plot",
                        textOutput("ridge_description"),
                        br(),
                        plotOutput("ridge_plot")
                    ),
                    tabPanel(
                        "Bradley-Terry Abilities",
                        textOutput("bt_abilities_description"),
                        br(),
                        plotOutput("bt_abilities_plot")
                    ),
                    tabPanel(
                        "Win Percentage Plot",
                        textOutput("win_percentage_description"),
                        br(),
                        plotOutput("win_percentage_plot")
                    ),
                    tabPanel(
                        "Correlation Plot",
                        textOutput("correlation_description"),
                        br(),
                        plotOutput("correlation_plot")
                    ),
                    tabPanel(
                        "Score Distribution",
                        textOutput("score_distribution_description"),
                        br(),
                        plotOutput("score_distribution_plot")
                    ),
                    tabPanel(
                        "Score Trend",
                        textOutput("score_trend_description"),
                        br(),
                        plotOutput("score_trend_plot")
                    )
                )
            ),
            tabItem(
                tabName = "bt_analysis",
                h2("Bradley-Terry Analysis"),
                p("The Bradley-Terry model is a sophisticated statistical approach for ranking items based on pairwise comparisons."),
                br(),

                # Explanation cards
                fluidRow(
                    column(
                        6,
                        div(
                            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                            h4("Why Bradley-Terry?", style = "color: #495057;"),
                            h5("Problems with Elo for Survey Data:", style = "color: #dc3545;"),
                            tags$ul(
                                tags$li("Designed for sequential games, not simultaneous ratings"),
                                tags$li("Assumes temporal ordering matters (it doesn't here)"),
                                tags$li("Rating changes based on match order"),
                                tags$li("Poor correlation with win percentages (0.7 vs 0.994)")
                            ),
                            h5("Bradley-Terry Advantages:", style = "color: #28a745;"),
                            tags$ul(
                                tags$li("Specifically designed for pairwise comparison data"),
                                tags$li("Treats all comparisons equally (no temporal bias)"),
                                tags$li("Maximum likelihood estimation for optimal rankings"),
                                tags$li("Provides uncertainty measures (confidence intervals)")
                            )
                        )
                    ),
                    column(
                        6,
                        div(
                            style = "background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                            h4("Our Results", style = "color: #495057;"),
                            h5("Model Performance:", style = "color: #155724;"),
                            tags$ul(
                                tags$li("Bradley-Terry correlation with win %: 0.994"),
                                tags$li("Elo correlation with win %: 0.700"),
                                tags$li("Resolves ranking inconsistencies (e.g., 'Roast Potatoes Small')")
                            ),
                            h5("Interpretation:", style = "color: #155724;"),
                            tags$ul(
                                tags$li("Higher ability = consistently preferred food"),
                                tags$li("Standard errors show ranking confidence"),
                                tags$li("Based on 34 foods × 24 respondents = 816 pairwise comparisons")
                            )
                        )
                    )
                ),
                h3("Top 20 Bradley-Terry Rankings"),
                p("Ranked by Bradley-Terry ability score with uncertainty measures:"),
                DT::dataTableOutput("bt_results_table")
            ),
            # About/Contact page
            tabItem(
                tabName = "about",
                h2("About This Project"),
                br(),
                fluidRow(
                    column(
                        8,
                        div(
                            style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;",
                            h3("Project Overview"),
                            p("This Shiny application analyzes potato food rankings from survey data using advanced statistical methods.
                              The app demonstrates various data visualization and analysis techniques, including Bradley-Terry modeling
                              for pairwise comparisons."),
                            br(),
                            h4("Features:"),
                            tags$ul(
                                tags$li("Interactive data exploration with sortable tables and filtering"),
                                tags$li("Advanced statistical analysis using Bradley-Terry model"),
                                tags$li("Head-to-head food comparison tool"),
                                tags$li("Interactive network graph showing food similarity patterns"),
                                tags$li("Comprehensive visualizations (ridge plots, heatmaps, correlation analysis)")
                            ),
                            br(),
                            h4("Technologies Used:"),
                            p("R, Shiny, shinydashboard, ggplot2, DT, visNetwork, Bradley-Terry modeling")
                        )
                    ),
                    column(
                        4,
                        div(
                            style = "background-color: #e3f2fd; padding: 20px; border-radius: 10px;",
                            h3("Connect With Me", style = "text-align: center;"),
                            br(),
                            div(
                                style = "text-align: center;",
                                # GitHub
                                a(
                                    href = "https://github.com/BenRichy", # Replace with your actual GitHub
                                    target = "_blank",
                                    class = "btn btn-dark btn-lg",
                                    style = "margin: 10px; width: 200px;",
                                    icon("github"),
                                    " GitHub"
                                ),
                                br(),
                                # LinkedIn
                                a(
                                    href = "https://www.linkedin.com/in/ben-richmond-112322123/", # Replace with your actual LinkedIn
                                    target = "_blank",
                                    class = "btn",
                                    style = "background-color: #0077b5; color: white; margin: 10px; width: 200px;",
                                    icon("linkedin"),
                                    " LinkedIn"
                                ),
                                br(),
                                # Email
                                a(
                                    href = "mailto:benjamesrichmond@gmail.com", # Replace with your actual email
                                    class = "btn btn-success btn-lg",
                                    style = "margin: 10px; width: 200px;",
                                    icon("envelope"),
                                    " Email Me"
                                ),
                                br(),
                                # Portfolio/Website
                                a(
                                    href = "https://benrichy.github.io/", # Replace with your actual website
                                    target = "_blank",
                                    class = "btn btn-info btn-lg",
                                    style = "margin: 10px; width: 200px;",
                                    icon("globe"),
                                    " Portfolio"
                                )
                            )
                        )
                    )
                ),
                br(),
                div(
                    style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;",
                    h4("Data & Privacy", style = "color: #856404;"),
                    p("All survey responses are anonymous and used solely for statistical analysis and visualization purposes.",
                        style = "margin-bottom: 0; color: #856404;"
                    )
                )
            )
        )
    )
)
