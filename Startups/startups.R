#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

top_types_table <- top_types %>% 
    select(category, percent, State) %>% 
    ungroup() %>% 
    gt() %>% 
    tab_header(
        title = "Most Prevalent Startup Categories by State
",
        subtitle = "Data from Dec. 4, 2015 Crunchbase Data Report
"
    ) %>% 
    cols_move_to_start(
        columns = vars(state_code, State)
    ) %>% 
    cols_label(
        state_code = "State Code",
        State = "State",
        category = "Category",
        percent = "Prevalence"
    ) %>% 
    fmt_percent(
        columns = vars(percent)
    ) %>% 
    cols_align(
        "center"
    )

# Define UI for application that draws a histogram
ui <- navbarPage(
    
    # INTRO / INDUSTRY PAGE
    
    "Startup Trends in the US",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Industries"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Top Startup Industries Per State" = "a", "Total Distribution of Industries" = "b")
                         )),
                     mainPanel(plotOutput("intro_plot")))
             )),
    
    # INVESTMENTS PAGE
    
    tabPanel("Investments",
             fluidPage(
                 titlePanel("Investments Over Time"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Top Startup Industries Per State" = "a", "Total Distribution of Industries" = "b")
                         )),
                     mainPanel(plotOutput("intro_plot"))
             )),
    # finish the above.
    
    # REGRESSION PAGE
    
    # DISCUSSION PAGE
    
    tabPanel("Discussion",
             titlePanel("Methodology"),
             h3("Data Sources"),
             p("Data was sourced from the following link, using the Dec. 4 2015 Crunchbase Data Report."),
             h3("Research Questions"),
             p(""),
             h3("Challenges"),
             p(""),
             h3("Conclusions")),
   
    # ABOUT PAGE
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Belinda and I'm a junior studying History of Science. 
             You can reach me at belindahu@college.harvard.edu.")))

# Define server logic required to output plots

server <- function(input, output, session) {
    output$intro_plot <- renderPlot({
        
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            output$table <-
                render_gt(
                    expr = top_types_table,
                    height = px(600),
                    width = px(600)
                ),
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- faithful[, 1]
        )
        
        # Draw the histogram with the specified number of bins
        
        hist(x, col = 'darkgray', border = 'white')
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
