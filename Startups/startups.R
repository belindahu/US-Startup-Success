library(shiny)
library(tidyverse)
library(tidyselect)
library(readr)
library(tidycensus)
library(gt)
library(scales)
library(ggplot2)
library(sf)
library(date)
library(gganimate)
library(skimr)
library(broom)

nine_industries <- read_rds("nine_industries.rds")
top_ten <- read_rds("top_ten.rds")
full_colleges <- read_rds("full_colleges.rds")
ind_funding_over_time <- read_rds("ind_funding_over_time.rds")

# Define UI for application 

ui <- navbarPage(
    
    # INTRO / INDUSTRY PAGE
    
    "How Startups Can Thrive in the US",
    
    tabPanel("Top Industries",
             fluidPage(
                 titlePanel("Top 9 Industries"),
                 mainPanel(gt_output("industries_plot")),
                 titlePanel("Trends in Funding Over Time"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Normal Scale" = "a", "Logarithmic Scale" = "b")
                         )),
                     mainPanel(plotOutput("ind_funding_over_time"))
                 ))),
    
    tabPanel("Top Cities",
             fluidPage(
                 titlePanel("Top Cities for Funding"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "industries",
                             "Industries",
                             levels(nine_industries$category)
                         )),
                     mainPanel(
                         textOutput("selected_industry"),
                         plotOutput("cities_plot"))
                 ))), 
    
    tabPanel("Education",
             fluidPage(
                 titlePanel("Impact of Education"),
                 p("There are many factors that could impact whether or not a startup is founded. 
                   Specifically, I wanted to look into how education in recent years (2011-2015, or the 
                   last five years of the dataset) affected the number of new startups. To do so, I used 
                   data on higher education institutions in the US from the Department of Homeland Security"),
                 h3("Impact of Number of Higher Ed Institutions on Number of Startups Founded"),
                 p("I aggregated the number of all higher ed institutions per state and ran a linear
                   regression on the number of startups founded in each state from 2011-2015. Each state
                   repesented its own row in the dataset, totaling 52 rows."),
                 mainPanel(gt_output("reg_gt")),
                 p("There is a relationship between the number of institutions in an area and the number of startups
                   founded. The inst_count estimate indicates that for every new higher ed institution, there is
                   approximately 4 more startups founded. Moreover, because the intercept is so negative, this 
                   may be indicative of how important higher education institutions are in promoting new startups, 
                   like funneling talent, research, or capital."),
                 mainPanel(plotOutput("reg_plot"))
             ))
)

# DISCUSSION PAGE

# tabPanel("Discussion",
#          titlePanel("Methodology"),
#          h3("Data Sources"),
#          p("Data was sourced from the following link, using the Dec. 4 2015 Crunchbase Data Report."),
#          h3("Research Questions"),
#          p(""),
#          h3("Challenges"),
#          p(""),
#          h3("Conclusions")
#          ),

# ABOUT PAGE

# tabPanel("About", 
#          titlePanel("About"),
#          h3("Project Motivations"),
#          p("Hello, this is where I talk about my project."),
#          h3("About Me"),
#          p("My name is Belinda and I'm a junior studying History of Science. 
#          You can reach me at belindahu@college.harvard.edu."))




# Define server logic required to output plots

server <- function(input, output, session) {
    
    output$ind_funding_over_time <- renderPlot({
        
        ifelse(
            input$plot_type == "a",
            
            a <- ind_funding_over_time %>%
                ggplot(., aes(x = funding_year, y = total, color = category)) +
                facet_wrap(~category) +
                geom_point(show.legend = FALSE) +
                geom_line(show.legend = FALSE) +
                labs(title = "US Investments Over Time for Top-Funded Industry, 1979 - 2015",
                     subtitle = "Using a normal y scale",
                     x = "Year",
                     y = "Funding (Billion USD)"),
            
            a <- ind_funding_over_time%>% 
                ggplot(., aes(x = funding_year, y = total, color = category)) +
                facet_wrap(~category) +
                geom_point(show.legend = FALSE) +
                geom_line(show.legend = FALSE) +
                labs(title = "US Investments Over Time for Top-Funded Industry, 1979 - 2015",
                     subtitle = "Using a log y scale",
                     x = "Year",
                     y = "Funding (Billion USD)") + 
                scale_y_log10() 
        )
        
        a
        
    })
    
    output$cities_plot <- renderPlot({
        
        plot <- nine_industries %>% 
            filter(category == input$industries) %>%
            filter(! is.na(company_city)) %>% 
            filter(company_country_code == "USA") %>% 
            select(raised_amount_usd, company_city, company_state_code) %>% 
            mutate(company_city = paste(company_city, sep = ", ", company_state_code)) %>% 
            group_by(company_city) %>% 
            summarise(total = sum(raised_amount_usd)) %>% 
            arrange(desc(total)) %>% 
            head(10) %>% 
            ggplot(., aes(x = total/1000, y = fct_reorder(company_city, total))) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(x = "Total Funding (Billion USD)",
                 y = "Company Cities",
                 caption = "Based off of Company Location, until 2015")
        
        plot
        
    })
    
    output$selected_industry <- renderText({ 
        
        paste("Top 10 US Cities for", input$industries, "Funding")
        
    })
    
    output$industries_plot <- render_gt({
        
        gt <- top_ten %>% 
            gt() %>% 
            tab_header(
                title = "Top 9 US Startup Industries by Total Funding",
                subtitle = "1979-2015"
            ) %>% 
            cols_label(
                category = "Industry",
                total = "Total Funding (Billion USD)"
            ) %>% 
            cols_align(
                "center"
            ) %>% 
            tab_footnote(
                footnote = "Data from Dec. 4, 2015 Crunchbase Data Report",
                locations = cells_title("title"))
        
        gt
    })
    
    output$reg_gt <- render_gt({
        
        inst_model <- full_colleges %>% 
            lm(startup_count ~ inst_count, data = .) %>% 
            tidy(conf.int = TRUE) %>% 
            select(term, estimate, conf.low, conf.high) %>%
            mutate(estimate = round(estimate, digits = 3), 
                   conf.low = round(conf.low, digits = 3), 
                   conf.high = round(conf.high, digits = 3))
        
        inst_gt <- inst_model %>% 
            gt() %>% 
            tab_header(
                title = "Effect of Number of Higher Level Institutions on Number of Startups Founded in 2011-2015",
                subtitle = "Using College and University Data from US Dept. of Homeland Security"
            ) %>% 
            cols_label(
                term = "Variable",
                estimate = "Estimate",
                conf.low = "Lower bound",
                conf.high = "Upper bound"
            ) %>% 
            cols_align(
                "center"
            )
        
        inst_gt
    })
    
    output$reg_plot <- renderImage({
        
        # Return a list containing the filename, alt text, and sizing
        list(src = "reg_plot.png",
             contentType = 'image/png',
             alt = "This is an image",
             width = 750,
             height = 700)
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
