
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

nine_industries <- read_rds("Final-Project/Startups/nine_industries.rds")
top_ten <- read_rds("Final-Project/Startups/top_ten.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tabPanel("Top Industries",
             fluidPage(
                 titlePanel("Top Cities for Funding"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "industries",
                             "Industries",
                             levels(nine_industries$category)
                         )),
                     mainPanel(plotOutput("cities_plot"))
                 )
             )),
    
    tabPanel("Model",
             fluidPage(
                 titlePanel("Top Industries"),
                     mainPanel(gt_output("industries_plot"))
                 )
             )) 


# Define server logic required to draw a histogram
server <- function(input, output) {

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
                labs(title = "Top 10 US Cities for, input$industries, Funding, until 2015",
                     subtitle = "Based off of company location",
                     x = "Total Funding (Billion USD)",
                     y = "Company Cities")
            
            plot
            
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
        
}

# Run the application 
shinyApp(ui = ui, server = server)
