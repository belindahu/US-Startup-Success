# loading in libraries

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

# loading in necessary data

nine_industries <- read_rds("nine_industries.rds")
top_ten <- read_rds("top_ten.rds")
full_colleges <- read_rds("full_colleges.rds")
ind_funding_over_time <- read_rds("ind_funding_over_time.rds")


# Define server logic required to output plots

server <- function(input, output, session) {
    
    output$log_over_time <- renderImage({
        
        # Return a list containing the filename, alt text, and sizing
        list(src = "log_over_time.gif",
             contentType = 'image/gif',
             alt = "This is an animation",
             width = 400,
             height = 400)
    }, deleteFile = FALSE)
    
    output$ind_funding_over_time <- renderPlot({
        
        ifelse(
            input$plot_type == "a",
            
            a <- ind_funding_over_time %>%
                ggplot(., aes(x = funding_year, y = total, color = category)) +
                facet_wrap(~category) +
                geom_point(show.legend = FALSE) +
                geom_line(show.legend = FALSE) +
                labs(title = "US Investments Over Time for Top-Funded Industries, 1979 - 2015",
                     subtitle = "Using a normal y scale",
                     x = "Year",
                     y = "Funding (Billion USD)"),
            
            a <- ind_funding_over_time%>% 
                ggplot(., aes(x = funding_year, y = total, color = category)) +
                facet_wrap(~category) +
                geom_point(show.legend = FALSE) +
                geom_line(show.legend = FALSE) +
                labs(title = "US Investments Over Time for Top-Funded Industries, 1979 - 2015",
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
             width = 700,
             height = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
    
}
