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
clean_full_colleges <- read_rds("clean_full_colleges.rds")


# Define server logic required to output plots

server <- function(input, output, session) {
    
    output$log_over_time <- renderImage({
        
        # Return a list containing the filename, alt text, and sizing
        list(src = "log_over_time.gif",
             contentType = 'image/gif',
             alt = "This is an animation",
             width = 400,
             height = 400)}, 
    deleteFile = FALSE)
    
    # total funding over time plot, logarithmic and normal scale
    # using pre-loaded ind_funding_over_time data
    
    output$ind_funding_over_time <- renderPlot({
        
        # basing input off of what user chooses in dropdown bar
        
        ifelse(
            input$plot_type == "a",
            
            # generating ggplot based off of user selection - normal scale
            
            a <- ind_funding_over_time %>%
                ggplot(., aes(x = funding_year, y = total, color = category)) +
                facet_wrap(~category) +
                geom_point(show.legend = FALSE) +
                geom_line(show.legend = FALSE) +
                labs(title = "US Investments Over Time for Top-Funded Industries, 1977 - 2015",
                     subtitle = "Using a normal y scale",
                     x = "Year",
                     y = "Funding (Billion USD)"),
            
            # generating ggplot based off of user selection - log scale
            
            a <- ind_funding_over_time %>% 
                ggplot(., aes(x = funding_year, y = total, color = category)) +
                facet_wrap(~category) +
                geom_point(show.legend = FALSE) +
                geom_line(show.legend = FALSE) +
                labs(title = "US Investments Over Time for Top-Funded Industries, 1977 - 2015",
                     subtitle = "Using a log y scale",
                     x = "Year",
                     y = "Funding (Billion USD)") + 
                scale_y_log10() 
        )
        
        a
        
    })
    
    # top cities per industry plot using pre-loaded nine_industries data
    
    output$cities_plot <- renderPlot({
        
        # dynamic plotting using input$industries 
        
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
    
    # adjusting the title of each plot based off of industry selected in dropdown menu
    
    output$selected_industry <- renderText({ 
        
        paste("Top 10 US Cities for", input$industries, "Funding")
        
    })
    
    # outputting top industries gt table using loaded in top_ten data
    
    output$industries_plot <- render_gt({
        
        gt <- top_ten %>% 
            gt() %>% 
            tab_header(
                title = "Top 9 US Startup Industries by Total Funding",
                subtitle = "1977-2015"
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
    
    # outputting the regresing gt table using pre-loaded full_colleges data
    
    output$reg_gt <- render_gt({
        
        inst_model <- full_colleges %>% 
            lm(startup_count ~ inst_count, data = .) %>% 
            tidy(conf.int = TRUE) %>% 
            select(term, estimate, conf.low, conf.high) %>%
            mutate(estimate = round(estimate, digits = 3), 
                   conf.low = round(conf.low, digits = 3), 
                   conf.high = round(conf.high, digits = 3))
        
        # using inst_model to create and print the gt table
        
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
    
    # outputting cleaned up gt table using pre-loaded clean_full_colleges data,
    # tidying up data, and printing gt table
    
    output$clean_reg_gt <- render_gt({
        
        clean_inst_model <- clean_full_colleges %>% 
            lm(startup_count ~ inst_count, data = .) %>% 
            tidy(conf.int = TRUE) %>% 
            select(term, estimate, conf.low, conf.high) %>%
            mutate(estimate = round(estimate, digits = 3), 
                   conf.low = round(conf.low, digits = 3), 
                   conf.high = round(conf.high, digits = 3))
        
        clean_inst_gt <- clean_inst_model %>% 
            gt() %>% 
            tab_header(
                title = "Effect of Number of Higher Level Institutions on Number of New Startups, 2011-2015",
                subtitle = "Removing Outliers below 5th and above 95th Percentile"
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
    })
    
    # plotting the regression by using image saved using ggsave and loaded into
    # Shiny folder
    
    output$reg_plot <- renderImage({
        
        # Return a list containing the filename, alt text, and sizing
        
        list(src = "reg_plot.png",
             contentType = 'image/png',
             alt = "This is an image",
             width = 850,
             height = 800,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
}
