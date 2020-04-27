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

# Define UI for application 

ui <- navbarPage(
    
    # INTRO / TOP INDUSTRY PAGE
    
    "How Startups Can Thrive in the US",
    
    tabPanel("Introduction",
             fluidPage(
                 titlePanel(
                     h1("An Overview of Startups from 1979-2015",
                        align = "center")
                     ),
                 p("An exploratory analysis using data from Crunchbase to better understand 
                   what makes a successful startup. Brings in college/university data to explore
                   the relationship between higher education institutions and the founding of 
                   startups in the same area.", 
                   align = "center"),
                 hr(), 
                 h2("Top-Funded Industries", 
                    align = "center"),
                 br(),
                 fluidRow(
                     column(6,
                            offset = 3,
                            gt_output("industries_plot")
                 )),
                 br(),
                 p("To understand which industries received the most aggregate funding, I 
                              grouped the funding data by category (industry) and arranged in descending
                              order by total funding. It's not surprising that biotech, software, and mobile come out
                              at the top!", 
                   align = "center"),
                 hr(),
                 h2("Total Funding Over Time", 
                    align = "center"),
                 br(),
                 fluidRow(
                     column(10,
                            offset = 2,
                            imageOutput("log_over_time")
                     )
                 ),
                 br(),
                 p("Here, we can see the oscillations in total startup funding over time from 1979-2015. 
                              I used a logarithmic scale to fit the values, as total funding increased exponentially 
                              from the late 1990s! It seems some of the values go under 0 between 1980 and 1995, but in reality they are
                              just hitting 0. The animation makes it appear as if goes below by obscuring the points with the axes.",
                   align = "center")
             )),
    
    tabPanel("Top Industries",
             fluidPage(
                 titlePanel(
                     h1("Funding Over Time by Industry", align = "center")
                     ),
                 hr(),
                 sidebarLayout(position = "left",
                               sidebarPanel(
                                   selectInput(
                                       "plot_type",
                                       "Plot Type",
                                       c("Normal Scale" = "a", "Logarithmic Scale" = "b")
                                       )),
                  mainPanel(plotOutput("ind_funding_over_time"))
                  ),
                 hr(),
                 p("I plotted funding trends for each of the top 9 industries using both a normal y scale and a 
                   logarithmic y scale. Using the log scale allows us to account for any skewedness towards larger values,
                   as investments for these industries began to increase significantly in the late 1990s.",
                   align = "center")
                 )),
    
    tabPanel("Top Cities",
             fluidPage(
                 titlePanel(
                     h1("Top Cities for Funding", align = "center"),
                     ),
                 h3("By Industry", align = "center"),
                 hr(),
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
                 ),
                 hr(),
                 p("I grouped the funding data out by cities for each of the top 9 industries. This allows us to see in which
                   cities startups have historically raised the most money.",
                   align = "center")
                 )), 
    
    tabPanel("Education",
             fluidPage(
                 titlePanel(
                     h1("Impact of Education", align = "center")
                     ),
                 p("There are many factors that could impact whether or not a startup is founded. 
                   Specifically, I was interested in how nearby higher educational institutions (like colleges, 
                   community colleges, and technical schools) affected the number of new startups
                   in recent years. I hypothesized that the more colleges and universities there were in proximity to 
                   a potential startup location, the more new startups there would be, as colleges would be able to provide
                   a talent pool of new grads and experienced researchers, knowlegde transfer, and funding. 
                   I chose to examine the last five years of the dataset, 2011-2015. To do so, I used 
                   data on higher education institutions from the US from the Department of Homeland Security",
                   align = "center"),
                 hr(),
                 h3("Analyzing the Number of Higher Education Institutions", 
                    align = "center"),
                 p("I aggregated the number of all higher ed institutions by state and joined this new dataset with my
                 current startup dataset. This new dataset had one row for every state. I then ran a linear regression 
                 on the number of startups founded in each state from 2011-2015. I assumed that `proximity` would be within 
                 state lines. This provided a general picture on the general higher education levels and its potential effect 
                   on the number of new startups.",
                   align = "center"),
                 br(),
                 fluidRow(
                     column(8, 
                            offset = 2,
                            gt_output("reg_gt"),
                     )),
                 br(),
                 p("As the regression shows, there is a relationship between the number of institutions within state borders and 
                 the number of startups founded in this time period. The inst_count estimate indicates that for every additional higher ed institution, there is
                   approximately 4 new startups founded. Moreover, because the intercept is so negative, this 
                   may be indicative of how important higher education institutions are in promoting new startups, 
                   like funneling talent, research, or capital, especially when there are very startups in an area to begin with.", 
                   align = "center"),
                 br(),
                 fluidRow(
                     column(8, 
                            offset = 2,
                            plotOutput("reg_plot"),
                     ))
             )),

tabPanel("About",
         titlePanel(
             h1("About This Project", 
                align = "center")),
         hr(),
         h3("Project Motivations",
            align = "center"),
         p("After working at a startup last summer, I became interested in how startups functioned 
           and what factors made them successful. I came across various startup datasets online and 
           became excited to see what insights I could find!", 
           align = "center"),
         h3("Data Sources",
            align = "center"),
         p("I obtained startup data from a Dec. 4, 2015 Crunchbase Data Export found here: 
           https://github.com/notpeter/crunchbase-data. Specifically, I used two csv files: companies.csv (informattion
           about startups founded betweeen 1977 and 2015) and investments.csv (records of funding rounds for each startup 
           betweeen 1977 and 2015).",
           align = "center"),
         p("I obtained data on US higher educational institutions from the US Department of Homeland Security
           here: https://hifld-geoplatform.opendata.arcgis.com/datasets/colleges-and-universities/data. I 
           cleaned up extraneous columns in excel before loading into R.", align = "center"),
         h3("Methodology",
            align = "center"),
         p("Much of my work went into organizing the data to make the visualization easier. Specifically, I had to separate and rejoin
           columns to organize the startup categories, as startups would often have more than one category (one even had 44). 
           This  then anabled me to perform the rest of my analysis.", align = "center"),
         h3("Conclusions",
            align = "center"),
         p("I learned a lot about what it takes to start and maintain a successful business. Location, industry, and 
           timing are important. While these may seem like common knowledge, it was very eye-opening to generate these 
           insights through the data. By analyzing historical data trends, I was able to see how some industries have changed over
           time as well as how geographical characteristics, like nearby colleges, matter.", 
           align = "center"),
         h3("Future Directions",
            align = "center"),
         p("There is still a lot that could be done to expand this project. For example, I could examine how regional factors
           (like income or population) impact how many startups are founded, analyze where most startups get their
           funding from by industry, or analyzing along more local levels than by state, to name a few.", align = "center"),
         h3("Acknowledgements",
            align = "center"),
         p("Special thanks to Preceptor, my TF Kaneesha, and the Gov1005 teaching staff for equipping me
           with the tools to complete this project!", align = "center"),
         h3("About Me",
            align = "center"),
         p("My name is Belinda and I'm a junior studying History of Science. 
         You can reach me at belindahu@college.harvard.edu.",
           align = "center"),
         p("https://github.com/belindahu", 
           align = "center")
         
))




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

# Run the application 
shinyApp(ui = ui, server = server)
