
#loading in libraries

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
library(shinythemes)

# loading in necessary data

nine_industries <- read_rds("nine_industries.rds")
top_ten <- read_rds("top_ten.rds")
full_colleges <- read_rds("full_colleges.rds")
ind_funding_over_time <- read_rds("ind_funding_over_time.rds")

# NOTE: throughout this file, I blocked the text into more readable chunks by entering
# a new line when the line got too long

# Define UI for application 

ui <- navbarPage(
    
    # INTRO / TOP INDUSTRY PAGE
    
    "How Startups Can Thrive in the US",
    
    # first tab, setting theme to DARKLY, and outlining the titlePanel and
    # mainPanel, adding in spacing using fluidRow
    
    tabPanel("Introduction",
             fluidPage(theme = shinytheme("darkly"),
                 titlePanel(
                     h1("An Overview of Startups from 1977-2015",
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
                     )
                     ),
                 br(),
                 p("To understand which industries received the most aggregate funding, I 
                              grouped the funding data by category (industry) and arranged in descending
                              order by total funding. It's not surprising that biotech, software, 
                              and mobile come out at the top!", 
                   align = "center"),
                 hr(),
                 h2("Total Funding Over Time", 
                    align = "center"),
                 br(),
                 fluidRow(
                     column(4,
                            offset = 2,
                            imageOutput("log_over_time")
                     ),
                     br(),
                     column(5, 
                            p("Here, we can see the oscillations in total startup funding over time from 1977-2015. 
                              I used a logarithmic scale to fit the values, as total funding increased exponentially 
                              from the late 1990s! It seems some of the values go under 0 between 1980 and 1995,
                              but in reality they are just hitting 0. The animation makes it appear as if goes below 
                              by obscuring the points with the axes.",
                              style="padding:60px;")
                            )
                 ),
                 br()
             )
             ),
    
    # TOP INDUSTRIES PAGE
    
    # outlining the titlePanel and mainPanel, adding in spacing using
    # fluidRow, adding in descriptions of graphics
    
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
                                   )
                                   ),
                               mainPanel(plotOutput("ind_funding_over_time"))
                 ),
                 hr(),
                 p("I plotted funding trends for each of the top 9 industries using both a normal y scale and a 
                   logarithmic y scale. Using the log scale allows us to account for any skewedness towards larger values,
                   as investments for these industries began to increase significantly in the late 1990s. Here, we see 
                   that industries like Software and Biotechnology have achieved rapid growth in recent decades,
                   while Clean Technology and Advertising are slowing down.",
                   align = "center"),
                 br(),
                 p("Fun fact: the singular pre-1980 point for Enterprise Software represents ABO Data, which is a consulting
                   company for projects on Big Data, IoT, and Analytics today. It was founded in 1979, which was when it received its
                   first investment, for software and research development. Evidently, enterprise software was not a distinct 
                   industry back then, but after researching the company, it seems to have evolved into more of an enterprise 
                   software company (providing software services for organizations instead of individual consumers) over time.")
             )
             ),
    
    # TOP CITIES PAGE
    
    # outlining the titlePanel and mainPanel, defining sidebarPanel,
    # writing explanations of graphics
    
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
                         )
                         ),
                     mainPanel(
                         textOutput("selected_industry"),
                         plotOutput("cities_plot"))
                 ),
                 br(),
                 p("I grouped the funding data out by cities for each of the top 9 industries.
                   This allows us to see in which cities startups have historically raised the most money.",
                   align = "center"),
                 hr(),
                 h3("Key Takeaways", align = "center"),
                 br(),
                 p("Industies like Clean Technology, Enterprise Software, and Software find most of their funding on specific coasts,
                   in this case, the West Coast. Biotechnology and Health Care find most of their funding in 
                   the Boston/Cambridge area. Interestingly, much of the Mobile funding comes from Kirkland, WA. 
                   A quick scan of the data shows that Clearwire, a telecom operator in the Seattle area, 
                   received significant funding before its acquisition by Sprint Nextel. For other industries, like
                   Advertising, E-Commerce and Internet, the top cities ae more dispersed between the East and West Coasts,
                   as well as the Midwest.",
                   align = "center")
             )
             ), 
    
    # EDUCATION / REGRESSION PAGE
    
    # formatting the various panels to not be too overwhelming,
    # inserting graphics, aligning p blocks, inserting spaces like hr() and br()
    # when necessary
    
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
                   data on higher education institutions from the US Department of Homeland Security",
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
                     )
                     ),
                 br(),
                 p("As the regression shows, there is a relationship between the number of institutions within state borders and 
                 the number of startups founded in this time period. The inst_count estimate indicates that for every additional 
                   higher ed institution, there is approximately 4 new startups founded. Moreover, because the intercept is so negative, this 
                   may be indicative of how important higher education institutions are in promoting new startups, 
                   like funneling talent, research, or capital, especially when there are very few startups in an area to begin with.", 
                   align = "center"),
                 br(),
                 p("That being said, this regression provides a general view of a potential relationship that would need to be explored
                   further. There were some key outliers in the dataset in terms of number of startups and number of higher educational 
                   institutions, like California, New York, South Dakota, and West Virgina. I fit another regression line that does not 
                   that excludes these outliers, as well as ran a nother regression. Here, we see the relationship between educational 
                   institutions and new startups as more 1-to-1.", 
                   align = "center"),
                 br(),
                 fluidRow(
                     column(8, 
                            offset = 2,
                            gt_output("clean_reg_gt"),
                     )
                     ),
                 br(),
                 fluidRow(
                     column(8, 
                            offset = 2,
                            plotOutput("reg_plot"),
                     )
                     )
             )
             ),
    
    # ABOUT PAGE
    
    # formatting the various panels to not be too overwhelming,  using sidebar
    # to create a more readable, adjusting height of sidebar using padding,
    # inserting hyperlinks when necessary
    
    tabPanel("About",
             titlePanel(
                 h1("About This Project", 
                    align = "center")),
             hr(),
             sidebarLayout(
                 sidebarPanel(
                     h3("Project Motivations",
                                 align = "center")),
                 mainPanel(
                 p("After working at a startup last summer, I became interested in how startups functioned 
           and what factors made them successful. I came across various startup datasets online and 
           became excited to see what insights I could find!"))), 
             sidebarLayout(
                 sidebarPanel(
                     h3("Data Sources",
                        align = "center", 
                        style="padding:50px;")),
                 mainPanel(
                     p("I obtained startup data from a Dec. 4, 2015 Crunchbase Data Export found", 
                       a(href = 'https://github.com/notpeter/crunchbase-data', 
                         ' here', .noWS = "outside"),
                         ". Specifically, I used two csv files: companies.csv (information about startups founded betweeen 1977 and 2015) 
                       and investments.csv (records of funding rounds for each startup betweeen 1977 and 2015)."),
                 p("I obtained data on US higher educational institutions from the US Department of Homeland Security found", 
                   a(href = 'https://hifld-geoplatform.opendata.arcgis.com/datasets/colleges-and-universities/data', 
                     ' here', .noWS = "outside"),
                   ". I cleaned up 
                   extraneous columns in excel before loading into R."),
                 p("I also used a", 
                   a(href = 'https://worldpopulationreview.com/states/state-abbreviations/', 
                                      ' dataset from the World Population Review', 
                                      .noWS = "outside"),
                                      " to convert state abbreviations to names"))),
             sidebarLayout(
                 sidebarPanel(
                     h3("Methodology",
                           align = "center")),
                     mainPanel(
                     p("Much of my work went into organizing the data to make the visualization easier. Specifically, I had to separate and rejoin
           columns to organize the startup categories, as startups would often have more than one category (one even had 44). 
           This then enabled me to perform the rest of my analysis."))),
             sidebarLayout(
                 sidebarPanel(
                     h3("Conclusions",
                           align = "center")),
                 mainPanel(
                 p("I learned a lot about what it takes to start and maintain a successful business. Location, industry, and 
           timing are important. While these may seem like common knowledge, it was very eye-opening to generate these 
           insights through the data. By analyzing historical data trends, I was able to see how some industries have changed over
           time as well as how geographical characteristics, like nearby colleges, matter."))),
             sidebarLayout(
                 sidebarPanel(
                     h3("Future Directions",
                        align = "center")),
                 mainPanel(
                 p("There is still a lot that could be done to expand this project. For example, I could examine how regional factors
           (like income or population) impact how many startups are founded, analyze where most startups get their
           funding from by industry, running a regression on how educational institutions impact different industries,
               or analyzing along more local levels than by state lines, to name a few."))),
             hr(),
             h3("Acknowledgements",
                align = "center"),
             p("Special thanks to Preceptor, my TF Kaneesha, and the Gov1005 teaching staff for equipping me
           with the tools to complete this project!", align = "center"),
             h3("About Me",
                align = "center"),
             p("My name is Belinda and I'm a junior studying History of Science. You can reach me at belindahu@college.harvard.edu. 
             I'm interested in startups, specifically relating to consumer products and sustainability!",
               align = "center"),
             p('This is my ', a(href = 'https://github.com/belindahu', 
                                'Github', .noWS = "outside"), 
               '.', 
               .noWS = c("after-begin", "before-end"), align = "center")
             
    ))