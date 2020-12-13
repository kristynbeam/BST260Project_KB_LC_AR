# Bring in libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(directlabels)
library(zoo)
library(ozmaps)
library(ggthemes)

# Define UI for application 
ui <- fluidPage(
    
    # Change theme to darkly
    theme = shinythemes::shinytheme("darkly"),
    
    # Application title
    titlePanel("Trends in COVID-19 and Other Infectious Diseases in Australian States/Territories from November 2018 to October 2020"),
    
    fluidRow(
        
        sidebarLayout(
            
            sidebarPanel(
                
                selectInput("state",
                            label = "Select a state/territory for plot:",
                            choices = unique(aus_total_high$State),
                            selected = "New South Wales"), # end of state/territory selection
                
                selectInput("disease",
                            label = "Select a disease for plot and map:",
                            choices = levels(factor(aus_total_high$Disease)),
                            selected = "Influenza (laboratory confirmed)"), # end of disease selection
                
                selectInput("policy", 
                            label = "Select a policy restriction category for plot to display date of policy start (black line):",
                            choices = levels(factor(pol_dat_aus$type)),
                            selected = "Restrictions of Mass Gatherings"), # end of policy selection
                
                selectInput("date", 
                            label = "Select a date for plot (green line) and map:",
                            choices = unique(as.Date(aus_total_high$Date)),
                            selected = "2019-07-31"), # end of date selection
                
                radioButtons(inputId="plot_type", 
                             label="Select a variable for plot and map:",
                             choices=c("New cases", 
                                       "New cases per million"),
                             selected = "New cases") # end of variable selection
                
                ), # end sidebarPanel
            
            # Line plot
            mainPanel(
                
                plotOutput("linePlot")
            
                ) # end mainPanel
        
            ), #end sidebarLayout
        
        fluidRow(
            
            column(3,
                   tableOutput("table")
                   ), # end table column
                   
            column(9,
                   plotOutput("mapPlot")
                   ) # end map column
            
            ) # end fluidRow
        
        ) # end fluidRow
    
    ) # end fluidPage

# Define server logic
server <- function(input, output, session) {
    
    # Line plot
    output$linePlot <- renderPlot({
        
        # Count
        if (input$plot_type == "New cases") {
            
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", 
                          input$disease)
            
            # Define state/territory and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$state & type == input$policy)
            
            # Output line plot
            aus_total_high %>% 
                filter(State == input$state & Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Cases, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                geom_vline(xintercept = as.numeric(as.Date(input$date)),
                           color = "green") +
                theme_minimal() +
                labs(x = "Date",
                     y = "New cases") +
                ggtitle(paste("New confirmed cases of COVID-19 and",
                              input$disease,
                              "over time in", 
                              input$state)) +
                scale_x_date(date_breaks = "3 months", 
                             date_labels = "%b %y",
                             limits = as.Date(c("2018-11-30", 
                                                "2020-10-31"))) +
                scale_y_continuous(trans = "log10",
                                   limits = c(1, 
                                              32685)) +
                scale_color_discrete(name = "Disease (log scale):")
            
            # Rate
            } else if (input$plot_type == "New cases per million") {
            
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", 
                          input$disease)
            
            # Define state/territory and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$state & type == input$policy)
            
            # Output line plot
            aus_total_high %>% 
                filter(State == input$state & Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Rates, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                geom_vline(xintercept = as.numeric(as.Date(input$date)),
                           color = "green") +
                theme_minimal() +
                labs(x = "Date",
                     y = "New cases per million") +
                ggtitle(paste("New confirmed cases per million of COVID-19 and",
                              input$disease,
                              "over time in",
                              input$state)) +
                scale_x_date(date_breaks = "3 months", 
                             date_labels = "%b %y",
                             limits = as.Date(c("2018-11-30", 
                                                "2020-10-31"))) +
                scale_y_continuous(trans = "log10",
                                   limits = c(0.2, 
                                              410)) +
                scale_color_discrete(name = "Disease (log scale):")
            
            }
        
        }) # end of line plot
    
    # Table
    output$table <- renderTable({
        
        Abbreviation <- c("ACT", 
                          "NSW", 
                          "NT", 
                          "Qld", 
                          "SA", 
                          "Tas", 
                          "Vic", 
                          "WA")
        
        Region <- c("Australian Capital Territory",
                    "New South Wales",
                    "Northern Territory",
                    "Queensland",
                    "Southern Territory",
                    "Tasmania",
                    "Victoria",
                    "Western Australia")
        table.df <- data.frame(Abbreviation, Region)

        }) # end of table
    
    # Map plot
    output$mapPlot <- renderPlot({
        
        # Count
        if (input$plot_type == "New cases") {
            
           # Rename states/territories
           merge_dat$State <- recode(merge_dat$State, 
                                     "Australian Capital Territory" = "ACT", 
                                     "New South Wales" = "NSW", 
                                     "Northern Territory" = "NT", 
                                     "Queensland" = "Qld", 
                                     "South Australia" = "SA", 
                                     "Tasmania" = "Tas", 
                                     "Victoria" = "Vic", 
                                     "Western Australia" = "WA")
   
            # Output map
            merge_dat %>% 
                group_by(input$state) %>% 
                filter(Disease == input$disease,
                       Date == input$date) %>% 
                ggplot(aes(fill = Cases)) + 
                geom_sf(colour="white") +
                ggtitle(paste("New confirmed cases of", 
                              input$disease,
                              "on", 
                              input$date,
                              "by Australian region")) +
                scale_fill_viridis_c(name = "New cases (log scale):",
                                     trans = "log10",
                                     direction = -1,
                                     limits = c(1, 32685)) +
                geom_sf_label(aes(label = State),
                              nudge_x = 3.5,
                              nudge_y = -0.75) +
                labs(x = "Latitude",
                     y = "Longitude")
            
            # Rate
            } else if (input$plot_type == "New cases per million") {
            
                # Rename states/territories
                merge_dat$State <- recode(merge_dat$State, 
                                          "Australian Capital Territory" = "ACT", 
                                          "New South Wales" = "NSW", 
                                          "Northern Territory" = "NT", 
                                          "Queensland" = "Qld", 
                                          "South Australia" = "SA", 
                                          "Tasmania" = "Tas", 
                                          "Victoria" = "Vic", 
                                          "Western Australia" = "WA")
                
                # Output map
                merge_dat %>% 
                    group_by(input$state) %>% 
                    filter(Disease == input$disease,
                           Date == input$date) %>% 
                    ggplot(aes(fill = Rates)) + 
                    geom_sf(colour="white") +
                    ggtitle(paste("New confirmed cases of", 
                                  input$disease,
                                  "per million on", 
                                  input$date,
                                  "by Australian region")) +
                    scale_fill_viridis_c(name = "New cases per mil (log scale):",
                                         trans = "log10",
                                         direction = -1,
                                         limits = c(0.2, 410)) +
                    geom_sf_label(aes(label = State),
                                  nudge_x = 3.5,
                                  nudge_y = -0.75) +
                    labs(x = "Latitude",
                         y = "Longitude")
            
            }  
        
        }) # end of map plot
    
    } # end of server

# Run the application 
shinyApp(ui = ui, server = server)
