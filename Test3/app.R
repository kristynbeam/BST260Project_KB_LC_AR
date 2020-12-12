
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(directlabels)
library(zoo)
library(ozmaps)

# Bring in data

# We have to name a relative file path instead of an absolute one. It all depends on the directory we are working in. 
# When they clone our github and run our code, they will be located in the "final_proj_app" to run the shinyapp
# In order to get them into the correct folder to load the data files, we have to add the "../" before
# the code in order to direct them to the correct folder.
# As long as all of our data files are saved into the project_data_files folder, then this should work for the 
# shiny app purposes.

# aus_total_high = read.csv("../project_data_files/aus_total_high.csv")
# pol_dat_aus = read.csv("../project_data_files/pol_dat_aus.csv")
# aus_total_covid = read.csv("../project_data_files/aus_total_covid.csv")

# Define UI for application 
ui <- fluidPage(
    
    # Change theme to darkly
    theme = shinythemes::shinytheme("darkly"),
    
    # Application title
    titlePanel("Trends in COVID-19 and Other Infectious Diseases in Australian States/Territories from 2019-2020"),
    
    fluidRow(
        
        sidebarLayout(
            sidebarPanel(
                selectInput("state",
                            label = "Select a state/territory for plot:",
                            choices = unique(aus_total_high$State),
                            selected = "Australian Capital Territory"), # end of state selection
                selectInput("disease",
                            label = "Select a disease for plot and map:",
                            choices = levels(factor(aus_total_high$Disease)),
                            selected = "Hepatitis B (unspecified)"), # end of disease selection
                selectInput("policy", 
                            label = "Select a policy restriction category for plot to display date of policy start (black line):",
                            choices = levels(factor(pol_dat_aus$type)),
                            selected = "Restrictions of Mass Gatherings"), # end of policy selection
                selectInput("date", 
                            label = "Select a date for plot (green line) and map:",
                            choices = aus_total_high$Date,
                            selected = "2019-01-01"), # end of date selection
                radioButtons(inputId="plot_type", 
                             label="Select a variable for plot and map:",
                             choices=c("New cases", 
                                       "New cases per million"),
                             selected = "New cases"), # end of y-axis selection
                
            ), #end sidebarPanel
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("linePlot")
            ) #end main Panel
        ),#end of fluid Row
        
        fluidRow(
            column(4,
                   
            ),
            column(8,
                   plotOutput("mapPlot")
            )#, #end column
            
        ) #end fluidRow
        
        
    )
) # end UI

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$linePlot <- renderPlot({
        
             if (input$plot_type=="New cases") {
            
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", 
                          input$disease)
            
            # Define province and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$state & type == input$policy)
            
            aus_total_high %>% 
                filter(State == input$state & Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Cases, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                geom_vline(xintercept = as.numeric(as.Date(input$date)),
                           color = "green")+
                theme_minimal() +
                labs(x = "Date",
                     y = "New cases") +
                ggtitle(paste("New confirmed cases over time in", 
                              input$state)) +
                scale_x_date(date_breaks = "3 months", 
                             date_labels = "%b %y")
            
        } else if (input$plot_type=="New cases per million") {
            
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", 
                          input$disease)
            
            # Define province and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$state & type == input$policy)
            
            aus_total_high %>% 
                filter(State == input$state & Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Rates, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                geom_vline(xintercept = as.numeric(as.Date(input$date)),
                           color = "green")+
                theme_minimal() +
                labs(x = "Date",
                     y = "New cases per million") +
                ggtitle(paste("New confirmed cases per million over time in", 
                              input$state)) +
                scale_x_date(date_breaks = "3 months", 
                             date_labels = "%b %y")
            
        }  
    }) #end of plot
    
    
    
    
    #output$regression <- renderTable({
    
    #}) #output of regression analysis
    
    
    
    
    # Map plot
    
    # Need to make sure we write our final dataset as a csv to the folder and then read it in at the top
    
    output$mapPlot <- renderPlot({
        
       if (input$plot_type == "New cases") {
            
            merge_dat %>% 
                group_by(input$state) %>% 
                filter(Disease == input$disease,
                       Date == input$date) %>% 
                ggplot(aes(fill = Cases)) + 
                geom_sf() +
                ggtitle(paste("New confirmed cases of", 
                              input$disease,
                              "on", 
                              input$date,
                              "by Australian state/territory")) +
                scale_fill_viridis_c(name = "New cases (sqrt scale)",
                                     trans = "sqrt",
                                     direction = -1)
            
        } else if (input$plot_type == "New cases per million") {
            
            merge_dat %>% 
                group_by(input$state) %>% 
                filter(Disease == input$disease,
                       Date == input$date) %>% 
                ggplot(aes(fill = Rates)) + 
                geom_sf() +
                ggtitle(paste("New confirmed cases of", 
                              input$disease,
                              "per million on", 
                              input$date,
                              "by Australian state/territory")) +
                scale_fill_viridis_c(name = "New cases per mil (sqrt scale)",
                                     trans = "sqrt",
                                     direction = -1)
            
        }  
    }) #end of map plot
    
}

# Run the application 
shinyApp(ui = ui, server = server)
