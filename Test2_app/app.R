
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(directlabels)
library(zoo)

# Bring in data

# We have to name a relative file path instead of an absolute one. It all depends on the directory we are working in. 
#When they clone our github and run our code, they will be located in the "final_proj_app" to run the shinyapp
#In order to get them into the correct folder to load the data files, we have to add the "../" before
#the code in order to direct them to the correct folder.
#As long as all of our data files are saved into the project_data_files folder, then this should work for the 
#shiny app purposes.
aus_total_high = read.csv("../project_data_files/aus_total_high.csv")
pol_dat_aus = read.csv("../project_data_files/pol_dat_aus.csv")
aus_total_covid = read.csv("../project_data_files/aus_total_covid.csv")

aus_total_high$Date <- as.Date(aus_total_high$Date)

aus_total_high<-aus_total_high %>%group_by (State) %>% arrange(Date) %>% 
    mutate (Cases_7dayavg = rollmean(Cases, k = 7, fill = NA))%>% mutate (Rates_7dayavg= rollmean(Rates, k = 7, fill = NA))

# Define UI for application 
ui <- fluidPage(
    
    # Change theme to darkly
    theme = shinythemes::shinytheme("darkly"),
    
    # Application title
    titlePanel("Impact of Policy Restrictions on Rates of COVID-19 and other Infectious Diseases in Australian Provinces in 2020"),
    
    fluidRow(
        
        sidebarLayout(
            sidebarPanel(
                selectInput("province",
                            label = "Select a province:",
                            choices = aus_total_high$State,
                            selected = "Australian Capital Territory"), #end of province selection
                selectInput("disease",
                            label = "Select a disease:",
                            choices = aus_total_high$Disease,
                            selected = "Hepatitis B (unspecified)"), #end of Disease selection
                selectInput("policy", 
                            label = "Select a policy restriction type:",
                            choices = pol_dat_aus$type,
                            selected = "Restrictions of Mass Gatherings"),
                radioButtons(inputId="plot_type", 
                             label="Select variable to display",
                             choices=c("Cases", "Rates", "7-day average of Cases", "7-day average of Rates"),
                             selected = "Cases"),#end of Policy selection
                
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
        
        if(input$plot_type=="7-day average of Cases"){
            
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", input$disease)
            
            # Define province and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$province & type == input$policy)
            
            aus_total_high %>% 
                filter(State == input$province & 
                           Date >= "2020-01-01" & 
                           Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Cases_7dayavg, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(alpha = 0.5, 
                           xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                #geom_dl(aes(label = "COVID-19"), method = "top.qp") +
                theme_minimal() +
                #geom_line(data = aus_total_high %>% 
                #         filter(Disease == input$disease & State == input$province), 
                #        aes(Date, Rates), color = "red") +
                #geom_vline(data = pol_dat_aus %>% 
                #         filter(province == input$province),
                #      xintercept = as.numeric(as.Date(pol_dat$date_start))) +
                labs(x = "Date",
                     y = "7-day Average of Cases") +
                ggtitle(paste("Infectious Disease 7-day average Cases in", 
                              input$province)) +
                scale_x_date(date_breaks = "1 month", 
                             date_labels = "%b") 
            
        } else if (input$plot_type=="7-day average of Rates"){
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", input$disease)
            
            # Define province and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$province & type == input$policy)
            
            aus_total_high %>% 
                filter(State == input$province & 
                           Date >= "2020-01-01" & 
                           Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Rates_7dayavg, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(alpha = 0.5, 
                           xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                #geom_dl(aes(label = "COVID-19"), method = "top.qp") +
                theme_minimal() +
                #geom_line(data = aus_total_high %>% 
                #         filter(Disease == input$disease & State == input$province), 
                #        aes(Date, Rates), color = "red") +
                #geom_vline(data = pol_dat_aus %>% 
                #         filter(province == input$province),
                #      xintercept = as.numeric(as.Date(pol_dat$date_start))) +
                labs(x = "Date",
                     y = "7-day average of Rates") +
                ggtitle(paste("Infectious Disease 7-day average of Rates per 100,000 in", 
                              input$province)) +
                scale_x_date(date_breaks = "1 month", 
                             date_labels = "%b") 
        } else if(input$plot_type=="Cases"){
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", input$disease)
            # Define province and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$province & type == input$policy)
            aus_total_high %>% 
                filter(State == input$province & 
                           Date >= "2020-01-01" & 
                           Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Cases, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(alpha = 0.5, 
                           xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                #geom_dl(aes(label = "COVID-19"), method = "top.qp") +
                theme_minimal() +
                #geom_line(data = aus_total_high %>% 
                #         filter(Disease == input$disease & State == input$province), 
                #        aes(Date, Rates), color = "red") +
                #geom_vline(data = pol_dat_aus %>% 
                #         filter(province == input$province),
                #      xintercept = as.numeric(as.Date(pol_dat$date_start))) +
                labs(x = "Date",
                     y = "Cases") +
                ggtitle(paste("Infectious Disease Cases in", 
                              input$province)) +
                scale_x_date(date_breaks = "1 month", 
                             date_labels = "%b") 
            
            
        } else if(input$plot_type=="Rates"){
            # Define diseases to include in line plot as COVID-19 and user-selected disease
            diseases <- c("COVID-19", input$disease)
            
            # Define province and policy type
            pol_dat <- pol_dat_aus %>%
                filter(province == input$province & type == input$policy)
            
            aus_total_high %>% 
                filter(State == input$province & 
                           Date >= "2020-01-01" & 
                           Disease %in% diseases) %>% 
                ggplot(aes(x = Date, 
                           y = Rates, 
                           color = Disease)) +
                geom_line(aes(color = Disease)) +
                geom_vline(alpha = 0.5, 
                           xintercept = as.numeric(as.Date(pol_dat$date_start)),
                           color = "black") +
                #geom_dl(aes(label = "COVID-19"), method = "top.qp") +
                theme_minimal() +
                #geom_line(data = aus_total_high %>% 
                #         filter(Disease == input$disease & State == input$province), 
                #        aes(Date, Rates), color = "red") +
                #geom_vline(data = pol_dat_aus %>% 
                #         filter(province == input$province),
                #      xintercept = as.numeric(as.Date(pol_dat$date_start))) +
                labs(x = "Date",
                     y = "Rates") +
                ggtitle(paste("Infectious Disease Rates per 100,000 in", 
                              input$province)) +
                scale_x_date(date_breaks = "1 month", 
                             date_labels = "%b") 
            
        }  
    }) #end of plot

    #output$regression <- renderTable({
    
    #}) #output of regression analysis
    
    
    
    #Map plot
    output$mapPlot <- renderPlot({
        #Need to make sure we write our final dataset as a csv to the folder and then read it in at the top
        merge_dat %>% 
            group_by(input$province) %>% 
            filter(Disease == input$disease) %>% 
            ggplot(aes(fill = log(Rates))) + geom_sf() +
            ggtitle(paste(input$disease), "Rate Across all Provinces in Australia")
    })#end of map plot
    
}

# Run the application 
shinyApp(ui = ui, server = server)
