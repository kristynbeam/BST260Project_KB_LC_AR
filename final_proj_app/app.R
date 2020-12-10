
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(directlabels)

# Bring in data
#aus_total <- read.csv("aus_total.csv")
#aus_total_high <- read.csv("aus_total_high.csv")
#pol_dat_aus <- read.csv("pol_dat_aus.csv")
aus_total_high<- full_join(aus_total_high, aus_total_covid)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Change theme to darkly
    theme = shinythemes::shinytheme("darkly"),

    # Application title
    titlePanel("Impact of Policy Restrictions on Rates of COVID-19 and other Infectious Diseases in Australian Provinces in 2020"),
    
    fluidRow(
      # Sidebar with a slider input for number of bins 
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
                      selected = "Quarantine") #end of Policy selection
          
        ), #end sidebarPanel
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("linePlot")
        ) #end main Panel
    ),#end of fluid Row
    
    fluidRow(
      column(4,
             tableOutput("table"))#, #end column
      #column(8,
             #tableOutput("regression")) #end column (can put regression output here)
    ) #end fluidRow

    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlot({
      
      # Define province and policy type
      policies <- pol_dat_aus %>%
        filter(province == input$province & type == input$policy)
                aus_total_high %>% 
            filter(State == input$province & 
                     Date >= "2020-01-01" & 
                     Disease %in% c(input$disease, "COVID-19")) %>% 
            ggplot(aes(x = Date, 
                       y = Rates)) +
            geom_line(aes(x = Date, 
                          y = Rates, color = Disease),
                      alpha = 0.5) +
           # geom_vline(data = pol_dat_aus %>% 
            #           alpha = 0.5, 
             #          xintercept = as.numeric(as.Date(pol_dat_aus$date_start)), 
              #         color = "blue") +
            labs(x = "Date",
                 y = "Rate") +
            ggtitle(paste("Infectious Disease Rates per 100,000 in", 
                          input$province)) +
            scale_x_date(date_breaks = "1 month", 
                         date_labels = "%b") +
        
            
            geom_dl(aes(label = "COVID-19"), method = "top.qp") +
            #ylim(0, 40) +
            theme_minimal() +
            geom_line(data = aus_total_high %>% 
                          filter(Disease == input$disease & State == input$province), 
                      aes(Date, Rates), color = "red") 
            #geom_vline(data = pol_dat_aus %>% 
             #            filter(province = input$province),
              #         xintercept = as.numeric(as.Date("2020-03-16")
      

      
      
    
    })
      output$table <- renderTable({
        Abbreviation <- c("ACT", "NSW", "NT", "Qld", "SA", "Tas", "Vic", "WA")
        Country <- c("Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland", 
                     "Southern Territory", "Tasmania", "Victoria", "Western Australia")
        table.df <- data.frame(Abbreviation, Country)
        
      }) #end of table
      
      #output$regression <- renderTable({
        
      #}) #output of regression analysis

    
}

# Run the application 
shinyApp(ui = ui, server = server)
