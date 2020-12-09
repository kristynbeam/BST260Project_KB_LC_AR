
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Impact of Policy Changes on COVID and other Infectious Diseases in Australia"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("province",
                        label = "Select a Province:",
                        choices = aus_total$State,
                        selected = "NSW"), #end of province selection
            selectInput("disease",
                        label = "Select a Disease:",
                        choices = aus_total$Disease,
                        selected = "Influenza (laboratory confirmed)"), #end of Disease selection
           selectInput("policy", 
                       label = "Select a Policy:",
                       choices = pol_dat_aus$type,
                       selected = "Quarantine") #end of Policy selection
            
        ), #end sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot")
        ) #end main Panel
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlot({
        aus_total_covid %>% 
            filter(State == input$province) %>% 
            ggplot(aes(Date, Rates)) +
            geom_line(color = "blue") +
            ggtitle(paste("Infectious Disease Rates in", input$province)) +
            #ylim(0, 40) +
            theme_minimal() +
            geom_line(data = aus_total %>% 
                          filter(Disease == input$disease & State == input$province), 
                      aes(Date, Rates), color = "red") #+
            #geom_vline(data = pol_dat_aus %>% 
             #            filter(province = input$province),
              #         xintercept = as.numeric(as.Date("2020-03-16")
        
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
