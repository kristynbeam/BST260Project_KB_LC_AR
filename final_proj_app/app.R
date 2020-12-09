
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(directlabels)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Impact of Policy Changes on COVID and other Infectious Diseases in Australia"),
    
    fluidRow(
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("province",
                      label = "Select a Province:",
                      choices = aus_total$State,
                      selected = "ACT"), #end of province selection
          selectInput("disease",
                      label = "Select a Disease (n = 42):",
                      choices = aus_total_high$Disease,
                      selected = "Hepatitis B (unspecified)"), #end of Disease selection
          selectInput("policy", 
                      label = "Select a Policy:",
                      choices = c("Quarantine", "Restrictions of Mass Gatherings", "Social Distancing", 
                                  "Closure and Regulation of Schools", "Lockdown", "Hygiene"),
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
        aus_total_covid %>% 
            filter(State == input$province) %>% 
            ggplot(aes(Date, Rates)) +
            geom_line(color = "blue") +
            ggtitle(paste("Infectious Disease Rates per 100,000 in", input$province)) +
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
