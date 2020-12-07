
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
           selectInput("policy", 
                       label = "Select a Policy:",
                       choices = pol_dat_aus$type,
                       selected = "Quarantine"), #end of Policy selection
           selectInput("disease",
                      label = "Select a Disease:",
                      choices = aus_total$Disease,
                      selected = "Influenza (laboratory confirmed)") #end of Disease selection
            
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
        aus_total %>%
            filter(State == input$province & Date >= "2020-01-01" & Disease %in% c("Influenza (laboratory confirmed)", "COVID-19")) %>%
            ggplot(aes(Date, Rates)) +
            geom_line(aes(color = Disease), 
                      alpha = 0.5) +
            # 2020-03-16 is date of first policy restriction in NSW (restriction of mass gatherings)
            #geom_vline(alpha = 0.5, 
             #          xintercept = as.numeric(as.Date("2020-03-16")), 
              #         color = "blue") +
            labs(x = "Date",
                 y = "Rate") +
            ggtitle("Covid rates in NSW over time") +
            scale_x_date(date_breaks = "1 month", 
                         date_labels = "%b")

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
