#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

dta <- readr::read_csv('../data/YearlyAttitudeTowardsWifeBeating.csv') %>%
  dplyr::mutate(Indicator = stringr::str_remove(Indicator, '^if she |^for '))

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Exploring Attitudes Towards Wife Beating"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput('country',
                    'Choose country of interest',
                    unique(dta$`Country Name`)),
        selectInput('dem',
                    'Choose Demographic Key',
                    unique(dta$`Characteristic Category`))
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("countryPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$countryPlot <- renderPlot({
     dta %>%
       dplyr::filter(`Characteristic Category` == input$dem,
                     `Country Name` == input$country) %>%
       ggplot() +
       geom_line(aes(x = `Survey Year`, y = Value, color = gender)) +
       facet_grid(`Characteristic Label`~Indicator)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

