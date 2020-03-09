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

diffDta <- readr::read_csv('../data/DiffYearAttitudeTowardsWifeBeating.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Exploring Attitudes Towards Wife Beating"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        h3('Control for all'),
        selectInput('dem',
                    'Choose Demographic Key',
                    unique(dta$`Characteristic Category`)),
        hr(),
        h3('Control for trends'),
        selectInput('country',
                    'Choose country of interest',
                    unique(dta$`Country Name`)),

        hr(),
        h3('Control for delta'),
        selectInput('indicator',
                    'Choose Indicator',
                    unique(diffDta$Indicator)),
        selectInput('lab',
                    'Choose Demographic Section',
                    unique(dplyr::filter(diffDta, `Characteristic Category` == 'Education')$`Characteristic Label`)),
        selectInput('gender',
                    'Choose Gender',
                    unique(diffDta$gender))
      ),

      # Show a plot of the generated distribution
      mainPanel(
        h3('Trends'),
         plotOutput("countryPlot"),
        br(),
        hr(),
        h3('Delta'),
         plotOutput('diffPlot')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

   output$countryPlot <- renderPlot({
     dta %>%
       dplyr::filter(`Characteristic Category` == input$dem,
                     `Country Name` == input$country) %>%
       ggplot() +
       geom_line(aes(x = `Survey Year`, y = Value, color = gender)) +
       facet_grid(`Characteristic Label`~Indicator)
   })

   observe({
     dem <- input$dem
     updateSelectInput(session, "lab",
                       choices = unique(dplyr::filter(diffDta, `Characteristic Category` == dem)$`Characteristic Label`))
   })

   output$diffPlot <- renderPlot({

     interest <- diffDta %>%
       dplyr::filter(
         Indicator == input$indicator,
         `Characteristic Category` == input$dem,
         `Characteristic Label` == input$lab,
         gender == input$gender
       ) %>%
       dplyr::arrange(desc(diff))

     interestTop <- head(interest, 5)
     interestBottom <- tail(interest, 5)

     interest <- rbind(interestTop, interestBottom) %>%
       dplyr::mutate(`Country Name` = factor(`Country Name`, `Country Name`))

     ggplot(interest) +
       geom_segment(aes(x = first,
                        xend = last,
                        y = `Country Name`,
                        yend = `Country Name`,
                        color = delta), size = 2, arrow = arrow(length = unit(0.03, "npc"))) +
       labs(x = 'Difference in Acceptance',
            y = 'Country',
            title = paste0('Delta of Acceptance of Most First and Last Surveys'),
            subtitle = paste0(input$dem, ' ', input$lab, ' ', input$gender))
   })

}

# Run the application
shinyApp(ui = ui, server = server)

