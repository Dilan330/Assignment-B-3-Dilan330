library(shiny)
library(tidyverse)

ui <- fluidPage(
    titlePanel("R Built-in Data Sets"),
    sidebarLayout(
      # Define what is going on the side bar panel 
        sidebarPanel(
      # Feature 1: Use selectInput and choices to allow user input to select 
          # which dataset they would like to look at 
      selectInput(inputId = "dataset",
                  label = "Please select a dataset:",
                  choices = c("mtcars", "iris", "ToothGrowth", "PlantGrowth", "USArrests")),
      # Feature 2: use numericInput to allow user input to select how many 
      # observations are being shown 
            numericInput(inputId = "obs",
                   label = "Number of observations displayed:",
                   value = 5)
    ),
    # Define what is going on the main panel 
    mainPanel(
      # Feature 3: Use verbatimTextOutput to output an interactive summary table 
      # which corresponds to the selected dataset. # Use tableOutput to ensure
      # there is an output table for the selected dataset  
      verbatimTextOutput("summary"),
      tableOutput("view")
    )
  )
)

server <- function(input, output) {
  # use reactive and switch to ensure the correct inputted dataset is returned 
  # call it InputDataset 
  InputDataset <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris,
           "ToothGrowth" = ToothGrowth,
           "PlantGrowth" = PlantGrowth,
           "USArrests" = USArrests)
  })
  # Use renderPrint and summary to print the summary table 
  output$summary <- renderPrint({
    dataset <- InputDataset()
    summary(dataset)
  })
  
  # use renderTable and head to output the table for the corresponding dataset
  # ensure to set n = input$obs so the number of observations depends on the 
  # user input 
  output$view <- renderTable({
    head(InputDataset(), n = input$obs)
  })
  
}

# Load the app 
shinyApp(ui = ui, server = server)
