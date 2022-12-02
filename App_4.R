library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("R Built-in Data Sets"),
  sidebarLayout(
    # Define what is going on the side bar panel 
    sidebarPanel(
      # Feature 1: Use selectInput and choices to allow user input to select 
      # which data set they would like to look at 
      selectInput(inputId = "dataset",
                  label = "Please select a dataset:",
                  choices = c("mtcars", "iris", "ToothGrowth", "PlantGrowth", 
                              "USArrests")),
      # Feature 2: use numericInput to allow user input to select how many 
      # observations are being shown 
      numericInput(inputId = "obs",
                   label = "Number of observations displayed:",
                   value = 5),
      
      #NEW FEATURE 1: The sidebar panel will feature a small note which will 
      # specify how the summary display is not impacted by the selected number
      # of observations 
      helpText("The Summary display will be based on the full dataset while the 
               Observations display will only show the number of observations 
               specified (i.e. The selected number of observations will not 
               affect the Summary display). Similarly, the Download button will 
               download the entire selected data set as a .csv file"),
      
      # NEW FEATURE 2: Use actionButton to allow user input to determine when 
      # the summary table and observations will display. There will be a button
      # for users to click on to update the Summary and Observation display. 
      actionButton("update", "Update Display"),
      
      # NEW FEATURE 3: Use downloadButton to allow user an option to download 
      # the Observation display table as a csv file 
      downloadButton("downloadData", "Download")
      
    ),
    # Define what is going on the main panel 
    mainPanel(
      # Feature 3: Use verbatimTextOutput to output an interactive summary table 
      # which corresponds to the selected data set. # Use tableOutput to ensure
      # there is an output table for the selected data set  
      
      # NEW FEATURE 1: Use h4 to create a header for the summary and 
      # observations display to help reference the 'helpText' message on the 
      # sidebar panel
      h4("Summary"),
      verbatimTextOutput("summary"),
      h4("Observations"),
      tableOutput("view")
    )
  )
)

server <- function(input, output) {
  # use eventReactive (this is different than the original app, since we need to 
  # adjust the code for the "Update View" button) and switch to ensure the 
  # correct inputted data set is returned call it InputDataset 
  InputDataset <- eventReactive(input$update, {
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris,
           "ToothGrowth" = ToothGrowth,
           "PlantGrowth" = PlantGrowth,
           "USArrests" = USArrests)
  }, ignoreNULL=FALSE)
  # Use renderPrint and summary to print the summary table 
  output$summary <- renderPrint({
    dataset <- InputDataset()
    summary(dataset)
  })
  
  # use renderTable and head to output the table for the corresponding data set
  # ensure to set n = input$obs so the number of observations depends on the 
  # user input 
  output$view <- renderTable({
    head(InputDataset(), n = isolate(input$obs))
  })
  
  # Use downloadHandler to ensure the selected data set is downloaded as a csv 
  # file when "Download" button is selected 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file){
      write.csv(InputDataset(), file, row.names = FALSE)
    }
  )
  
}

# Load the app 
shinyApp(ui = ui, server = server)
