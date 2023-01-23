library(shiny)
library(tidyverse)

ui <- fluidPage(
  sidebarPanel(
    numericInput("x", "Enter Value of X", 1),
    numericInput("y", "Enter Value of Y", 1),
    actionButton("add_data", "Add Data", width = "100%")
  ),
  mainPanel(
    tableOutput("xy_Table")
  )
)

server <- function(input, output, session) {
  xyTable <- reactiveVal(
    tibble(x = numeric(), y = numeric())
  )
  
  observeEvent(input$add_data, {
    xyTable() %>%
      add_row(
        x = input$x,
        y = input$y,
      ) %>%
      xyTable()
  })
  
  output$xy_Table <- renderTable(xyTable())
}

shinyApp(ui, server)