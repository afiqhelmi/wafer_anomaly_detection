#Change Register
#20240520 - AfiqHelmi Initial Changes

# Load necessary libraries
library(shiny)
library(DT)
library(RSQLite)

# Define UI for application that uploads CSV and displays table
ui <- fluidPage(
  
  # Application title
  titlePanel("Upload CSV File"),
  
  # Sidebar with a file input
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      textOutput("table_status")
    ),
    
    # Show a table of the uploaded CSV file
    mainPanel(
      DTOutput("db_data")
    )
  )
)

# Define server logic to read selected file and create table
server <- function(input, output, session) {
  
  # Create SQLite database connection
  db <- dbConnect(SQLite(), dbname = "anomalydetection.db")
  
  # Observe when a file is uploaded
  observe({
    req(input$file1)
    
    # Read the CSV data
    data <- read.csv(input$file1$datapath, header = TRUE)
    
    # Create or replace the "sensor_data" table in the SQLite database
    dbWriteTable(db, "sensor_data", data, overwrite = TRUE)
    
    # Fetch data from the database
    dbData <- dbReadTable(db, "sensor_data")
    
    # Render the table from the database
    output$db_data <- renderDT({
      datatable(dbData)
    })
    
    # Update table status
    output$table_status <- renderText({
      if (!is.null(dbData)) {
        return("Table 'sensor_data' has been created and data is loaded successfully from the database.")
      } else {
        return("Failed to create the table.")
      }
    })
  })
  
  # Close the database connection when the session ends
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
