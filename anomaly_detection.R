# Load necessary libraries
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RSQLite)
library(shinyjs)

# Increase the maximum upload size
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# Helper function to list SQLite databases in a directory
list_databases <- function(path = ".") {
  files <- list.files(path, pattern = "\\.db$", full.names = TRUE)
  basename(files)
}

# Helper function to list tables in the selected database
list_tables <- function(db_path) {
  db <- dbConnect(SQLite(), dbname = db_path)
  tables <- dbListTables(db)
  dbDisconnect(db)
  tables
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Anomaly Detection"),
  dashboardSidebar(
    useShinyjs(), # Initialize shinyjs
    selectInput("db_select", "Select Database", choices = list_databases()),
    uiOutput("table_selector"),
    actionButton("load_db", "Load Data from DB"),
    uiOutput("wafer_selector"),
    uiOutput("tech_selector"),
    uiOutput("run_selector"),
    uiOutput("lot_selector"),
    uiOutput("machine_recipe_selector"),
    uiOutput("physical_recipe_selector"),
    uiOutput("port_selector"),
    uiOutput("process_op_selector"),
    uiOutput("product_grp_selector"),
    uiOutput("product_selector"),
    uiOutput("recipe_selector"),
    uiOutput("route_selector"),
    verbatimTextOutput("debug_info") # Debugging info
  ),
  dashboardBody(
    fluidRow(
      uiOutput("sensor_plots")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to get the selected database path
  selected_db_path <- reactive({
    req(input$db_select)
    file.path(".", input$db_select)
  })
  
  # Update table list when a database is selected
  observeEvent(input$db_select, {
    tables <- list_tables(selected_db_path())
    updateSelectInput(session, "table_select", "Select Table", choices = tables)
  })
  
  # Render table selector UI
  output$table_selector <- renderUI({
    selectInput("table_select", "Select Table", choices = NULL)
  })
  
  # Reactive expression to retrieve data from the selected database and table
  data <- eventReactive(input$load_db, {
    req(selected_db_path(), input$table_select)
    
    # Disable dropdowns while loading
    shinyjs::disable("wafer_id")
    shinyjs::disable("technology")
    shinyjs::disable("run_id")
    shinyjs::disable("lot_id")
    shinyjs::disable("machine_recipe_id")
    shinyjs::disable("physical_recipe_id")
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    # Create SQLite database connection
    db <- dbConnect(SQLite(), dbname = selected_db_path())
    
    # Query the database to get the data
    df <- dbReadTable(db, input$table_select)
    
    # Filter rows based on DATA_QUALITY
    df <- df %>% filter(DATA_QUALITY > 60)
    
    # Remove rows where the column next to WAFER_ID is NA or empty
    wafer_id_col_index <- which(names(df) == "WAFER_ID")
    df <- df %>% filter(!is.na(df[[wafer_id_col_index + 1]]) & df[[wafer_id_col_index + 1]] != "")
    
    # Remove all columns starting from EventType and after
    event_type_col_index <- which(names(df) == "EventType")
    if (length(event_type_col_index) > 0) {
      df <- df %>% select(1:(event_type_col_index - 1))
    }
    
    # Fill missing values with the median
    df <- df %>%
      mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
    # Close the database connection
    dbDisconnect(db)
    
    # Enable dropdowns after loading
    shinyjs::enable("wafer_id")
    shinyjs::enable("technology")
    shinyjs::enable("run_id")
    shinyjs::enable("lot_id")
    shinyjs::enable("machine_recipe_id")
    shinyjs::enable("physical_recipe_id")
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
    
    df
  })
  
  observeEvent(data(), {
    df <- data()
    
    updateSelectInput(session, "wafer_id", "Select Wafer ID", choices = unique(df$WAFER_ID))
    updateSelectInput(session, "technology", "Select Technology", choices = unique(df$Technology))
  })
  
  observeEvent(input$wafer_id, {
    req(input$wafer_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("technology")
    shinyjs::disable("run_id")
    shinyjs::disable("lot_id")
    shinyjs::disable("machine_recipe_id")
    shinyjs::disable("physical_recipe_id")
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id)
    
    updateSelectInput(session, "technology", "Select Technology", choices = unique(filtered_df$Technology))
    
    # Enable dropdowns after filtering
    shinyjs::enable("technology")
    shinyjs::enable("run_id")
    shinyjs::enable("lot_id")
    shinyjs::enable("machine_recipe_id")
    shinyjs::enable("physical_recipe_id")
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$technology, {
    req(input$wafer_id, input$technology)
    
    # Disable dropdowns while filtering
    shinyjs::disable("run_id")
    shinyjs::disable("lot_id")
    shinyjs::disable("machine_recipe_id")
    shinyjs::disable("physical_recipe_id")
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology)
    
    updateSelectInput(session, "run_id", "Select Run ID", choices = c("All", unique(filtered_df$Run)))
    updateSelectInput(session, "lot_id", "Select LOT ID", choices = c("All", unique(filtered_df$LOT_ID)))
    updateSelectInput(session, "machine_recipe_id", "Select Machine Recipe ID", choices = c("All", unique(filtered_df$MachineRecipeID)))
    updateSelectInput(session, "physical_recipe_id", "Select Physical Recipe ID", choices = c("All", unique(filtered_df$PhysicalRecipeID)))
    updateSelectInput(session, "port_id", "Select Port ID", choices = c("All", unique(filtered_df$PortID)))
    updateSelectInput(session, "process_op_num", "Select Process Op Num", choices = c("All", unique(filtered_df$ProcessOpNum)))
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("run_id")
    shinyjs::enable("lot_id")
    shinyjs::enable("machine_recipe_id")
    shinyjs::enable("physical_recipe_id")
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$run_id, {
    req(input$wafer_id, input$technology, input$run_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("lot_id")
    shinyjs::disable("machine_recipe_id")
    shinyjs::disable("physical_recipe_id")
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & (input$run_id == "All" | Run == input$run_id))
    
    updateSelectInput(session, "lot_id", "Select LOT ID", choices = c("All", unique(filtered_df$LOT_ID)))
    updateSelectInput(session, "machine_recipe_id", "Select Machine Recipe ID", choices = c("All", unique(filtered_df$MachineRecipeID)))
    updateSelectInput(session, "physical_recipe_id", "Select Physical Recipe ID", choices = c("All", unique(filtered_df$PhysicalRecipeID)))
    updateSelectInput(session, "port_id", "Select Port ID", choices = c("All", unique(filtered_df$PortID)))
    updateSelectInput(session, "process_op_num", "Select Process Op Num", choices = c("All", unique(filtered_df$ProcessOpNum)))
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("lot_id")
    shinyjs::enable("machine_recipe_id")
    shinyjs::enable("physical_recipe_id")
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$lot_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("machine_recipe_id")
    shinyjs::disable("physical_recipe_id")
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & (input$run_id == "All" | Run == input$run_id) & (input$lot_id == "All" | LOT_ID == input$lot_id))
    
    updateSelectInput(session, "machine_recipe_id", "Select Machine Recipe ID", choices = c("All", unique(filtered_df$MachineRecipeID)))
    updateSelectInput(session, "physical_recipe_id", "Select Physical Recipe ID", choices = c("All", unique(filtered_df$PhysicalRecipeID)))
    updateSelectInput(session, "port_id", "Select Port ID", choices = c("All", unique(filtered_df$PortID)))
    updateSelectInput(session, "process_op_num", "Select Process Op Num", choices = c("All", unique(filtered_df$ProcessOpNum)))
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("machine_recipe_id")
    shinyjs::enable("physical_recipe_id")
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$machine_recipe_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("physical_recipe_id")
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id))
    
    updateSelectInput(session, "physical_recipe_id", "Select Physical Recipe ID", choices = c("All", unique(filtered_df$PhysicalRecipeID)))
    updateSelectInput(session, "port_id", "Select Port ID", choices = c("All", unique(filtered_df$PortID)))
    updateSelectInput(session, "process_op_num", "Select Process Op Num", choices = c("All", unique(filtered_df$ProcessOpNum)))
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("physical_recipe_id")
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$physical_recipe_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("port_id")
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id) &
                                   (input$physical_recipe_id == "All" | PhysicalRecipeID == input$physical_recipe_id))
    
    updateSelectInput(session, "port_id", "Select Port ID", choices = c("All", unique(filtered_df$PortID)))
    updateSelectInput(session, "process_op_num", "Select Process Op Num", choices = c("All", unique(filtered_df$ProcessOpNum)))
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("port_id")
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$port_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id, input$port_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("process_op_num")
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id) &
                                   (input$physical_recipe_id == "All" | PhysicalRecipeID == input$physical_recipe_id) &
                                   (input$port_id == "All" | PortID == input$port_id))
    
    updateSelectInput(session, "process_op_num", "Select Process Op Num", choices = c("All", unique(filtered_df$ProcessOpNum)))
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("process_op_num")
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$process_op_num, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id, input$port_id, input$process_op_num)
    
    # Disable dropdowns while filtering
    shinyjs::disable("product_grp_id")
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id) &
                                   (input$physical_recipe_id == "All" | PhysicalRecipeID == input$physical_recipe_id) &
                                   (input$port_id == "All" | PortID == input$port_id) &
                                   (input$process_op_num == "All" | ProcessOpNum == input$process_op_num))
    
    updateSelectInput(session, "product_grp_id", "Select Product Grp ID", choices = c("All", unique(filtered_df$ProductGrpID)))
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("product_grp_id")
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$product_grp_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id, input$port_id, input$process_op_num, input$product_grp_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("product_id")
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id) &
                                   (input$physical_recipe_id == "All" | PhysicalRecipeID == input$physical_recipe_id) &
                                   (input$port_id == "All" | PortID == input$port_id) &
                                   (input$process_op_num == "All" | ProcessOpNum == input$process_op_num) &
                                   (input$product_grp_id == "All" | ProductGrpID == input$product_grp_id))
    
    updateSelectInput(session, "product_id", "Select Product ID", choices = c("All", unique(filtered_df$ProductID)))
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("product_id")
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$product_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id, input$port_id, input$process_op_num, input$product_grp_id, input$product_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("recipe_id")
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id) &
                                   (input$physical_recipe_id == "All" | PhysicalRecipeID == input$physical_recipe_id) &
                                   (input$port_id == "All" | PortID == input$port_id) &
                                   (input$process_op_num == "All" | ProcessOpNum == input$process_op_num) &
                                   (input$product_grp_id == "All" | ProductGrpID == input$product_grp_id) &
                                   (input$product_id == "All" | ProductID == input$product_id))
    
    updateSelectInput(session, "recipe_id", "Select RECIPE ID", choices = c("All", unique(filtered_df$RECIPE_ID)))
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("recipe_id")
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$recipe_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id, input$port_id, input$process_op_num, input$product_grp_id, input$product_id, input$recipe_id)
    
    # Disable dropdowns while filtering
    shinyjs::disable("route_id")
    
    df <- data()
    filtered_df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology & 
                                   (input$run_id == "All" | Run == input$run_id) & 
                                   (input$lot_id == "All" | LOT_ID == input$lot_id) & 
                                   (input$machine_recipe_id == "All" | MachineRecipeID == input$machine_recipe_id) &
                                   (input$physical_recipe_id == "All" | PhysicalRecipeID == input$physical_recipe_id) &
                                   (input$port_id == "All" | PortID == input$port_id) &
                                   (input$process_op_num == "All" | ProcessOpNum == input$process_op_num) &
                                   (input$product_grp_id == "All" | ProductGrpID == input$product_grp_id) &
                                   (input$product_id == "All" | ProductID == input$product_id) &
                                   (input$recipe_id == "All" | RECIPE_ID == input$recipe_id))
    
    updateSelectInput(session, "route_id", "Select Route ID", choices = c("All", unique(filtered_df$RouteID)))
    
    # Enable dropdowns after filtering
    shinyjs::enable("route_id")
  })
  
  observeEvent(input$route_id, {
    req(input$wafer_id, input$technology, input$run_id, input$lot_id, input$machine_recipe_id, input$physical_recipe_id, input$port_id, input$process_op_num, input$product_grp_id, input$product_id, input$recipe_id, input$route_id)
    
    # No more dropdowns to disable or enable after route_id
    # But you can handle additional logic here if necessary
  })
  
  filtered_data <- reactive({
    req(input$wafer_id, input$technology)
    df <- data()
    df <- df %>% filter(WAFER_ID == input$wafer_id & Technology == input$technology)
    
    # Apply additional filters based on user selections
    if (input$run_id != "All") {
      df <- df %>% filter(Run == input$run_id)
    }
    if (input$lot_id != "All") {
      df <- df %>% filter(LOT_ID == input$lot_id)
    }
    if (input$machine_recipe_id != "All") {
      df <- df %>% filter(MachineRecipeID == input$machine_recipe_id)
    }
    if (input$physical_recipe_id != "All") {
      df <- df %>% filter(PhysicalRecipeID == input$physical_recipe_id)
    }
    if (input$port_id != "All") {
      df <- df %>% filter(PortID == input$port_id)
    }
    if (input$process_op_num != "All") {
      df <- df %>% filter(ProcessOpNum == input$process_op_num)
    }
    if (input$product_grp_id != "All") {
      df <- df %>% filter(ProductGrpID == input$product_grp_id)
    }
    if (input$product_id != "All") {
      df <- df %>% filter(ProductID == input$product_id)
    }
    if (input$recipe_id != "All") {
      df <- df %>% filter(RECIPE_ID == input$recipe_id)
    }
    if (input$route_id != "All") {
      df <- df %>% filter(RouteID == input$route_id)
    }
    
    df
  })
  
  output$wafer_selector <- renderUI({
    selectInput("wafer_id", "Select Wafer ID", choices = NULL)
  })
  
  output$tech_selector <- renderUI({
    selectInput("technology", "Select Technology", choices = NULL)
  })
  
  output$run_selector <- renderUI({
    selectInput("run_id", "Select Run ID", choices = NULL)
  })
  
  output$lot_selector <- renderUI({
    selectInput("lot_id", "Select LOT ID", choices = NULL)
  })
  
  output$machine_recipe_selector <- renderUI({
    selectInput("machine_recipe_id", "Select Machine Recipe ID", choices = NULL)
  })
  
  output$physical_recipe_selector <- renderUI({
    selectInput("physical_recipe_id", "Select Physical Recipe ID", choices = NULL)
  })
  
  output$port_selector <- renderUI({
    selectInput("port_id", "Select Port ID", choices = NULL)
  })
  
  output$process_op_selector <- renderUI({
    selectInput("process_op_num", "Select Process Op Num", choices = NULL)
  })
  
  output$product_grp_selector <- renderUI({
    selectInput("product_grp_id", "Select Product Grp ID", choices = NULL)
  })
  
  output$product_selector <- renderUI({
    selectInput("product_id", "Select Product ID", choices = NULL)
  })
  
  output$recipe_selector <- renderUI({
    selectInput("recipe_id", "Select RECIPE ID", choices = NULL)
  })
  
  output$route_selector <- renderUI({
    selectInput("route_id", "Select Route ID", choices = NULL)
  })
  
  output$sensor_plots <- renderUI({
    req(filtered_data())
    df <- filtered_data()
    
    # Find the index of WAFER_ID
    wafer_index <- which(colnames(filtered_data()) == "WAFER_ID")
    
    # Filter columns after WAFER_ID
    sensor_columns <- colnames(filtered_data())[(wafer_index + 1):ncol(filtered_data())]
    
    plot_output_list <- lapply(sensor_columns, function(col) {
      plotname <- paste("plot_", col, sep = "")
      plotOutput(plotname, height = 300)
    })
    
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(filtered_data())
    df <- filtered_data()
    
    # Identify sensor columns (adjust based on your dataset)
    sensor_columns <- df %>% select(-TimeStamp, -ToolName, -TOOL_ID, -WAFER_ID, -Technology, -Run, -LOT_ID, -MachineRecipeID, -PhysicalRecipeID, -PortID, -ProcessOpNum, -ProductGrpID, -ProductID, -RECIPE_ID, -RouteID) %>% names()
    
    for (col in sensor_columns) {
      local({
        column <- col
        plotname <- paste("plot_", column, sep = "")
        
        output[[plotname]] <- renderPlot({
          df <- df %>% mutate(z_score = (get(column) - mean(get(column), na.rm = TRUE)) / sd(get(column), na.rm = TRUE))
          df <- df %>% mutate(outlier = abs(z_score) > 3)
          
          ggplot(df, aes_string(x = "TimeStamp", y = column, color = "ToolName")) +
            geom_point(aes(color = ifelse(outlier, "Outlier", "Normal"))) +
            labs(x = "Time", y = column, color = "Legend") +
            scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
            theme_minimal()
        })
      })
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
