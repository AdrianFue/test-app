library(shiny)
library(readr)
library(dplyr)

# Define user project file count limit
max_files <- 5

ui <- fluidPage(
  textInput("project_name", "Project Name:", value = ""),
  sliderInput("slider1", "Slider 1:", min = 0, max = 100, value = 50),
  sliderInput("slider2", "Slider 2:", min = 0, max = 100, value = 25),
  actionButton("save", "Save Settings"),
  selectInput("version_select", "Select Version to Load:", choices = NULL),
  actionButton("load", "Load Selected Version"),
  verbatimTextOutput("status")
)

server <- function(input, output, session) {
  user_id <- Sys.getenv("USER")
  file_dir <- "/srv/shiny-app-data/user-states/test-app/"
  
  # Function to get user files
  user_files <- reactive({
    list.files(file_dir, pattern = paste0("^", user_id, "_"), full.names = TRUE)
  })
  
  observe({
    versions <- basename(user_files())
    updateSelectInput(session, "version_select", choices = versions)
  })
  
  observeEvent(input$save, {
    # Create the user's directory if it doesn't exist
    if (!dir.exists(user_dir)) {
      dir.create(user_dir, recursive = TRUE)
    }
    
    files <- user_files()
    # Check number of files
    if (length(files) >= max_files) {
      output$status <- renderText("Error: Maximum number of files reached.")
      return()
    }
    # Validate project name input
    if (input$project_name == "") {
      output$status <- renderText("Error: Please enter a project name before saving.")
      return()
    }
    
    # Create new filename with project name and timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H-%M-%S")
    safe_project_name <- gsub("[^A-Za-z0-9_]", "_", input$project_name)  # Remove special characters
    file_path <- paste0(file_dir, user_id, "_", safe_project_name, "_", timestamp, ".csv")
    
    # Save settings to CSV
    state <- data.frame(slider1 = input$slider1, slider2 = input$slider2)
    write_csv(state, file_path)
    output$status <- renderText("Settings saved successfully.")
  })
  
  observeEvent(input$load, {
    selected_file <- paste0(file_dir, input$version_select)
    if (file.exists(selected_file)) {
      saved_state <- read_csv(selected_file)
      updateSliderInput(session, "slider1", value = saved_state$slider1)
      updateSliderInput(session, "slider2", value = saved_state$slider2)
    }
  })
}

shinyApp(ui = ui, server = server)
