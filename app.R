library(shiny)
library(readr)
library(dplyr)

# Define user project file count limit
max_files <- 5

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
.selectize-input {
  padding-right: 20%; /* Adds padding to prevent text from going behind the arrow */
  overflow: hidden;
  white-space: nowrap;
  text-overflow: ellipsis; /* Add ellipsis to overflow */
}

.selectize-dropdown-content {
  overflow: hidden;
  white-space: nowrap;
  text-overflow: ellipsis;
}
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("project_name", "Project Name:", value = ""),
      actionButton("save", "Save Settings"),
      
      selectInput("version_select", "Select Version to Load:", choices = NULL),
      actionButton("load", "Load Selected Version"),
      
      # Dropdown for selecting a version to delete and delete button
      selectInput("delete_version_select", "Select Version to Delete:", choices = NULL),
      actionButton("delete", "Delete Selected Version"),
      
      verbatimTextOutput("status")
    ),
    mainPanel(
      sliderInput("slider1", "Slider 1:", min = 0, max = 100, value = 50),
      sliderInput("slider2", "Slider 2:", min = 0, max = 100, value = 25)
    )
  )
)

server <- function(input, output, session) {
  file_dir <- "/srv/shiny-app-data/user-states/test-app/"
  user_id <- session$user
  user_dir <- paste0(file_dir, user_id, "/")
  
  # Create the user's directory if it doesn't exist
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE)
  }
  
  # Function to refresh dropdown choices with current files in the user's directory
  refresh_file_choices <- function() {
    versions <- basename(list.files(user_dir, full.names = TRUE))
    updateSelectInput(session, "version_select", choices = versions)
    updateSelectInput(session, "delete_version_select", choices = versions)
  }
  
  # Initialize dropdown choices when app loads
  refresh_file_choices()
  
  observeEvent(input$save, {
    files <- list.files(user_dir, full.names = TRUE)
    
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
    safe_project_name <- gsub("[^A-Za-z0-9_]+", "_", input$project_name)  # Remove special characters
    file_path <- paste0(user_dir, timestamp, "_", safe_project_name, ".csv")
    
    # Save settings to CSV
    state <- data.frame(slider1 = input$slider1, slider2 = input$slider2)
    write_csv(state, file_path)
    output$status <- renderText("Settings saved successfully.")
    
    # Refresh dropdown choices after saving
    refresh_file_choices()
  })
  
  observeEvent(input$load, {
    selected_file <- paste0(user_dir, input$version_select)
    if (file.exists(selected_file)) {
      saved_state <- read_csv(selected_file)
      updateSliderInput(session, "slider1", value = saved_state$slider1)
      updateSliderInput(session, "slider2", value = saved_state$slider2)
    }
  })
  
  observeEvent(input$delete, {
    req(input$delete_version_select) # Ensure a file is selected
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete the file:", input$delete_version_select, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    removeModal() # Hide confirmation dialog
    
    # Delete the selected file securely
    file_to_delete <- file.path(user_dir, input$delete_version_select)
    if (file.exists(file_to_delete)) {
      file.remove(file_to_delete)
      output$status <- renderText("File deleted successfully.")
      
      # Refresh dropdown choices after deletion
      refresh_file_choices()
    } else {
      output$status <- renderText("Error: File could not be deleted.")
    }
  })
  
}

shinyApp(ui = ui, server = server)
