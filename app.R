library(shiny)
library(readr)
library(dplyr)

# Define user project file count limit
max_files <- 5

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-input, .selectize-dropdown-content {
        max-width: 100%;
        overflow: hidden;
        white-space: nowrap;
        text-overflow: ellipsis;
      }

      .selectize-input > div.item {
        max-width: calc(100% - 20px); /* Reserve space for dropdown arrow */
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
      
      selectInput("delete_version_select", "Select Version to Delete:", choices = NULL),
      actionButton("delete", "Delete Selected Version")
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
  
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE)
  }
  
  refresh_file_choices <- function() {
    versions <- basename(list.files(user_dir, full.names = TRUE))
    updateSelectInput(session, "version_select", choices = versions)
    updateSelectInput(session, "delete_version_select", choices = versions)
  }
  
  refresh_file_choices()
  
  observeEvent(input$save, {
    if (!dir.exists(user_dir)) {
      dir.create(user_dir, recursive = TRUE)
    }
    
    files <- list.files(user_dir, full.names = TRUE)
    if (length(files) >= max_files) {
      showNotification("Error: Maximum number of files reached.", type = "error", duration = 5)
      return()
    }
    
    if (input$project_name == "") {
      showNotification("Error: Please enter a project name before saving.", type = "error", duration = 5)
      return()
    }
    
    timestamp <- format(Sys.time(), "%y%m%d_%H-%M")
    safe_project_name <- gsub("[^A-Za-z0-9_]+", "_", input$project_name)
    file_path <- paste0(user_dir, timestamp, "_", safe_project_name, ".csv")
    
    # Check if file already exists
    if (file.exists(file_path)) {
      showNotification("A file with the same name already exists. Please wait or use a different project name.", type = "warning", duration = 5)
      return()
    }
    
    # Save settings to CSV
    state <- data.frame(slider1 = input$slider1, slider2 = input$slider2)
    write_csv(state, file_path)
    
    showNotification("Settings saved successfully.", type = "message", duration = 5)
    
    # Refresh dropdown choices after saving
    refresh_file_choices()
  })
  
  
  observeEvent(input$load, {
    selected_file <- paste0(user_dir, input$version_select)
    if (file.exists(selected_file)) {
      saved_state <- read_csv(selected_file)
      updateSliderInput(session, "slider1", value = saved_state$slider1)
      updateSliderInput(session, "slider2", value = saved_state$slider2)
      showNotification("Settings loaded successfully.", type = "message", duration = 5)
    } else {
      showNotification("Error: File could not be loaded.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$delete, {
    req(input$delete_version_select)
    
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
    removeModal()
    
    file_to_delete <- file.path(user_dir, input$delete_version_select)
    if (file.exists(file_to_delete)) {
      file.remove(file_to_delete)
      showNotification("File deleted successfully.", type = "message", duration = 5)
      refresh_file_choices()
    } else {
      showNotification("Error: File could not be deleted.", type = "error", duration = 5)
    }
  })
}

shinyApp(ui = ui, server = server)
