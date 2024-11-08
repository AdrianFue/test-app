library(shiny)
library(readr)
library(dplyr)

# Define user project file count limit
max_files <- 5

# Define list of admin users
admin_users <- c("Adrian", "Prajna")  # Replace with actual admin usernames

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
        max-width: calc(100% - 20px);
        overflow: hidden;
        white-space: nowrap;
        text-overflow: ellipsis;
      }
      /* Admin panel styling */
      .admin-panel {
        border: 2px solid #d9534f; /* Bootstrap danger color */
        padding: 15px;
        margin-bottom: 20px;
        background-color: #f9f2f4; /* Light red background */
        border-radius: 5px;
      }
      .admin-panel h4 {
        color: #d9534f; /* Matching color for title */
        margin-top: 0;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Conditional admin dropdown for selecting a user folder
      uiOutput("admin_user_select"),
      
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
  file_dir <- if (Sys.info()[["sysname"]] == "Windows") {
    "user-states/test-app/"  # Use Windows path for local testing
  } else {
    "/srv/shiny-app-data/user-states/test-app/"  # Linux path for server
  }
  
  # Set a default user_id if session$user is NULL for local testing
  user_id <- ifelse(is.null(session$user), "local_test_user", session$user)
  # user_id <- ifelse(is.null(session$user), "local_test_user2", session$user)
  # user_id <- ifelse(is.null(session$user), "Adrian", session$user)
  
  is_admin <- user_id %in% admin_users
  
  output$admin_user_select <- renderUI({
    if (is_admin) {
      # Get list of user folders
      user_folders <- list.dirs(file_dir, full.names = FALSE, recursive = FALSE)
      
      # Admin panel UI with custom styling
      tags$div(class = "admin-panel",
               tags$h4("Admin Panel: Select User Folder"),
               selectInput("admin_selected_user", NULL, choices = user_folders, selected = user_id)
      )
    }
  })
  
  # Determine the target directory based on admin selection
  target_user_dir <- reactive({
    if (is_admin && !is.null(input$admin_selected_user)) {
      paste0(file_dir, input$admin_selected_user, "/")
    } else {
      paste0(file_dir, user_id, "/")
    }
  })
  
  # Ensure the target directory exists
  observe({
    dir <- target_user_dir()
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  })
  
  # Observe changes to target_user_dir and update file choices accordingly
  observe({
    req(target_user_dir())  # Ensure target_user_dir is available
    versions <- basename(list.files(target_user_dir(), full.names = TRUE))
    updateSelectInput(session, "version_select", choices = versions)
    updateSelectInput(session, "delete_version_select", choices = versions)
  })
  
  observeEvent(input$save, {
    files <- list.files(target_user_dir(), full.names = TRUE)
    
    if (length(files) >= max_files) {
      showNotification("Error: Maximum number of files reached.", type = "error", duration = 5)
      return()
    }
    
    if (input$project_name == "") {
      showNotification("Error: Please enter a project name before saving.", type = "error", duration = 5)
      return()
    }
    
    timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
    safe_project_name <- gsub("[^A-Za-z0-9_]+", "_", input$project_name)
    file_path <- paste0(target_user_dir(), timestamp, "_", safe_project_name, ".csv")
    
    state <- data.frame(slider1 = input$slider1, slider2 = input$slider2)
    write_csv(state, file_path)
    showNotification("Settings saved successfully.", type = "message", duration = 5)
    
    # Refresh dropdown choices after saving
    versions <- basename(list.files(target_user_dir(), full.names = TRUE))
    updateSelectInput(session, "version_select", choices = versions)
    updateSelectInput(session, "delete_version_select", choices = versions)
  })
  
  observeEvent(input$load, {
    selected_file <- paste0(target_user_dir(), input$version_select)
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
    
    file_to_delete <- file.path(target_user_dir(), input$delete_version_select)
    if (file.exists(file_to_delete)) {
      file.remove(file_to_delete)
      showNotification("File deleted successfully.", type = "message", duration = 5)
      
      # Refresh dropdown choices after deletion
      versions <- basename(list.files(target_user_dir(), full.names = TRUE))
      updateSelectInput(session, "version_select", choices = versions)
      updateSelectInput(session, "delete_version_select", choices = versions)
    } else {
      showNotification("Error: File could not be deleted.", type = "error", duration = 5)
    }
  })
}

shinyApp(ui = ui, server = server)
