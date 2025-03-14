app_name = "rBO-Citron v0.2.0"
#20250314
#Hongrong Yin


library(shiny)
library(shinydashboard)
library(shinyBS)      
library(DT)
library(data.table)
library(GPfit)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(ggcorrplot)
library(shinyjs)
library(rhandsontable)
library(RSQLite)
library(DBI)
library(randomForest)

###############################################################################
# Helper Functions
###############################################################################

# ---- Acquisition Functions ----
expected_improvement <- function(mu, sigma, best_y, jitter, minimize) {
  with_nonzero_sigma <- sigma > 0
  z <- rep(0, length(mu))
  ei <- rep(0, length(mu))
  if (minimize) {
    z[with_nonzero_sigma] <- (best_y - mu[with_nonzero_sigma] - jitter) / sigma[with_nonzero_sigma]
    ei[with_nonzero_sigma] <- (best_y - mu[with_nonzero_sigma] - jitter) * pnorm(z[with_nonzero_sigma]) +
      sigma[with_nonzero_sigma] * dnorm(z[with_nonzero_sigma])
  } else {
    z[with_nonzero_sigma] <- (mu[with_nonzero_sigma] - best_y - jitter) / sigma[with_nonzero_sigma]
    ei[with_nonzero_sigma] <- (mu[with_nonzero_sigma] - best_y - jitter) * pnorm(z[with_nonzero_sigma]) +
      sigma[with_nonzero_sigma] * dnorm(z[with_nonzero_sigma])
  }
  pmax(ei, 0)
}

probability_improvement <- function(mu, sigma, best_y, jitter, minimize) {
  with_nonzero_sigma <- sigma > 0
  z <- rep(0, length(mu))
  pi <- rep(0, length(mu))
  if (minimize) {
    z[with_nonzero_sigma] <- (best_y - mu[with_nonzero_sigma] - jitter) / sigma[with_nonzero_sigma]
    pi[with_nonzero_sigma] <- pnorm(z[with_nonzero_sigma])
  } else {
    z[with_nonzero_sigma] <- (mu[with_nonzero_sigma] - best_y - jitter) / sigma[with_nonzero_sigma]
    pi[with_nonzero_sigma] <- pnorm(z[with_nonzero_sigma])
  }
  pi
}

upper_confidence_bound <- function(mu, sigma, kappa, minimize) {
  if (minimize) mu - kappa * sigma else mu + kappa * sigma
}

# ---- Correlation Helper Functions ----
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tbl)$statistic)
  n <- sum(tbl)
  k <- nrow(tbl)
  r <- ncol(tbl)
  v <- sqrt(chi2 / (n * (min(k - 1, r - 1))))
  as.numeric(v)
}

correlation_ratio <- function(x, y) {
  if (!is.factor(x)) x <- as.factor(x)
  groups <- split(y, x)
  n <- length(y)
  grand_mean <- mean(y)
  ss_between <- sum(sapply(groups, function(g) length(g) * (mean(g) - grand_mean)^2))
  ss_total <- sum((y - grand_mean)^2)
  sqrt(ss_between / ss_total)
}

compute_cor <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    cor(x, y, use = "complete.obs")
  } else if (!is.numeric(x) && !is.numeric(y)) {
    cramers_v(x, y)
  } else {
    if (is.numeric(x) && !is.numeric(y)) {
      correlation_ratio(y, x)
    } else {
      correlation_ratio(x, y)
    }
  }
}

# ---- Shared Data Across Modules ----
# The database module will write its final data to sharedData$df
sharedData <- reactiveValues(df = NULL)

###############################################################################
# UI
###############################################################################
ui <- dashboardPage(
  dashboardHeader(title = app_name),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Database", tabName = "database", icon = icon("database")),
      menuItem("Bayes Optimization", tabName = "bayes", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "database",
        fluidRow(
          box(
            width = 4,
            radioButtons("data_source", "Select Data Source:", 
                         choices = c("Local SQLite (Directly Editable)", "Uploaded SQLite", "Uploaded CSV"),
                         inline = FALSE),
            hr(),
            conditionalPanel(
              condition = "input.data_source == 'Local SQLite (Directly Editable)'",
              textInput("db_dir", "SQLite Directory", value = getwd()),
              actionButton("load_dir", "Load Dir"),
              selectInput("db_file", "Choose SQLite file",
                          choices = c("None" = ""), selected = ""),
              selectInput("table", "Choose Table",
                          choices = c("None" = ""), selected = "")
            ),
            conditionalPanel(
              condition = "input.data_source == 'Uploaded SQLite'",
              fileInput("file_sqlite", "Upload .sqlite",
                        accept = c(".sqlite", ".db"))
            ),
            conditionalPanel(
              condition = "input.data_source == 'Uploaded CSV'",
              fileInput("file_csv", "Upload CSV", 
                        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
            ),
            hr(),
            textOutput("table_info"),
            hr(),
            textOutput("change_status"),
            selectInput("columns", "Display Columns",
                        choices = NULL, multiple = TRUE, selectize = TRUE),
            hr(),
            actionButton("toggle_mode", "View Mode", icon = icon("lock")),
            conditionalPanel(
              condition = "input.toggle_mode % 2 == 1",
              hr(),
              actionButton("add_row", "Add Row", icon = icon("plus")),
              selectizeInput("del_row_id", "Delete Row by ID", choices = NULL,
                             multiple = FALSE, options = list(placeholder = "Select row ID")),
              actionButton("del_row", "Delete Row", icon = icon("trash")),
              hr(),
              textInput("new_col_name", "New Column Name", value = ""),
              actionButton("add_col", "Add Column", icon = icon("plus-square")),
              selectInput("del_col_select", "Delete Column", choices = NULL),
              actionButton("del_col", "Delete Column", icon = icon("minus-square")),
              hr(),
              actionButton("save", "Save Changes", class = "btn-success"),
              actionButton("cancel", "Discard Changes", class = "btn-danger")
            ),
            hr(),
            downloadButton("download_csv", "Download CSV"),
            downloadButton("download_sqlite", "Download SQLite")
          ),
          box(
            width = 8,
            tabsetPanel(
              tabPanel(
                title = "Data Table",
                fluidRow(
                  column(width = 3, selectInput("search_col", "Search Column", choices = NULL)),
                  column(width = 5, textInput("search_val", "Search Keyword", "")),
                  column(width = 2, br(), actionButton("reset_search", "Reset Search"))
                ),
                fluidRow(
                  column(width = 3, selectInput("sort_col", "Sort Column", choices = c("None"))),
                  column(width = 3, radioButtons("sort_dir", "Direction", choices = c("asc"="asc", "desc"="desc"), inline = TRUE))
                ),
                fluidRow(
                  column(width = 2, numericInput("page_size", "Entries", value = 25, min = 1)),
                  column(width = 2, numericInput("page", "Page #", value = 1, min = 1)),
                  column(width = 4,
                         br(),
                         fluidRow(
                           column(width = 2, actionButton("prev_page", "<")),
                           column(width = 2, actionButton("next_page", ">")),
                           column(width = 4, textOutput("filtered_info"))
                         )
                  )
                ),
                rHandsontableOutput("hot")
              ),
              tabPanel(
                title = "Instructions",
                helpText("1. Select a data source (Local SQLite or Uploaded SQLite / CSV)"),
                helpText("2. For Local SQLite: Specify directory, click 'Load Dir', choose the .sqlite file, then choose a table (Local SQLite files can be edited directly)"),
                helpText("3. For Uploaded SQLite: Upload a .sqlite file and choose a table if there are multiple; data is loaded in memory."),
                helpText("4. For Uploaded CSV: Upload a CSV file; data is loaded in memory."),
                helpText("5. In 'View Mode', select columns, search keywords, or sort columns."),
                helpText("6. Switch to 'Edit Mode' to directly edit table cells, add/delete rows or columns."),
                helpText("7. 'Save Changes' writes back to the local SQLite file or updates in memory for uploads."),
                helpText("8. 'Discard Changes' reverts to the original data."),
                helpText("9. Use 'Download CSV' or 'Download SQLite' to export the data.")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "bayes",
        fluidRow(
          box(
            width = 4,
            radioButtons("bayes_data_source", "Bayesian Data Source:",
                         choices = c("Database Data", "Uploaded CSV"),
                         selected = "Database Data"), 
            conditionalPanel(
              condition = "input.bayes_data_source == 'Uploaded CSV'",
              fileInput("file1", "Upload Training CSV File",
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
            ),
            hr(),
            uiOutput("ovselectUI"),
            hr(),
            uiOutput("evselectUI"),
            uiOutput("evrangeUI"),
            hr(),
            h4("Optimization Constraint Settings"),
            DTOutput("constraintsTable"),
            actionButton("addConstraint", "Add Constraint"),
            actionButton("deleteConstraint", "Delete Selected Constraint")
          ),
          box(
            width = 8,
            tabsetPanel(
              tabPanel(
                title = "Data Display",
                h3("Data Filtering"),
                uiOutput("filterVariableUI"),
                uiOutput("filterValueUI"),
                hr(),
                DTOutput("contents")
              ),
              tabPanel(
                title = "Optimization",
                h3("Optimization Conditions"),
                selectInput("target", "Optimization Target:",
                            choices = c("Maximization", "Minimization")),
                numericInput("numCandidates", "Number of candidates proposed by BO:",
                             value = 5, min = 1),
                bsCollapse(
                  bsCollapsePanel("Hyperparameter Settings", 
                                  selectInput("acqFunc", "Select Acquisition Function:",
                                              choices = c("Expected Improvement (EI)" = "EI",
                                                          "Probability of Improvement (PI)" = "PI",
                                                          "Upper Confidence Bound (UCB)" = "UCB")),
                                  numericInput("kappa", "Kappa value for UCB (when selected):",
                                               value = 2.576, min = 0),
                                  sliderInput("jitter", "Jitter parameter for EI/PI:",
                                              min = 0, max = 2, value = 0.01, step = 0.01),
                                  hr(),
                                  selectInput("model_type", "Surrogate Model:",
                                              choices = c("Gaussian Process", "Random Forest"),
                                              selected = "Gaussian Process"),
                                  conditionalPanel(
                                  condition = "input.model_type == 'Random Forest'",
                                  numericInput("rf_ntree", "Number of Trees (ntree):", value = 500, min = 1),
                                  numericInput("rf_mtry", "Number of Variables for Split (mtry):", value = 2, min = 1)),
                                  style = "info"
                  )
                ),
                actionButton("optimize", "Execute Optimization", class = "btn-primary"),
                hr(),
                h3("Optimization Results"),
                DTOutput("optimizationResultsTable"),
                h3("Correlation Plot"),
                plotOutput("corrplot"),
                h3("Select Explanatory Variables for Plotting"),
                uiOutput("plotEvSelectUI"),
                h3("Prediction Plot"),
                plotOutput("varcorrplot"),
                h3("Acquisition Function Plot"),
                plotOutput("acquisitionplot")
              ),
              tabPanel(
                title = "Prediction",
                uiOutput("predictionUI")
              ),
              tabPanel(
                title = "Instructions",
                helpText("1. Choose 'Database Data' to use data from the Database tab or 'Uploaded CSV'."),
                helpText("2. Filter data, select objective and explanatory variables, and set constraints."),
                helpText("3. Click 'Execute Optimization' to run Bayesian optimization."),
                helpText("4. In the 'Prediction' tab, choose the prediction mode.")
              )
            )
          )
        )
      )
    )
  )
)

###############################################################################
# SERVER
###############################################################################
server <- function(input, output, session) {
  
  # =========  DATABASE TAB ==========
  rv <- reactiveValues(
    data = NULL,
    original = NULL,
    con = NULL,         # for Local SQLite
    changed = FALSE,
    edit_mode = FALSE
  )
  
  observeEvent(input$data_source, {
    if (!is.null(rv$con)) {
      try(dbDisconnect(rv$con), silent = TRUE)
      rv$con <- NULL
    }
    rv$data <- NULL
    rv$original <- NULL
    rv$edit_mode <- FALSE
    rv$changed <- FALSE
    updateSelectInput(session, "db_file", choices = c("None"=""), selected = "")
    updateSelectInput(session, "table", choices = c("None"=""), selected = "")
    updateSelectInput(session, "columns", choices = NULL)
    updateSelectInput(session, "sort_col", choices = c("None"))
    updateSelectInput(session, "search_col", choices = NULL)
  })
  
  observeEvent(input$load_dir, {
    req(input$db_dir)
    if (input$data_source != "Local SQLite (Directly Editable)") return()
    sql_files <- list.files(path = input$db_dir, pattern = "\\.sqlite$", full.names = TRUE)
    if (length(sql_files) == 0) {
      showNotification("No .sqlite file found in directory.", type = "warning")
    }
    updateSelectInput(session, "db_file",
                      choices = c("None" = "", sql_files),
                      selected = "")
  })
  
  observeEvent(input$db_file, {
    if (input$data_source != "Local SQLite (Directly Editable)") return()
    if (input$db_file == "") {
      if (!is.null(rv$con)) {
        dbDisconnect(rv$con)
        rv$con <- NULL
      }
      updateSelectInput(session, "table", choices = c("None"=""), selected = "")
      rv$data <- NULL
      rv$original <- NULL
      return()
    }
    if (!is.null(rv$con)) {
      dbDisconnect(rv$con)
    }
    rv$con <- dbConnect(SQLite(), input$db_file)
    tbls <- dbListTables(rv$con)
    updateSelectInput(session, "table",
                      choices = c("None"="", tbls), 
                      selected = "")
    rv$data <- NULL
    rv$original <- NULL
  })
  
  # Local SQLite
  observeEvent(input$table, {
    if (input$data_source != "Local SQLite (Directly Editable)") return()
    if (input$table == "") {
      rv$data <- NULL
      rv$original <- NULL
      return()
    }
    req(rv$con)
    tmp <- dbReadTable(rv$con, input$table)
    if (!("id" %in% names(tmp))) {
      tmp$id <- seq_len(nrow(tmp))
    }
    rv$original <- tmp
    rv$data <- tmp
    updateSelectInput(session, "columns", choices = names(rv$data), selected = names(rv$data))
    updateSelectInput(session, "sort_col", choices = c("None", names(rv$data)), selected = "None")
    updateSelectInput(session, "search_col", choices = c("All", names(rv$data)), selected = "All")
    updateSelectInput(session, "del_col_select", choices = setdiff(names(rv$data), "id"), selected = character(0))
    updateSelectizeInput(session, "del_row_id", choices = sort(unique(rv$data$id)), server = TRUE)
  })
  
  # Uploaded SQLite (in-memory)
  observeEvent(input$file_sqlite, {
    if (input$data_source != "Uploaded SQLite") return()
    req(input$file_sqlite)
    sqlite_path <- input$file_sqlite$datapath
    con_tmp <- dbConnect(SQLite(), sqlite_path)
    on.exit(dbDisconnect(con_tmp), add = TRUE)
    tbls <- dbListTables(con_tmp)
    if (length(tbls) == 0) {
      showNotification("No tables found in uploaded .sqlite", type = "error")
      return()
    }
    if (length(tbls) == 1) {
      chosen_table <- tbls[1]
      df_uploaded <- dbReadTable(con_tmp, chosen_table)
      if (!("id" %in% names(df_uploaded))) {
        df_uploaded$id <- seq_len(nrow(df_uploaded))
      }
      rv$original <- df_uploaded
      rv$data <- df_uploaded
      updateSelectInput(session, "columns", choices = names(rv$data), selected = names(rv$data))
      updateSelectInput(session, "sort_col", choices = c("None", names(rv$data)), selected = "None")
      updateSelectInput(session, "search_col", choices = c("All", names(rv$data)), selected = "All")
      updateSelectInput(session, "del_col_select", choices = setdiff(names(rv$data), "id"), selected = character(0))
      updateSelectizeInput(session, "del_row_id", choices = sort(unique(rv$data$id)), server = TRUE)
    } else {
      showModal(modalDialog(
        title = "Select table from uploaded SQLite",
        selectInput("uploaded_sqlite_table", "Table", choices = tbls),
        footer = tagList(
          actionButton("ok_uploaded_sqlite_table", "OK"),
          modalButton("Cancel")
        )
      ))
    }
  })
  
  observeEvent(input$ok_uploaded_sqlite_table, {
    removeModal()
    req(input$file_sqlite$datapath, input$uploaded_sqlite_table)
    con_tmp <- dbConnect(SQLite(), input$file_sqlite$datapath)
    on.exit(dbDisconnect(con_tmp), add = TRUE)
    df_uploaded <- dbReadTable(con_tmp, input$uploaded_sqlite_table)
    if (!("id" %in% names(df_uploaded))) {
      df_uploaded$id <- seq_len(nrow(df_uploaded))
    }
    rv$original <- df_uploaded
    rv$data <- df_uploaded
    updateSelectInput(session, "columns", choices = names(rv$data), selected = names(rv$data))
    updateSelectInput(session, "sort_col", choices = c("None", names(rv$data)), selected = "None")
    updateSelectInput(session, "search_col", choices = c("All", names(rv$data)), selected = "All")
    updateSelectInput(session, "del_col_select", choices = setdiff(names(rv$data), "id"), selected = character(0))
    updateSelectizeInput(session, "del_row_id", choices = sort(unique(rv$data$id)), server = TRUE)
  })
  
  # CSV (in-memory)
  observeEvent(input$file_csv, {
    if (input$data_source != "Uploaded CSV") return()
    req(input$file_csv)
    csv_path <- input$file_csv$datapath
    tmp <- tryCatch(
      read.csv(csv_path, stringsAsFactors = FALSE),
      error = function(e) {
        showNotification(paste("CSV read error:", e$message), type = "error")
        return(NULL)
      }
    )
    if (is.null(tmp)) return()
    if (!("id" %in% names(tmp))) {
      tmp$id <- seq_len(nrow(tmp))
    }
    rv$original <- tmp
    rv$data <- tmp
    updateSelectInput(session, "columns", choices = names(rv$data), selected = names(rv$data))
    updateSelectInput(session, "sort_col", choices = c("None", names(rv$data)), selected = "None")
    updateSelectInput(session, "search_col", choices = c("All", names(rv$data)), selected = "All")
    updateSelectInput(session, "del_col_select", choices = setdiff(names(rv$data), "id"), selected = character(0))
    updateSelectizeInput(session, "del_row_id", choices = sort(unique(rv$data$id)), server = TRUE)
  })
  
  observe({
    req(rv$data, rv$original)
    rv$changed <- !identical(rv$data, rv$original)
  })
  
  observeEvent(input$reset_search, {
    updateTextInput(session, "search_val", value = "")
  })
  
  sorted_filtered_data <- reactive({
    req(rv$data)
    data <- rv$data
    kw <- trimws(input$search_val)
    col_search <- input$search_col
    if (kw != "") {
      if (col_search == "All") {
        data <- data[apply(data, 1, function(row) any(grepl(kw, row))), ]
      } else if (col_search %in% names(data)) {
        data <- data[grepl(kw, data[[col_search]]), ]
      }
    }
    if (input$sort_col != "None") {
      col <- input$sort_col
      if (col %in% names(data)) {
        if (input$sort_dir == "asc") {
          data <- data[order(data[[col]]), ]
        } else {
          data <- data[order(data[[col]], decreasing = TRUE), ]
        }
      }
    }
    data
  })
  
  paginated_data <- reactive({
    data <- sorted_filtered_data()
    total_rows <- nrow(data)
    page_size <- input$page_size
    page <- input$page
    start_index <- (page - 1) * page_size + 1
    end_index <- min(start_index + page_size - 1, total_rows)
    if (start_index > total_rows) {
      data[FALSE, ]
    } else {
      data[start_index:end_index, ]
    }
  })
  
  display_data <- reactive({
    df <- paginated_data()
    req(input$columns)
    df[, intersect(input$columns, names(df)), drop = FALSE]
  })
  
  output$hot <- renderRHandsontable({
    if (is.null(rv$data)) return(NULL)
    df <- display_data()
    rhandsontable(
      df,
      readOnly = !rv$edit_mode,
      rowHeaders = TRUE,
      height = 400,
      width = "100%"
    ) %>%
      hot_table(
        stretchH = "all",
        fixedColumnsLeft = 1,
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
        highlightCol = TRUE,
        highlightRow = TRUE
      ) %>%
      hot_context_menu(allowRowEdit = rv$edit_mode,
                       allowColEdit = rv$edit_mode)
  })
  
  observeEvent(input$hot, {
    req(rv$data, rv$edit_mode)
    req(input$hot)
    new_data <- hot_to_r(input$hot)
    df_current <- paginated_data()
    displayed_cols <- colnames(new_data)
    df_for_edit <- df_current[, intersect(displayed_cols, names(df_current)), drop = FALSE]
    if (nrow(new_data) != nrow(df_for_edit)) return()
    for (i in seq_len(nrow(new_data))) {
      this_id <- if ("id" %in% displayed_cols) {
        df_for_edit$id[i]
      } else {
        df_current$id[i]
      }
      row_index <- which(rv$data$id == this_id)
      if (length(row_index) == 1) {
        for (col_nm in displayed_cols) {
          if (col_nm %in% names(rv$data)) {
            rv$data[row_index, col_nm] <- new_data[i, col_nm]
          }
        }
      }
    }
  })
  
  observeEvent(input$prev_page, {
    if (input$page > 1) {
      updateNumericInput(session, "page", value = input$page - 1)
    }
  })
  observeEvent(input$next_page, {
    req(rv$data)
    data <- sorted_filtered_data()
    max_page <- ceiling(nrow(data) / input$page_size)
    if (input$page < max_page) {
      updateNumericInput(session, "page", value = input$page + 1)
    }
  })
  
  observeEvent(input$toggle_mode, {
    rv$edit_mode <- !rv$edit_mode
    if (rv$edit_mode) {
      updateActionButton(session, "toggle_mode", label = "Edit Mode", icon = icon("lock-open"))
    } else {
      if (rv$changed) {
        showModal(modalDialog(
          title = "Unsaved Changes",
          "You have unsaved changes. Save now?",
          footer = tagList(
            actionButton("force_save", "Save"),
            actionButton("force_discard", "Discard"),
            modalButton("Cancel")
          )
        ))
      }
      updateActionButton(session, "toggle_mode", label = "View Mode", icon = icon("lock"))
    }
  })
  
  observeEvent(input$force_save, {
    if (input$data_source == "Local SQLite (Directly Editable)") {
      req(rv$con, input$table)
      dbWriteTable(rv$con, input$table, rv$data, overwrite = TRUE)
      showNotification("Changes saved to local SQLite.", type = "message")
    } else {
      rv$original <- rv$data
      showNotification("Changes saved (in-memory).", type = "message")
    }
    removeModal()
    rv$edit_mode <- FALSE
    rv$original <- rv$data
  })
  
  observeEvent(input$force_discard, {
    rv$data <- rv$original
    removeModal()
    rv$edit_mode <- FALSE
  })
  
  observeEvent(input$add_row, {
    if (!rv$edit_mode) {
      showNotification("Switch to Edit Mode first.", type = "warning")
      return()
    }
    req(rv$data)
    new_id <- ifelse(nrow(rv$data) == 0, 1, max(rv$data$id) + 1)
    new_row <- setNames(lapply(names(rv$data), function(x) NA), names(rv$data))
    new_row[["id"]] <- new_id
    rv$data <- rbind(rv$data, new_row)
    updateSelectizeInput(session, "del_row_id", choices = sort(unique(rv$data$id)), server = TRUE)
  })
  
  observeEvent(input$del_row, {
    if (!rv$edit_mode) {
      showNotification("Switch to Edit Mode first.", type = "warning")
      return()
    }
    req(rv$data)
    id_del <- input$del_row_id
    if (is.null(id_del) || id_del == "") {
      showNotification("Select a row ID to delete.", type = "warning")
      return()
    }
    id_del_num <- as.numeric(id_del)
    if (!(id_del_num %in% rv$data$id)) {
      showNotification(paste("ID does not exist:", id_del_num), type = "error")
      return()
    }
    rv$data <- rv$data[rv$data$id != id_del_num, ]
    showNotification(paste("Deleted row with ID =", id_del_num), type = "message")
    updateSelectizeInput(session, "del_row_id", choices = sort(unique(rv$data$id)), server = TRUE)
  })
  
  observeEvent(input$add_col, {
    if (!rv$edit_mode) {
      showNotification("Switch to Edit Mode first.", type = "warning")
      return()
    }
    req(rv$data)
    colName <- trimws(input$new_col_name)
    if (colName == "") {
      showNotification("Enter a column name.", type = "warning")
      return()
    }
    if (colName %in% names(rv$data)) {
      showNotification("Column already exists.", type = "error")
      return()
    }
    rv$data[[colName]] <- NA
    showNotification(paste("Added column:", colName), type = "message")
    updateSelectInput(session, "columns", choices = names(rv$data), 
                      selected = c(input$columns, colName))
    updateSelectInput(session, "del_col_select", choices = setdiff(names(rv$data), "id"), selected = character(0))
    updateTextInput(session, "new_col_name", value = "")
  })
  
  observeEvent(input$del_col, {
    if (!rv$edit_mode) {
      showNotification("Switch to Edit Mode first.", type = "warning")
      return()
    }
    req(rv$data)
    col_to_del <- input$del_col_select
    if (is.null(col_to_del) || col_to_del == "") {
      showNotification("Select a column to delete.", type = "warning")
      return()
    }
    if (!(col_to_del %in% names(rv$data))) {
      showNotification("Column not found.", type = "error")
      return()
    }
    if (col_to_del == "id") {
      showNotification("Cannot delete 'id' column.", type = "error")
      return()
    }
    rv$data[[col_to_del]] <- NULL
    showNotification(paste("Deleted column:", col_to_del), type = "message")
    new_selected <- setdiff(input$columns, col_to_del)
    updateSelectInput(session, "columns", choices = names(rv$data), selected = new_selected)
    updateSelectInput(session, "del_col_select", choices = setdiff(names(rv$data), "id"), selected = character(0))
  })
  
  observeEvent(input$save, {
    if (!rv$edit_mode) {
      showNotification("Switch to Edit Mode first.", type = "warning")
      return()
    }
    if (input$data_source == "Local SQLite (Directly Editable)") {
      req(rv$con, input$table)
      dbWriteTable(rv$con, input$table, rv$data, overwrite = TRUE)
      rv$original <- rv$data
      showNotification("Changes saved to local SQLite.", type = "message")
    } else {
      rv$original <- rv$data
      showNotification("Changes saved (in-memory).", type = "message")
    }
  })
  
  observeEvent(input$cancel, {
    if (!rv$edit_mode) {
      showNotification("Switch to Edit Mode first.", type = "warning")
      return()
    }
    rv$data <- rv$original
    showNotification("Changes discarded.", type = "warning")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      if (input$data_source == "Local SQLite (Directly Editable)") {
        tbl_name <- if (input$table == "") "table" else input$table
        paste0(tbl_name, ".csv")
      } else if (input$data_source == "Uploaded SQLite") {
        "uploaded_sqlite_data.csv"
      } else {
        "mydata.csv"
      }
    },
    content = function(file) {
      if (is.null(rv$data)) {
        write.csv(data.frame(), file, row.names = FALSE, na = "")
      } else {
        write.csv(rv$data, file, row.names = FALSE, na = "")
      }
    }
  )
  
  output$download_sqlite <- downloadHandler(
    filename = function() {
      if (input$data_source == "Local SQLite (Directly Editable)") {
        tbl_name <- if (input$table == "") "table" else input$table
        paste0(tbl_name, "_export.sqlite")
      } else if (input$data_source == "Uploaded SQLite") {
        "uploaded_sqlite_export.sqlite"
      } else {
        "mydata.sqlite"
      }
    },
    content = function(file) {
      con_tmp <- dbConnect(SQLite(), file)
      on.exit(dbDisconnect(con_tmp), add = TRUE)
      table_name <- "mydata"
      if (!is.null(rv$data)) {
        dbWriteTable(con_tmp, table_name, rv$data, overwrite = TRUE)
      }
    }
  )
  
  output$table_info <- renderText({
    if (is.null(rv$data)) return("No data loaded.")
    paste("Full Table: rows =", nrow(rv$data), "cols =", ncol(rv$data))
  })
  
  output$filtered_info <- renderText({
    if (is.null(rv$data)) return("")
    filtered <- sorted_filtered_data()
    total_rows <- nrow(filtered)
    total_pages <- ceiling(total_rows / input$page_size)
    page_cur <- input$page
    paste0("Rows:", total_rows, " Page:", page_cur, "/", total_pages)
  })
  
  output$change_status <- renderText({
    if (rv$changed) "(* Unsaved changes *)" else ""
  })
  
  observe({
    sharedData$df <- rv$data
  })
  
  onStop(function() {
    if (!is.null(rv$con)) {
      dbDisconnect(rv$con)
    }
  })
  
  # ========= BAYESIAN OPTIMIZATION TAB ==========
  data <- reactive({
    req(input$bayes_data_source)
    if (input$bayes_data_source == "Database Data") {
      req(sharedData$df)
      if (is.null(sharedData$df) || nrow(sharedData$df) == 0)
        validate(need(FALSE, "No data from database."))
      if (ncol(sharedData$df) <= 1)
        validate(need(FALSE, "Need at least 2 columns in database data."))
      sharedData$df
    } else {
      req(input$file1)
      tryCatch({
        df <- read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE)
        if (ncol(df) <= 1)
          validate(need(FALSE, "Data must have at least two columns."))
        df
      }, error = function(e) {
        showModal(modalDialog(
          title = "File Error",
          paste("An error occurred while reading the file:", e$message),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        NULL
      })
    }
  })
  
  output$filterVariableUI <- renderUI({
    req(data())
    selectInput("filterVars", "Select variables to filter:",
                choices = names(data()), multiple = TRUE)
  })
  
  output$filterValueUI <- renderUI({
    req(input$filterVars)
    lapply(input$filterVars, function(var) {
      var_data <- data()[[var]]
      if (is.numeric(var_data)) {
        min_val <- min(var_data, na.rm = TRUE)
        max_val <- max(var_data, na.rm = TRUE)
        tagList(
          fluidRow(
            column(3, h4(paste("Filter for", var))),
            column(3, numericInput(paste0("filter_min_", var), paste(var, "Minimum"), value = min_val)),
            column(3, numericInput(paste0("filter_max_", var), paste(var, "Maximum"), value = max_val))
          )
        )
      } else {
        var_levels <- sort(unique(as.character(var_data)))
        tagList(
          h4(paste("Filter for", var)),
          checkboxGroupInput(paste0("filter_values_", var), 
                             paste("Select values for", var),
                             choices = var_levels, selected = var_levels)
        )
      }
    })
  })
  
  filtered_data <- reactive({
    req(data())
    df <- data()
    if (!is.null(input$filterVars)) {
      for (var in input$filterVars) {
        var_data <- df[[var]]
        if (is.numeric(var_data)) {
          min_val <- input[[paste0("filter_min_", var)]]
          max_val <- input[[paste0("filter_max_", var)]]
          df <- df[var_data >= min_val & var_data <= max_val, ]
        } else {
          selected_values <- input[[paste0("filter_values_", var)]]
          df <- df[as.character(var_data) %in% selected_values, ]
        }
      }
    }
    df
  })
  
  selected_data <- reactive({
    req(filtered_data())
    if (!is.null(input$objective) && !is.null(input$evs)) {
      df <- filtered_data()[, c(input$objective, input$evs), drop = FALSE]
      df
    } else {
      filtered_data()
    }
  })
  
  output$contents <- renderDT({
    req(selected_data())
    datatable(selected_data())
  })
  
  output$ovselectUI <- renderUI({
    req(filtered_data())
    selectInput("objective", "Select Objective Variable:",
                choices = names(filtered_data()))
  })
  
  output$evselectUI <- renderUI({
    req(filtered_data())
    remaining_vars <- if (is.null(input$objective)) {
      names(filtered_data())
    } else {
      setdiff(names(filtered_data()), input$objective)
    }
    selectInput("evs", "Select Explanatory Variables:",
                choices = remaining_vars, multiple = TRUE)
  })
  
  output$evrangeUI <- renderUI({
    req(input$evs)
    tagList(
      lapply(input$evs, function(ev) {
        column_data <- filtered_data()[[ev]]
        if (is.numeric(column_data)) {
          min_val <- min(column_data, na.rm = TRUE)
          max_val <- max(column_data, na.rm = TRUE)
          fluidRow(
            column(12, strong(ev)),
            column(4, numericInput(paste0("min_", ev), "Minimum", value = min_val)),
            column(4, numericInput(paste0("max_", ev), "Maximum", value = max_val)),
            column(4, numericInput(paste0("interval_", ev), "Interval", value = 0.1, min = 0.01))
          )
        } else {
          levels_choices <- sort(unique(as.character(column_data)))
          fluidRow(
            column(12, strong(ev)),
            column(12, selectInput(paste0("levels_", ev), 
                                   paste("Select levels for", ev),
                                   choices = levels_choices, 
                                   selected = levels_choices, multiple = TRUE))
          )
        }
      })
    )
  })
  
  constraints <- reactiveVal(data.table(id = integer(), Expression = character()))
  constraint_counter <- reactiveVal(0)
  
  observeEvent(input$addConstraint, {
    showModal(modalDialog(
      title = "Add Constraint",
      numericInput("numEVs", "Number of explanatory variables in constraint:", value = 2, min = 1),
      uiOutput("dynamicConstraintUI"),
      numericInput("constraintValue", "Input constraint value:", value = 0),
      helpText("Operators: +, -, *, /"),
      helpText("Conditions: ==, <=, >=, <, >"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveConstraint", "Save Constraint")
      )
    ))
  })
  
  output$dynamicConstraintUI <- renderUI({
    req(input$numEVs)
    numEVs <- input$numEVs
    ui_elements <- vector("list", numEVs * 2 - 1)
    for (i in 1:numEVs) {
      ui_elements[[i * 2 - 1]] <- selectInput(paste0("constraintEV", i),
                                              paste("Select explanatory variable", i, ":"),
                                              choices = input$evs)
      if (i < numEVs) {
        ui_elements[[i * 2]] <- selectInput(paste0("constraintOp", i), "Select operator:",
                                            choices = c("+", "-", "*", "/"))
      }
    }
    ui_elements[[numEVs * 2]] <- selectInput("constraintCond", "Select condition:",
                                             choices = c("==", "<=", ">=", "<", ">"))
    do.call(tagList, ui_elements)
  })
  
  observeEvent(input$saveConstraint, {
    numEVs <- input$numEVs
    evs_vec <- vector("character", numEVs)
    ops <- vector("character", numEVs - 1)
    for (i in 1:numEVs) {
      evs_vec[i] <- input[[paste0("constraintEV", i)]]
      if (i < numEVs) {
        ops[i] <- input[[paste0("constraintOp", i)]]
      }
    }
    constraint_expr <- ""
    for (i in 1:numEVs) {
      constraint_expr <- paste0(constraint_expr, evs_vec[i])
      if (i < numEVs) {
        constraint_expr <- paste0(constraint_expr, ops[i])
      }
    }
    constraint_expr <- paste0(constraint_expr, input$constraintCond, input$constraintValue)
    is_valid <- tryCatch({
      parse(text = constraint_expr)
      TRUE
    }, error = function(e) FALSE)
    if (!is_valid) {
      showModal(modalDialog(
        title = "Invalid Constraint",
        "The constraint expression is invalid. Please check your input.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      constraint_counter(constraint_counter() + 1)
      new_constraint <- data.table(
        id = constraint_counter(),
        Expression = constraint_expr
      )
      constraints(rbind(constraints(), new_constraint))
      removeModal()
    }
  })
  
  output$constraintsTable <- renderDT({
    constraints_dt <- constraints()
    datatable(constraints_dt[, .(Expression)], selection = 'multiple',
              rownames = FALSE, options = list(dom = 't'))
  })
  
  observeEvent(input$deleteConstraint, {
    selected_rows <- input$constraintsTable_rows_selected
    if (length(selected_rows)) {
      constraints_dt <- constraints()
      constraints(constraints_dt[-selected_rows, ])
    }
  })
  
  output$plotEvSelectUI <- renderUI({
    req(input$evs)
    if (length(input$evs) > 0) {
      selectInput("plotEVs", "Explanatory Variables for Plotting (Max 2):",
                  choices = input$evs,
                  selected = input$evs[1:min(2, length(input$evs))],
                  multiple = TRUE)
    }
  })
  
  optimization_results <- reactiveValues(
    candidate_df = NULL,
    varcorrplot_data = NULL,
    acquisitionplot_data = NULL,
    gp_model = NULL
  )
  
  encode_explanatory <- function(df, evs) {
    df_subset <- df[, evs, drop = FALSE]
    for(col in evs) {
      if(!is.numeric(df_subset[[col]])){
        all_levels <- sort(unique(filtered_data()[[col]]))
        df_subset[[col]] <- factor(df_subset[[col]], levels = all_levels)
      }
    }
    model.matrix(~ . -1, data = df_subset)
  }
  
  compute_scaling_info <- function(X) {
    sc <- list()
    for(i in seq_len(ncol(X))){
      colname <- colnames(X)[i]
      if(all(X[, i] %in% c(0,1))){
        sc[[colname]] <- list(min = 0, max = 1)
      } else {
        sc[[colname]] <- list(min = min(X[, i], na.rm = TRUE), max = max(X[, i], na.rm = TRUE))
      }
    }
    sc
  }
  
  scale_matrix <- function(X, scaling) {
    X_scaled <- X
    for(i in seq_len(ncol(X))){
      colname <- colnames(X)[i]
      rng <- scaling[[colname]]$max - scaling[[colname]]$min
      if(rng == 0) rng <- 1
      X_scaled[, i] <- (X[, i] - scaling[[colname]]$min) / rng
    }
    X_scaled
  }

  surrogate_model <- reactive({
    req(filtered_data(), input$objective, input$evs)
    obj_var <- filtered_data()[[input$objective]]
    X <- encode_explanatory(filtered_data(), input$evs)
    sc <- compute_scaling_info(X)
    X_scaled <- scale_matrix(X, sc)
    
    if (input$model_type == "Gaussian Process") {
      tryCatch({
        GP_fit(X_scaled, obj_var)
      }, error = function(e) { NULL })
    } else if (input$model_type == "Random Forest") {
      tryCatch({
        randomForest(x = X_scaled, y = obj_var, ntree = input$rf_ntree, mtry = input$rf_mtry)
      }, error = function(e) { NULL })
    }
  })
  
  observeEvent(input$optimize, {
    withProgress(message = "Optimization in progress. Please wait...", value = 0, {
      incProgress(0.1, detail = "Validating input...")
      req(input$objective, input$evs, input$target, nrow(filtered_data()) > 0)

      for (ev in input$evs) {
        col_data <- filtered_data()[[ev]]
        if (is.numeric(col_data)) {
          if (input[[paste0("min_", ev)]] >= input[[paste0("max_", ev)]]) {
            showModal(modalDialog(
              title = "Invalid Input",
              paste("The minimum value for", ev, "must be smaller than its maximum value."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return()
          }
          interval <- input[[paste0("interval_", ev)]]
          if (interval <= 0 || interval >= (input[[paste0("max_", ev)]] - input[[paste0("min_", ev)]])) {
            showModal(modalDialog(
              title = "Invalid Interval",
              paste("The interval for", ev, "must be positive and less than (maximum - minimum)."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return()
          }
        } else {
          selected_levels <- input[[paste0("levels_", ev)]]
          if (is.null(selected_levels) || length(selected_levels) == 0) {
            showModal(modalDialog(
              title = "Invalid Input",
              paste("Please select at least one level for", ev, "."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return()
          }
        }
      }
      
      incProgress(0.3, detail = "Generating grid...")
      obj_var <- filtered_data()[[input$objective]]
      bounds_list <- lapply(input$evs, function(ev) {
        col_data <- filtered_data()[[ev]]
        if (is.numeric(col_data)) {
          min_val <- input[[paste0("min_", ev)]]
          max_val <- input[[paste0("max_", ev)]]
          interval <- input[[paste0("interval_", ev)]]
          n_points <- floor((max_val - min_val) / interval) + 1
          if (n_points <= 1) n_points <- 2
          seq(min_val, max_val, length.out = n_points)
        } else {
          factor(input[[paste0("levels_", ev)]], levels = sort(unique(filtered_data()[[ev]])))
        }
      })
      names(bounds_list) <- input$evs
      grid_df <- expand.grid(bounds_list, stringsAsFactors = FALSE)
      
      if (nrow(constraints()) > 0) {
        for (i in 1:nrow(constraints())) {
          expr <- constraints()$Expression[i]
          tryCatch({
            grid_df <- grid_df[with(grid_df, eval(parse(text = expr))), ]
          }, error = function(e) {
            showModal(modalDialog(
              title = "Constraint Error",
              paste("Constraint error:", e$message),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return()
          })
        }
      }
      if (nrow(grid_df) == 0) {
        showModal(modalDialog(
          title = "No Valid Points Found",
          "No valid points were found after applying constraints. Please adjust the constraints or variable ranges.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      
      incProgress(0.4, detail = "Building surrogate model...")
      if (is.null(surrogate_model())) {
        showModal(modalDialog(
          title = "Model Fitting Error",
          "Surrogate model cannot be built with the current data and settings.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      optimization_results$gp_model <- surrogate_model()
      minimize <- (input$target == "Minimization")
      best_y <- if (minimize) min(obj_var) else max(obj_var)
      
      incProgress(0.6, detail = "Performing prediction on grid...")
      grid_encoded <- encode_explanatory(grid_df, input$evs)
      sc_grid <- compute_scaling_info(grid_encoded)
      grid_scaled <- scale_matrix(grid_encoded, sc_grid)
      
      if (input$model_type == "Gaussian Process") {
        predictions_all <- predict.GP(surrogate_model(), as.matrix(grid_scaled))
        mu_all <- predictions_all$Y_hat
        sigma_all <- sqrt(predictions_all$MSE)
      } else if (input$model_type == "Random Forest") {
        rf_preds <- predict(surrogate_model(), newdata = as.data.frame(grid_scaled), predict.all = TRUE)
        mu_all <- rowMeans(rf_preds$individual)
        sigma_all <- apply(rf_preds$individual, 1, sd)
      }
      
      ci_multiplier <- 1.96
      grid_df$Predicted_OV <- round(mu_all, 3)
      grid_df$Lower_CI <- round(mu_all - ci_multiplier * sigma_all, 3)
      grid_df$Upper_CI <- round(mu_all + ci_multiplier * sigma_all, 3)
      
      incProgress(0.7, detail = "Calculating acquisition function...")
      if (input$acqFunc == "EI") {
        acq_val <- expected_improvement(mu_all, sigma_all, best_y, input$jitter, minimize)
      } else if (input$acqFunc == "PI") {
        acq_val <- probability_improvement(mu_all, sigma_all, best_y, input$jitter, minimize)
      } else {
        acq_val <- upper_confidence_bound(mu_all, sigma_all, input$kappa, minimize)
      }
      grid_df$Acquisition <- acq_val
      grid_sorted <- grid_df[order(-grid_df$Acquisition), ]
      candidate_df <- data.table::as.data.table(grid_sorted[1:min(input$numCandidates, nrow(grid_sorted)), ])
      optimization_results$candidate_df <- candidate_df
      
      incProgress(0.8, detail = "Organizing results and generating plots...")
      other_evs <- setdiff(input$evs, input$plotEVs)
      fixed_values <- sapply(other_evs, function(ev) {
        if (is.numeric(filtered_data()[[ev]])) {
          mean(filtered_data()[[ev]], na.rm = TRUE)
        } else {
          sort(unique(as.character(filtered_data()[[ev]])))[1]
        }
      }, simplify = FALSE)
      
      if (length(input$plotEVs) == 1) {
        ev_name <- input$plotEVs[1]
        if (is.numeric(filtered_data()[[ev_name]])) {
          min_val <- input[[paste0("min_", ev_name)]]
          max_val <- input[[paste0("max_", ev_name)]]
          n_points <- 100
          grid_var <- data.frame(seq(min_val, max_val, length.out = n_points))
          names(grid_var) <- ev_name
        } else {
          grid_var <- data.frame(factor(input[[paste0("levels_", ev_name)]], levels = sort(unique(filtered_data()[[ev_name]]))))
          names(grid_var) <- ev_name
        }
        for (ev in other_evs) {
          grid_var[[ev]] <- fixed_values[[ev]]
        }
        grid_var_encoded <- encode_explanatory(grid_var, input$evs)
        sc_var <- compute_scaling_info(grid_var_encoded)
        grid_var_scaled <- scale_matrix(grid_var_encoded, sc_var)
        if (input$model_type == "Gaussian Process") {
          preds_var <- predict.GP(surrogate_model(), as.matrix(grid_var_scaled))
          grid_var$Prediction <- preds_var$Y_hat
          grid_var$Uncertainty <- sqrt(preds_var$MSE)
          grid_var$Acquisition <- expected_improvement(preds_var$Y_hat, sqrt(preds_var$MSE), best_y, input$jitter, minimize)
        } else if (input$model_type == "Random Forest") {
          rf_preds <- predict(surrogate_model(), newdata = as.data.frame(grid_var_scaled), predict.all = TRUE)
          grid_var$Prediction <- rowMeans(rf_preds$individual)
          grid_var$Uncertainty <- apply(rf_preds$individual, 1, sd)
          grid_var$Acquisition <- expected_improvement(rowMeans(rf_preds$individual), 
                                                       apply(rf_preds$individual, 1, sd), 
                                                       best_y, input$jitter, minimize)
        }
        optimization_results$varcorrplot_data <- grid_var
        optimization_results$acquisitionplot_data <- grid_var
      } else if (length(input$plotEVs) == 2) {
        ev1 <- input$plotEVs[1]
        ev2 <- input$plotEVs[2]
        makeSeq <- function(ev) {
          if (is.numeric(filtered_data()[[ev]])) {
            min_val <- input[[paste0("min_", ev)]]
            max_val <- input[[paste0("max_", ev)]]
            seq(min_val, max_val, length.out = 50)
          } else {
            factor(input[[paste0("levels_", ev)]], levels = sort(unique(filtered_data()[[ev]])))
          }
        }
        seq1 <- makeSeq(ev1)
        seq2 <- makeSeq(ev2)
        grid_var <- expand.grid(seq1, seq2, stringsAsFactors = FALSE)
        names(grid_var) <- c(ev1, ev2)
        for (ev in other_evs) {
          grid_var[[ev]] <- fixed_values[[ev]]
        }
        grid_var_encoded <- encode_explanatory(grid_var, input$evs)
        sc_var <- compute_scaling_info(grid_var_encoded)
        grid_var_scaled <- scale_matrix(grid_var_encoded, sc_var)
        if (input$model_type == "Gaussian Process") {
          preds_var <- predict.GP(surrogate_model(), as.matrix(grid_var_scaled))
          grid_var$Prediction <- preds_var$Y_hat
          grid_var$Acquisition <- expected_improvement(preds_var$Y_hat, sqrt(preds_var$MSE), best_y, input$jitter, minimize)
        } else if (input$model_type == "Random Forest") {
          rf_preds <- predict(surrogate_model(), newdata = as.data.frame(grid_var_scaled), predict.all = TRUE)
          grid_var$Prediction <- rowMeans(rf_preds$individual)
          grid_var$Acquisition <- expected_improvement(rowMeans(rf_preds$individual),
                                                       apply(rf_preds$individual, 1, sd),
                                                       best_y, input$jitter, minimize)
        }
        optimization_results$varcorrplot_data <- grid_var
        optimization_results$acquisitionplot_data <- grid_var
      }
      
      output$corrplot <- renderPlot({
        req(selected_data())
        dat <- selected_data()
        cols <- names(dat)
        n <- length(cols)
        corr_mat <- matrix(NA, nrow = n, ncol = n)
        colnames(corr_mat) <- rownames(corr_mat) <- cols
        for(i in seq_along(cols)) {
          for(j in seq_along(cols)) {
            corr_mat[i,j] <- tryCatch({
              compute_cor(dat[[i]], dat[[j]])
            }, error = function(e) NA)
          }
        }
        ggcorrplot(corr_mat, hc.order = TRUE, type = "full", lab = TRUE) +
          labs(title = "Correlation Plot")
      })
      
      output$optimizationResultsTable <- renderDT({
        req(optimization_results$candidate_df)
        datatable(optimization_results$candidate_df,
                  options = list(scrollX = TRUE, autoWidth = TRUE, pageLength = 10),
                  caption = "Optimal solutions based on the selected acquisition function")
      })
      
      output$varcorrplot <- renderPlot({
        req(optimization_results$varcorrplot_data)
        df_plot <- optimization_results$varcorrplot_data
        if (length(input$plotEVs) == 1) {
          ev_name <- input$plotEVs[1]
          if (is.numeric(filtered_data()[[ev_name]])) {
            p <- ggplot(df_plot, aes_string(x = ev_name, y = "Prediction")) + geom_line()
            if ("Uncertainty" %in% names(df_plot)) {
              p <- p + geom_ribbon(aes(ymin = Prediction - Uncertainty,
                                       ymax = Prediction + Uncertainty), alpha = 0.2)
            }
            p + labs(title = "Prediction Plot (Numerical Explanatory Variable)",
                     x = ev_name, y = "Predicted Value") + theme_minimal()
          } else {
            ggplot(df_plot, aes_string(x = ev_name, y = "Prediction")) +
              geom_point(size = 3) +
              labs(title = "Prediction Plot (Categorical Explanatory Variable)",
                   x = ev_name, y = "Predicted Value") + theme_minimal()
          }
        } else if (length(input$plotEVs) == 2) {
          ev1 <- input$plotEVs[1]
          ev2 <- input$plotEVs[2]
          ggplot(df_plot, aes_string(x = ev1, y = ev2, fill = "Prediction")) +
            geom_tile() +
            labs(title = "Prediction Contour Plot",
                 x = ev1, y = ev2) + theme_minimal()
        }
      })
      
      output$acquisitionplot <- renderPlot({
        req(optimization_results$acquisitionplot_data)
        df_plot <- optimization_results$acquisitionplot_data
        acqFuncName <- switch(input$acqFunc,
                              "EI" = "Expected Improvement",
                              "PI" = "Probability of Improvement",
                              "UCB" = "Upper Confidence Bound")
        if (length(input$plotEVs) == 1) {
          ev_name <- input$plotEVs[1]
          if (is.numeric(filtered_data()[[ev_name]])) {
            ggplot(df_plot, aes_string(x = ev_name, y = "Acquisition")) +
              geom_line() +
              labs(title = paste(acqFuncName, "Plot"),
                   x = ev_name, y = acqFuncName) + theme_minimal()
          } else {
            ggplot(df_plot, aes_string(x = ev_name, y = "Acquisition")) +
              geom_point(size = 3) +
              labs(title = paste(acqFuncName, "Plot (Categorical Explanatory Variable)"),
                   x = ev_name, y = acqFuncName) + theme_minimal()
          }
        } else if (length(input$plotEVs) == 2) {
          ev1 <- input$plotEVs[1]
          ev2 <- input$plotEVs[2]
          ggplot(df_plot, aes_string(x = ev1, y = ev2, fill = "Acquisition")) +
            geom_tile() +
            labs(title = paste(acqFuncName, "Contour Plot"),
                 x = ev1, y = ev2) + theme_minimal()
        }
      })
      
      incProgress(1, detail = "Completed")
    })
  })
  
  output$predictionUI <- renderUI({
    tagList(
      h3("Prediction Mode"),
      hr(),
      radioButtons("pred_mode", "Select Prediction Mode", 
                   choices = c("Manual Input", "CSV Upload"), selected = "Manual Input", inline = TRUE),
      conditionalPanel(
        condition = "input.pred_mode == 'Manual Input'",
        h4("Manual Prediction"),
        rHandsontableOutput("evInputTable"),
        actionButton("addRow", "Add Row"),
        br(),
        actionButton("predictButton", "Execute Prediction (Manual)", class = "btn-primary")
      ),
      conditionalPanel(
        condition = "input.pred_mode == 'CSV Upload'",
        h4("Batch Prediction via CSV Upload"),
        fileInput("pred_csv", "Upload CSV File for Prediction",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        actionButton("predictCSVButton", "Execute Prediction (CSV)", class = "btn-primary"),
        downloadButton("downloadPred", "Download Prediction Result"),
        h4("CSV File Preview (First 20 Rows):"),
        DTOutput("pred_csv_preview")
      )
    )
  })
  
  ev_input_df <- reactiveVal()
  observeEvent(input$evs, {
    evs <- input$evs
    if (length(evs) > 0) {
      df <- data.frame(matrix(ncol = length(evs) + 3, nrow = 1))
      colnames(df) <- c(evs, 'Predicted_OV', 'Lower_CI', 'Upper_CI')
      ev_input_df(df)
    } else {
      ev_input_df(NULL)
    }
  })
  
  output$evInputTable <- renderRHandsontable({
    req(ev_input_df())
    df <- ev_input_df()
    rh <- rhandsontable(df, rowHeaders = NULL)
    if (!is.null(input$evs)) {
      for (ev in input$evs) {
        if (is.numeric(filtered_data()[[ev]])) {
          rh <- hot_col(rh, ev, type = "numeric")
        } else {
          levs <- sort(unique(as.character(filtered_data()[[ev]])))
          rh <- hot_col(rh, ev, type = "dropdown", source = levs, strict = FALSE)
        }
      }
    }
    rh <- hot_col(rh, "Predicted_OV", readOnly = TRUE)
    rh <- hot_col(rh, "Lower_CI", readOnly = TRUE)
    rh <- hot_col(rh, "Upper_CI", readOnly = TRUE)
    rh
  })
  
  observeEvent(input$addRow, {
    df <- ev_input_df()
    if (!is.null(df)) {
      new_row <- df[1, ]
      new_row[] <- NA
      df <- rbind(df, new_row)
      ev_input_df(df)
    }
  })
  
  observeEvent(input$predictButton, {
    req(ev_input_df(), surrogate_model())
    df <- hot_to_r(input$evInputTable)
    evs <- input$evs
    if (any(is.na(df[, evs, drop = FALSE]))) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Please enter all values for the explanatory variables before executing prediction.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    X_manual <- encode_explanatory(df[, evs, drop = FALSE], evs)
    sc_manual <- compute_scaling_info(X_manual)
    X_manual_scaled <- scale_matrix(X_manual, sc_manual)
    
    if (input$model_type == "Gaussian Process") {
      predictions <- predict.GP(surrogate_model(), as.matrix(X_manual_scaled))
      mu <- predictions$Y_hat
      sigma <- sqrt(predictions$MSE)
    } else if (input$model_type == "Random Forest") {
      rf_preds <- predict(surrogate_model(), newdata = as.data.frame(X_manual_scaled), predict.all = TRUE)
      mu <- rowMeans(rf_preds$individual)
      sigma <- apply(rf_preds$individual, 1, sd)
    }
    ci_multiplier <- 1.96
    df$Predicted_OV <- round(mu, 3)
    df$Lower_CI <- round(mu - ci_multiplier * sigma, 3)
    df$Upper_CI <- round(mu + ci_multiplier * sigma, 3)
    ev_input_df(df)
  })
  
  pred_results <- reactiveVal()
  output$pred_csv_preview <- renderDT({
    req(input$pred_csv)
    tryCatch({
      df <- read.csv(input$pred_csv$datapath, header = TRUE, stringsAsFactors = FALSE)
      datatable(head(df, 20))
    }, error = function(e) {
      datatable(data.frame(Error = e$message))
    })
  })
  
  observeEvent(input$predictCSVButton, {
    req(input$pred_csv, surrogate_model(), input$evs)
    df_full <- tryCatch({
      read.csv(input$pred_csv$datapath, header = TRUE, stringsAsFactors = FALSE)
    }, error = function(e) {
      showModal(modalDialog(
        title = "File Error",
        paste("An error occurred while reading the prediction CSV file:", e$message),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    })
    req(df_full)
    required_evs <- input$evs
    found_evs <- intersect(names(df_full), required_evs)
    if(length(found_evs) != length(required_evs)) {
      showModal(modalDialog(
        title = "Required Columns Not Found",
        paste("The uploaded CSV file does not contain the required explanatory variables. Missing columns:",
              paste(setdiff(required_evs, found_evs), collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    df_subset <- df_full[, required_evs, drop = FALSE]
    X_csv <- encode_explanatory(df_subset, required_evs)
    sc_csv <- compute_scaling_info(X_csv)
    X_csv_scaled <- scale_matrix(X_csv, sc_csv)
    
    if (input$model_type == "Gaussian Process") {
      predictions <- predict.GP(surrogate_model(), as.matrix(X_csv_scaled))
      mu <- predictions$Y_hat
      sigma <- sqrt(predictions$MSE)
    } else if (input$model_type == "Random Forest") {
      rf_preds <- predict(surrogate_model(), newdata = as.data.frame(X_csv_scaled), predict.all = TRUE)
      mu <- rowMeans(rf_preds$individual)
      sigma <- apply(rf_preds$individual, 1, sd)
    }
    ci_multiplier <- 1.96
    df_full$Predicted_OV <- round(mu, 3)
    df_full$Lower_CI <- round(mu - ci_multiplier * sigma, 3)
    df_full$Upper_CI <- round(mu + ci_multiplier * sigma, 3)
    pred_results(df_full)
    showModal(modalDialog(
      title = "Prediction Completed",
      "Batch prediction via CSV is complete. Please download the results.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$downloadPred <- downloadHandler(
    filename = function() {
      paste("prediction_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(pred_results())
      write.csv(pred_results(), file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)
