library(shiny)
source("survey_app_core.R")

ui <- fluidPage(
  titlePanel("Survey analysis"),
  tabsetPanel(
    id = "workflow",
    tabPanel(
      "Load data",
      br(),
      fileInput("files", "Upload survey files", multiple = TRUE,
                accept = c(".csv", ".xls", ".xlsx")),
      helpText("Each CSV or Excel worksheet has question names in row 1, types in row 2, and responses below. Every sheet needs one group key column containing its time-period labels."),
      actionButton("load_data", "Add uploaded data", class = "btn-primary"),
      actionButton("clear_data", "Clear loaded data"),
      br(), br(),
      textOutput("load_status"),
      h4("Loaded sources"),
      tableOutput("sources"),
      h4("Available questions"),
      tableOutput("catalog")
    ),
    tabPanel(
      "Analyze",
      br(),
      fluidRow(
        column(4,
          selectInput("question", "Question", choices = NULL),
          textOutput("question_type"),
          selectInput("view_group", "Show responses from",
                      choices = c("All groups" = "")),
          selectInput("graph", "Graph", choices = NULL),
          helpText("The group filter changes the overview. Tests compare all loaded groups."),
          selectInput("test", "Statistical test", choices = NULL),
          uiOutput("choice_ui"),
          actionButton("run_test", "Run test", class = "btn-primary")
        ),
        column(8,
          h4("Response counts"),
          tableOutput("quality"),
          plotOutput("chart", height = "350px"),
          h4("Summary"),
          tableOutput("summary"),
          tableOutput("numeric_summary"),
          h4("Statistical result"),
          verbatimTextOutput("test_result"),
          uiOutput("free_heading"),
          tableOutput("free_responses")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_state <- reactiveVal(list(surveys = NULL, loaded_paths = character(),
                                 error = NULL))

  observeEvent(input$load_data, {
    previous <- data_state()
    files <- input$files
    new_files <- if (is.null(files)) NULL else
      files[!files$datapath %in% previous$loaded_paths, , drop = FALSE]
    state <- tryCatch({
      surveys <- load_survey_files(
        new_files, if (is.null(previous$surveys)) list() else previous$surveys)
      list(surveys = surveys,
           loaded_paths = c(previous$loaded_paths, new_files$datapath),
           error = NULL)
    }, error = function(e)
      list(surveys = previous$surveys,
           loaded_paths = previous$loaded_paths,
           error = conditionMessage(e)))
    data_state(state)
    if (is.null(state$error)) {
      questions <- unique(unlist(lapply(state$surveys, function(x)
        names(x$types)[x$types != "group key"])))
      updateSelectInput(session, "question", choices = questions,
                        selected = if (length(questions)) questions[1] else character())
      updateTabsetPanel(session, "workflow", selected = "Analyze")
    }
  })

  observeEvent(input$clear_data, {
    data_state(list(surveys = NULL, loaded_paths = character(), error = NULL))
    updateSelectInput(session, "question", choices = character())
    updateSelectInput(session, "test", choices = character())
    updateSelectInput(session, "view_group", choices = c("All groups" = ""))
    updateSelectInput(session, "graph", choices = character())
    updateTabsetPanel(session, "workflow", selected = "Load data")
  })

  loaded <- reactive({
    state <- data_state()
    req(state$surveys)
    state$surveys
  })

  output$load_status <- renderText({
    state <- data_state()
    if (!is.null(state$error)) return(state$error)
    if (is.null(state$surveys)) return("Upload files, then click Load data.")
    paste(length(state$surveys), "sheets loaded;",
          sum(vapply(state$surveys, function(x) nrow(x$data), integer(1))),
          "response rows.")
  })

  output$sources <- renderTable({
    surveys <- loaded()
    do.call(rbind, lapply(surveys, function(x) {
      periods <- unique(trimws(x$data[[x$key]]))
      periods <- periods[!is.na(periods) & nzchar(periods)]
      data.frame(source = x$source, rows = nrow(x$data),
                 group_key = x$key, periods = paste(periods, collapse = ", "))
    }))
  })

  output$catalog <- renderTable({
    surveys <- loaded()
    questions <- unique(unlist(lapply(surveys, function(x)
      names(x$types)[x$types != "group key"])))
    do.call(rbind, lapply(questions, function(question) {
      present <- vapply(surveys, function(x) question %in% names(x$types), logical(1))
      data.frame(question = question,
                 type = surveys[[which(present)[1]]]$types[[question]],
                 sheets = sum(present))
    }))
  })

  current_frame <- reactive({
    req(loaded(), input$question)
    analysis_frame(loaded(), input$question)
  })

  output$question_type <- renderText({
    paste("Question type:", current_frame()$type[1])
  })

  observeEvent(current_frame(), {
    frame <- current_frame()
    groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
    tests <- available_tests(frame$type[1], length(groups))
    updateSelectInput(session, "view_group",
                      choices = c("All groups" = "", stats::setNames(groups, groups)),
                      selected = "")
    graphs <- graph_options(frame$type[1])
    updateSelectInput(session, "graph", choices = graphs,
                      selected = if (length(graphs)) graphs[1] else character())
    updateSelectInput(session, "test", choices = tests,
                      selected = if (length(tests)) tests[1] else character())
  })

  view_frame <- reactive({
    frame <- current_frame()
    if (!is.null(input$view_group) && nzchar(input$view_group) &&
        input$view_group %in% frame$group)
      frame <- frame[!is.na(frame$group) & frame$group == input$view_group,
                     , drop = FALSE]
    frame
  })

  output$choice_ui <- renderUI({
    frame <- current_frame()
    if (frame$type[1] != "multiselect") return(NULL)
    rows <- frame$values[frame$status == "valid"]
    selectInput("choice", "Multiselect choice to test",
                choices = sort(unique(unlist(rows))))
  })

  output$quality <- renderTable({
    frame <- view_frame()
    as.data.frame.matrix(table(Group = frame$group, Status = frame$status,
                               useNA = "ifany"))
  }, rownames = TRUE)

  output$summary <- renderTable(summarize_question(view_frame()), digits = 2)
  output$numeric_summary <- renderTable(summarize_numeric(view_frame()),
                                        digits = 3)

  output$chart <- renderPlot({
    frame <- view_frame()
    validate(need(any(frame$status == "valid" & !is.na(frame$group)),
                  "No valid responses to plot."))
    req(input$graph)
    validate(need(input$graph %in% graph_options(frame$type[1]),
                  "Choose a graph for this question."))
    plot_question(frame, input$graph, input$question)
  })

  result_state <- reactiveVal(NULL)
  observeEvent(current_frame(), result_state(NULL), ignoreInit = TRUE)
  observeEvent(input$test, result_state(NULL), ignoreInit = TRUE)
  observeEvent(input$choice, result_state(NULL), ignoreInit = TRUE)
  observeEvent(input$run_test, {
    result_state(tryCatch({
      frame <- current_frame()
      req(input$test)
      groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
      if (!input$test %in% available_tests(frame$type[1], length(groups)))
        stop("This test is not available for the current question.")
      run_question_test(frame, input$test, input$choice)
    }, error = function(e) list(error = conditionMessage(e))))
  })

  output$test_result <- renderPrint({
    frame <- current_frame()
    if (frame$type[1] == "free response") {
      cat("Free response: descriptive review only.")
      return()
    }
    x <- result_state()
    if (is.null(x)) {
      cat("Choose a test and click Run test.")
    } else if (!is.null(x$error)) {
      cat(x$error)
    } else if (!is.null(x$pairwise)) {
      cat(x$method, "\n")
      print(x$pairwise)
      cat(x$note, "\n")
    } else {
      cat(x$method, "\nStatistic:", signif(x$statistic, 4),
          "\np-value:", signif(x$p_value, 4), "\n")
      if (!is.null(x$df) && !all(is.na(x$df)))
        cat("Degrees of freedom:", paste(signif(x$df, 4), collapse = ", "), "\n")
      if (nzchar(x$note)) cat(x$note, "\n")
    }
  })

  output$free_heading <- renderUI({
    if (view_frame()$type[1] == "free response") h4("Free responses")
  })
  output$free_responses <- renderTable({
    frame <- view_frame()
    if (frame$type[1] != "free response") return(NULL)
    valid <- frame[frame$status == "valid" & !is.na(frame$group), , drop = FALSE]
    data.frame(source = valid$source, row = valid$row, period = valid$group,
               response = vapply(valid$values, `[[`, "", 1))
  })
}

shinyApp(ui, server)
