library(shiny)
source("survey_app_core.R")
source("survey_report.R")

ui <- fluidPage(
  titlePanel("Survey analysis"),
  tabsetPanel(
    id = "workflow",
    tabPanel(
      "Load data",
      br(),
      fileInput("files", "Upload survey files", multiple = TRUE,
                accept = c(".csv", ".xls", ".xlsx")),
      helpText("Each CSV or Excel worksheet has question names in row 1, types in row 2, and responses below. Mark one or more columns as group key."),
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
          selectInput("group_keys", "Group by", choices = NULL, multiple = TRUE),
          selectInput("view_group", "Show responses from",
                      choices = c("All groups" = "__all_groups__")),
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
          tableOutput("ordered_summary"),
          h4("Statistical result"),
          verbatimTextOutput("test_result"),
          uiOutput("free_heading"),
          tableOutput("free_responses")
        )
      )
    ),
    tabPanel(
      "Report",
      br(),
      fluidRow(
        column(4,
          textInput("report_title", "Report title", value = "Survey analysis report"),
          selectInput("report_question", "Question", choices = NULL),
          textOutput("report_type"),
          selectInput("report_group_keys", "Group by", choices = NULL,
                      multiple = TRUE),
          selectInput("report_group", "Show responses from",
                      choices = c("All groups" = "__all_groups__")),
          uiOutput("report_sections_ui"),
          selectInput("report_graph", "Graph to include", choices = NULL),
          selectInput("report_test", "Test to include", choices = NULL),
          uiOutput("report_choice_ui"),
          textAreaInput("report_note", "Notes for this question", rows = 3),
          helpText("The group choice filters descriptive sections. Tests use all loaded groups."),
          actionButton("add_report", "Add question section", class = "btn-primary"),
          textOutput("report_message"),
          tags$hr(),
          selectInput("remove_report_item", "Added section to remove", choices = NULL),
          actionButton("remove_report", "Remove selected"),
          br(), br(),
          downloadButton("download_report", "Download HTML report")
        ),
        column(8,
          h4("Included sections"),
          tableOutput("report_list"),
          h4("Report preview"),
          tags$style(".survey-report img{max-width:100%;height:auto}.survey-report section{border-top:1px solid #ccc;padding:16px 0}.survey-report pre{white-space:pre-wrap}.survey-report table{border-collapse:collapse;width:100%;margin:12px 0}.survey-report th,.survey-report td{border:1px solid #ccc;padding:6px 9px;text-align:left}"),
          uiOutput("report_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_state <- reactiveVal(list(surveys = NULL, loaded_paths = character(),
                                 error = NULL))
  report_entries <- reactiveVal(list())
  report_message <- reactiveVal("")
  report_title <- reactive({
    title <- input$report_title
    if (is.null(title) || !nzchar(trimws(title)))
      "Survey analysis report" else trimws(title)
  })

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
      updateSelectInput(session, "report_question", choices = questions,
                        selected = if (length(questions)) questions[1] else character())
      updateTabsetPanel(session, "workflow", selected = "Analyze")
    }
  })

  observeEvent(input$clear_data, {
    data_state(list(surveys = NULL, loaded_paths = character(), error = NULL))
    updateSelectInput(session, "question", choices = character())
    updateSelectInput(session, "group_keys", choices = character())
    updateSelectInput(session, "test", choices = character())
    updateSelectInput(session, "view_group",
                      choices = c("All groups" = "__all_groups__"),
                      selected = "__all_groups__")
    updateSelectInput(session, "graph", choices = character())
    updateSelectInput(session, "report_question", choices = character())
    updateSelectInput(session, "report_group_keys", choices = character())
    updateSelectInput(session, "report_group",
                      choices = c("All groups" = "__all_groups__"),
                      selected = "__all_groups__")
    updateSelectInput(session, "report_graph", choices = character())
    updateSelectInput(session, "report_test", choices = character())
    report_entries(list())
    report_message("")
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
      data.frame(source = x$source, rows = nrow(x$data),
                 group_keys = paste(x$keys, collapse = ", "))
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

  observeEvent(list(loaded(), input$question), {
    req(input$question)
    options <- group_key_options(loaded(), input$question)
    updateSelectInput(session, "group_keys", choices = options,
                      selected = if (length(options)) unname(options[1]) else character())
  })

  current_frame <- reactive({
    req(loaded(), input$question)
    options <- group_key_options(loaded(), input$question)
    selected <- input$group_keys
    if (!length(selected) || !all(selected %in% unname(options)))
      selected <- default_group_keys(loaded(), input$question)
    analysis_frame(loaded(), input$question, selected)
  })

  output$question_type <- renderText({
    paste("Question type:", current_frame()$type[1])
  })

  observeEvent(current_frame(), {
    frame <- current_frame()
    groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
    tests <- available_tests(frame$type[1], length(groups))
    updateSelectInput(session, "view_group",
                      choices = c("All groups" = "__all_groups__",
                                  stats::setNames(groups, groups)),
                      selected = "__all_groups__")
    graphs <- graph_options(frame$type[1])
    updateSelectInput(session, "graph", choices = graphs,
                      selected = if (length(graphs)) graphs[1] else character())
    updateSelectInput(session, "test", choices = tests,
                      selected = if (length(tests)) tests[1] else character())
  })

  view_frame <- reactive({
    frame <- current_frame()
    if (!is.null(input$view_group) &&
        !identical(input$view_group, "__all_groups__") &&
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
  output$ordered_summary <- renderTable(summarize_ordered(view_frame()),
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
    if (is.null(x)) cat("Choose a test and click Run test.")
    else cat(report_test_text(x))
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

  observeEvent(list(loaded(), input$report_question), {
    req(input$report_question)
    options <- group_key_options(loaded(), input$report_question)
    updateSelectInput(session, "report_group_keys", choices = options,
                      selected = if (length(options)) unname(options[1]) else character())
  })

  report_keys <- reactive({
    req(loaded(), input$report_question)
    options <- group_key_options(loaded(), input$report_question)
    selected <- input$report_group_keys
    if (!length(selected) || !all(selected %in% unname(options)))
      selected <- default_group_keys(loaded(), input$report_question)
    selected
  })

  report_frame <- reactive({
    analysis_frame(loaded(), input$report_question, report_keys())
  })

  output$report_type <- renderText({
    paste("Question type:", report_frame()$type[1])
  })

  observeEvent(report_frame(), {
    frame <- report_frame()
    groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
    updateSelectInput(session, "report_group",
      choices = c("All groups" = "__all_groups__", stats::setNames(groups, groups)),
      selected = "__all_groups__")
    graphs <- graph_options(frame$type[1])
    updateSelectInput(session, "report_graph", choices = graphs,
                      selected = if (length(graphs)) graphs[1] else character())
    tests <- available_tests(frame$type[1], length(groups))
    updateSelectInput(session, "report_test", choices = tests,
                      selected = if (length(tests)) tests[1] else character())
  })

  output$report_sections_ui <- renderUI({
    type <- report_frame()$type[1]
    sections <- c("Response counts" = "counts", "Summary" = "summary",
                  "Graph" = "graph")
    if (type == "free response")
      sections <- c(sections, "Free responses" = "free")
    else
      sections <- c(sections, "Statistical test" = "test")
    checkboxGroupInput("report_sections", "Include",
      choices = sections,
      selected = if (type == "free response")
        c("counts", "graph", "free") else c("counts", "summary", "graph", "test"))
  })

  output$report_choice_ui <- renderUI({
    frame <- report_frame()
    if (frame$type[1] != "multiselect") return(NULL)
    values <- frame$values[frame$status == "valid"]
    selectInput("report_choice", "Multiselect choice to test",
                choices = sort(unique(unlist(values))))
  })

  observeEvent(input$add_report, {
    tryCatch({
      frame <- report_frame()
      sections <- input$report_sections
      if (!length(sections)) stop("Select at least one section.")
      selected_group <- if (is.null(input$report_group))
        "__all_groups__" else input$report_group
      groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
      if (!selected_group %in% c("__all_groups__", groups))
        stop("Select a group for this question.")
      if ("graph" %in% sections &&
          !input$report_graph %in% graph_options(frame$type[1]))
        stop("Select a graph for this question.")
      if ("test" %in% sections) {
        if (is.null(input$report_test) ||
            !input$report_test %in% available_tests(frame$type[1], length(groups)))
          stop("Select an available test for this question.")
        if (frame$type[1] == "multiselect" &&
            (is.null(input$report_choice) || !nzchar(input$report_choice)))
          stop("Select a multiselect choice to test.")
      }
      entry <- list(
        question = input$report_question,
        group_keys = report_keys(),
        group = selected_group,
        sections = sections,
        graph = input$report_graph,
        test = input$report_test,
        choice = input$report_choice,
        note = if (is.null(input$report_note)) "" else trimws(input$report_note)
      )
      report_entries(c(report_entries(), list(entry)))
      report_message(paste("Added", entry$question, "to the report."))
    }, error = function(e) report_message(conditionMessage(e)))
  })
  output$report_message <- renderText(report_message())

  observeEvent(report_entries(), {
    entries <- report_entries()
    labels <- vapply(seq_along(entries), function(i)
      paste(i, entries[[i]]$question, if (identical(entries[[i]]$group,
            "__all_groups__")) "(all groups)" else paste0("(", entries[[i]]$group, ")")),
      "")
    updateSelectInput(session, "remove_report_item",
                      choices = stats::setNames(as.character(seq_along(entries)),
                                                labels))
  })

  observeEvent(input$remove_report, {
    index <- suppressWarnings(as.integer(input$remove_report_item))
    entries <- report_entries()
    if (length(index) == 1 && !is.na(index) && index %in% seq_along(entries)) {
      report_entries(entries[-index])
      report_message("Selected section removed.")
    }
  })

  output$report_list <- renderTable({
    entries <- report_entries()
    if (!length(entries)) return(NULL)
    do.call(rbind, lapply(seq_along(entries), function(i) {
      x <- entries[[i]]
      data.frame(order = i, question = x$question,
        group_by = report_group_label(x$group_keys),
        group = if (identical(x$group, "__all_groups__")) "All groups" else x$group,
        included = paste(x$sections, collapse = ", "))
    }))
  })

  output$report_preview <- renderUI({
    entries <- report_entries()
    if (!length(entries)) return(p("Add a question section to preview the report."))
    HTML(report_body_html(entries, loaded(), report_title()))
  })

  output$download_report <- downloadHandler(
    filename = function() "survey-analysis-report.html",
    content = function(file) {
      entries <- report_entries()
      req(length(entries))
      html <- report_document_html(entries, loaded(), report_title())
      writeLines(enc2utf8(html), file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
