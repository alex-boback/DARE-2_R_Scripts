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
          h4("Response status"),
          tableOutput("quality"),
          plotOutput("chart", height = "350px"),
          uiOutput("summary_heading"),
          textOutput("summary_note"),
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
          checkboxGroupInput("report_questions", "Questions to include", choices = NULL),
          actionButton("report_select_all", "Select all"),
          actionButton("report_clear_selection", "Clear selection"),
          helpText("Each selected question includes a basic summary. Open its settings for optional content."),
          h4("Question settings"),
          uiOutput("report_settings"),
          br(), br(),
          downloadButton("download_report", "Download HTML report")
        ),
        column(8,
          h4("Included questions"),
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
      updateCheckboxGroupInput(session, "report_questions", choices = questions,
                               selected = intersect(input$report_questions, questions))
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
    updateCheckboxGroupInput(session, "report_questions", choices = character(),
                             selected = character())
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

  output$summary <- renderTable(present_question_summary(view_frame()), digits = 2)
  output$summary_heading <- renderUI({
    if (view_frame()$type[1] == "multiselect")
      h4("Option counts") else h4("Summary")
  })
  output$summary_note <- renderText({
    if (view_frame()$type[1] == "multiselect")
      "Counts are per option, once per respondent. Percentages can add above 100%."
  })
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

  report_questions_available <- reactive({
    surveys <- loaded()
    unique(unlist(lapply(surveys, function(x)
      names(x$types)[x$types != "group key"])))
  })

  observeEvent(input$report_select_all, {
    updateCheckboxGroupInput(session, "report_questions",
      selected = report_questions_available())
  })
  observeEvent(input$report_clear_selection, {
    updateCheckboxGroupInput(session, "report_questions", selected = character())
  })

  output$report_settings <- renderUI({
    surveys <- loaded()
    questions <- report_questions_available()
    tagList(lapply(seq_along(questions), function(i) {
      question <- questions[i]
      options <- group_key_options(surveys, question)
      keys <- default_group_keys(surveys, question)
      frame <- analysis_frame(surveys, question, keys)
      type <- frame$type[1]
      groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
      tests <- available_tests(type, length(groups))
      extras <- c("Response status counts" = "counts", "Graph" = "graph")
      if (type == "free response")
        extras <- c(extras, "Free responses" = "free")
      else if (length(tests))
        extras <- c(extras, "Statistical test" = "test")
      choices <- if (type == "multiselect")
        sort(unique(unlist(frame$values[frame$status == "valid"]))) else NULL
      tags$details(
        style = "border:1px solid #ddd;padding:8px;margin-bottom:8px",
        tags$summary(paste(question, "(", type, ")")),
        selectInput(paste0("report_keys_", i), "Group by", choices = options,
                    selected = keys, multiple = TRUE),
        selectInput(paste0("report_group_", i), "Show responses from",
                    choices = c("All groups" = "__all_groups__",
                                stats::setNames(groups, groups))),
        checkboxGroupInput(paste0("report_sections_", i), "Also include",
                           choices = extras),
        selectInput(paste0("report_graph_", i), "Graph",
                    choices = graph_options(type)),
        if (length(tests)) selectInput(paste0("report_test_", i), "Test",
                                      choices = tests),
        if (length(choices)) selectInput(paste0("report_choice_", i),
                                        "Multiselect option to test", choices = choices),
        textAreaInput(paste0("report_note_", i), "Notes", rows = 2)
      )
    }))
  })

  observe({
    surveys <- loaded()
    questions <- report_questions_available()
    for (i in seq_along(questions)) {
      options <- group_key_options(surveys, questions[i])
      keys <- input[[paste0("report_keys_", i)]]
      if (!length(keys) || !all(keys %in% unname(options)))
        keys <- default_group_keys(surveys, questions[i])
      frame <- analysis_frame(surveys, questions[i], keys)
      groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
      updateSelectInput(session, paste0("report_group_", i),
        choices = c("All groups" = "__all_groups__", stats::setNames(groups, groups)),
        selected = "__all_groups__")
      tests <- available_tests(frame$type[1], length(groups))
      updateSelectInput(session, paste0("report_test_", i), choices = tests,
        selected = if (length(tests)) unname(tests[1]) else character())
    }
  })

  report_entries <- reactive({
    surveys <- loaded()
    questions <- report_questions_available()
    selected <- intersect(questions, input$report_questions)
    lapply(which(questions %in% selected), function(i) {
      question <- questions[i]
      options <- group_key_options(surveys, question)
      keys <- input[[paste0("report_keys_", i)]]
      if (!length(keys) || !all(keys %in% unname(options)))
        keys <- default_group_keys(surveys, question)
      frame <- analysis_frame(surveys, question, keys)
      type <- frame$type[1]
      groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
      group <- input[[paste0("report_group_", i)]]
      if (!length(group) || !group %in% c("__all_groups__", groups))
        group <- "__all_groups__"
      sections <- input[[paste0("report_sections_", i)]]
      tests <- available_tests(type, length(groups))
      test <- input[[paste0("report_test_", i)]]
      if (!length(test) || !test %in% unname(tests))
        test <- if (length(tests)) unname(tests[1]) else NULL
      graphs <- graph_options(type)
      graph <- input[[paste0("report_graph_", i)]]
      if (!length(graph) || !graph %in% unname(graphs))
        graph <- if (length(graphs)) unname(graphs[1]) else NULL
      choice <- input[[paste0("report_choice_", i)]]
      if (type == "multiselect") {
        choices <- sort(unique(unlist(frame$values[frame$status == "valid"])))
        if (!length(choice) || !choice %in% choices)
          choice <- if (length(choices)) choices[1] else NULL
        if (is.null(choice)) sections <- setdiff(sections, "test")
      }
      if (is.null(test)) sections <- setdiff(sections, "test")
      list(question = question, group_keys = keys, group = group,
           sections = sections, graph = graph, test = test, choice = choice,
           note = if (is.null(input[[paste0("report_note_", i)]])) "" else
             trimws(input[[paste0("report_note_", i)]]))
    })
  })

  output$report_list <- renderTable({
    entries <- report_entries()
    if (!length(entries)) return(NULL)
    do.call(rbind, lapply(seq_along(entries), function(i) {
      x <- entries[[i]]
      data.frame(order = i, question = x$question,
        group_by = report_group_label(x$group_keys),
        group = if (identical(x$group, "__all_groups__")) "All groups" else x$group,
        included = paste(c("Basic summary", x$sections), collapse = ", "))
    }))
  })

  output$report_preview <- renderUI({
    entries <- report_entries()
    if (!length(entries)) return(p("Select questions to preview the report."))
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
