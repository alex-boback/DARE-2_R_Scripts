library(shiny)
source("survey_app_core.R")

ui <- fluidPage(
  titlePanel("Survey comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput("period1", "Period 1 spreadsheet", accept = c(".csv", ".xls", ".xlsx")),
      fileInput("period2", "Period 2 spreadsheet", accept = c(".csv", ".xls", ".xlsx")),
      textInput("sheet1", "Period 1 Excel sheet", value = "1"),
      textInput("sheet2", "Period 2 Excel sheet", value = "1"),
      tags$hr(),
      fileInput("mapping_file", "Optional mapping spreadsheet",
                accept = c(".csv", ".xls", ".xlsx")),
      selectInput("mapping_question", "Edit mappings for question", choices = NULL),
      selectInput("mapping_period", "Mapping applies to",
                  choices = c("All", "Period 1", "Period 2")),
      textAreaInput("mapping_text", "UI mapping (one raw => mapped per line)",
                    rows = 4, placeholder = "1 => Strongly disagree"),
      actionButton("save_mapping", "Save UI mapping"),
      textOutput("mapping_saved"),
      tags$hr(),
      textInput("delimiter", "Multiselect delimiter", value = ";"),
      selectInput("question", "Question to analyze", choices = NULL),
      radioButtons("comparison", "Compare", c("Uploaded files", "Group key values"),
                   selected = "Uploaded files"),
      conditionalPanel("input.comparison == 'Group key values'",
        selectInput("group_question", "Group key", choices = NULL)),
      selectInput("test", "Statistical test", choices = NULL),
      uiOutput("choice_ui"),
      actionButton("run_test", "Run test", class = "btn-primary")
    ),
    mainPanel(
      h3("Question overview"),
      tableOutput("quality"),
      plotOutput("chart", height = "350px"),
      tableOutput("summary"),
      tableOutput("numeric_summary"),
      h3("Statistical result"),
      verbatimTextOutput("test_result"),
      h3("Free responses"),
      tableOutput("free_responses"),
      textOutput("status")
    )
  )
)

server <- function(input, output, session) {
  saved_mappings <- reactiveVal(empty_mapping())
  mapping_note <- reactiveVal("")

  surveys <- reactive({
    req(input$period1, input$period2)
    sheet <- function(x) if (grepl("^[0-9]+$", x)) as.integer(x) else x
    first <- read_two_row_survey(input$period1$datapath,
                                 input$period1$name, sheet(input$sheet1))
    second <- read_two_row_survey(input$period2$datapath,
                                  input$period2$name, sheet(input$sheet2))
    common <- intersect(names(first$types), names(second$types))
    if (!length(common)) stop("The two files have no question names in common.")
    mismatch <- common[first$types[common] != second$types[common]]
    if (length(mismatch))
      stop("Question types differ between files: ", paste(mismatch, collapse = ", "))
    list(first, second)
  })

  observeEvent(surveys(), {
    common <- intersect(names(surveys()[[1]]$types),
                        names(surveys()[[2]]$types))
    updateSelectInput(session, "question", choices = common,
                      selected = common[1])
    updateSelectInput(session, "mapping_question", choices = common,
                      selected = common[1])
    keys <- common[surveys()[[1]]$types[common] == "group key"]
    updateSelectInput(session, "group_question", choices = keys)
  })

  file_mappings <- reactive({
    if (is.null(input$mapping_file)) return(empty_mapping())
    read_mapping_file(input$mapping_file$datapath, input$mapping_file$name)
  })

  observeEvent(input$save_mapping, {
    req(input$mapping_question)
    tryCatch({
      new <- widget_mapping(input$mapping_text, input$mapping_question,
                            input$mapping_period)
      old <- saved_mappings()
      old <- old[!(old$question == input$mapping_question &
                     old$period == input$mapping_period), , drop = FALSE]
      saved_mappings(rbind(old, new))
      mapping_note(paste(nrow(new), "UI mapping rows saved for",
                         input$mapping_question, "in", input$mapping_period))
    }, error = function(e) mapping_note(conditionMessage(e)))
  })
  output$mapping_saved <- renderText(mapping_note())

  mappings <- reactive({
    file <- file_mappings()
    ui <- saved_mappings()
    file$source <- rep("file", nrow(file))
    ui$source <- rep("ui", nrow(ui))
    rbind(file, ui)
  })

  current_frame <- reactive({
    req(surveys(), input$question)
    if (!nzchar(input$delimiter)) stop("Multiselect delimiter cannot be blank.")
    mode <- if (input$comparison == "Uploaded files") "Period" else "Group"
    analysis_frame(surveys(), input$question, mode, input$group_question,
                   mappings(), input$delimiter)
  })

  observeEvent(current_frame(), {
    frame <- current_frame()
    groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
    choices <- available_tests(frame$type[1], length(groups))
    updateSelectInput(session, "test", choices = choices,
                      selected = if (length(choices)) choices[1] else character())
  })

  output$choice_ui <- renderUI({
    frame <- current_frame()
    if (frame$type[1] != "multiselect") return(NULL)
    rows <- frame$values[frame$status == "valid"]
    selectInput("choice", "Multiselect choice to test",
                choices = sort(unique(unlist(rows))))
  })

  output$quality <- renderTable({
    frame <- current_frame()
    as.data.frame.matrix(table(Group = frame$group, Status = frame$status,
                               useNA = "ifany"))
  }, rownames = TRUE)

  output$summary <- renderTable(summarize_question(current_frame()),
                                digits = 2)
  output$numeric_summary <- renderTable(summarize_numeric(current_frame()),
                                        digits = 3)

  output$chart <- renderPlot({
    frame <- current_frame()
    if (frame$type[1] == "continuous") {
      valid <- frame[frame$status == "valid" & !is.na(frame$group), , drop = FALSE]
      validate(need(nrow(valid), "No valid responses to plot."))
      y <- as.numeric(unlist(valid$values))
      boxplot(y ~ valid$group, xlab = "Group", ylab = input$question,
              col = "#78b5ad")
      return()
    }
    summary <- summarize_question(current_frame())
    validate(need(nrow(summary), "No valid responses to plot."))
    groups <- unique(summary$group)
    responses <- unique(summary$response)
    values <- matrix(0, nrow = length(responses), ncol = length(groups),
                     dimnames = list(responses, groups))
    is_free <- frame$type[1] == "free response"
    for (i in seq_len(nrow(summary)))
      values[summary$response[i], summary$group[i]] <-
        if (is_free) summary$n[i] else summary$percent[i]
    if (current_frame()$type[1] == "likert") {
      barplot(values, beside = FALSE, col = c("#b45050", "#d99577", "#d4d4d4",
                                             "#78b5ad", "#247f7a"),
              legend.text = rownames(values), xlab = "Group",
              ylab = "Percent of valid responses", ylim = c(0, 100))
    } else {
      barplot(values, beside = TRUE, las = 2,
              col = grDevices::hcl.colors(nrow(values), "Set 2"),
              legend.text = rownames(values), xlab = "Group",
              ylab = if (is_free) "Responses" else "Percent of valid responses")
    }
  })

  result_state <- reactiveVal(NULL)
  observeEvent(current_frame(), result_state(NULL), ignoreInit = TRUE)
  observeEvent(input$run_test, {
    result_state(tryCatch({
      frame <- current_frame()
      req(input$test)
      if (!input$test %in% available_tests(frame$type[1],
           length(unique(frame$group[frame$status == "valid" & !is.na(frame$group)]))))
        stop("This test is not available for the current question and groups.")
      run_question_test(frame, input$test, input$choice)
    }, error = function(e) list(error = conditionMessage(e))))
  })

  output$test_result <- renderPrint({
    if (current_frame()$type[1] == "free response") {
      cat("Free response: descriptive review only.")
      return()
    }
    x <- result_state()
    if (is.null(x)) {
      cat("Choose a test and click Run test.")
      return()
    }
    if (!is.null(x$error)) {
      cat(x$error)
      return()
    }
    if (!is.null(x$pairwise)) {
      cat(x$method, "\n")
      print(x$pairwise)
      cat(x$note, "\n")
      return()
    }
    cat(x$method, "\nStatistic:", signif(x$statistic, 4),
        "\np-value:", signif(x$p_value, 4), "\n")
    if (!is.null(x$df) && !all(is.na(x$df)))
      cat("Degrees of freedom:", paste(signif(x$df, 4), collapse = ", "), "\n")
    if (nzchar(x$note)) cat(x$note, "\n")
  })

  output$free_responses <- renderTable({
    frame <- current_frame()
    if (frame$type[1] != "free response") return(NULL)
    valid <- frame[frame$status == "valid", , drop = FALSE]
    data.frame(period = valid$period, row = valid$row,
               response = vapply(valid$values, `[[`, "", 1))
  })

  output$status <- renderText({
    tryCatch({
      surveys()
      paste("Loaded", nrow(surveys()[[1]]$data), "Period 1 rows and",
            nrow(surveys()[[2]]$data), "Period 2 rows.")
    }, error = function(e) conditionMessage(e))
  })
}

shinyApp(ui, server)
