# Self-contained HTML report helpers for the Shiny app.

report_test_text <- function(x) {
  if (!is.null(x$error)) return(x$error)
  if (!is.null(x$pairwise))
    return(paste(c(x$method, capture.output(print(x$pairwise)), x$note),
                 collapse = "\n"))
  lines <- x$method
  if (length(x$statistic) && any(is.finite(x$statistic)))
    lines <- c(lines, paste("Statistic:",
                            paste(signif(x$statistic, 4), collapse = ", ")))
  lines <- c(lines, paste("p-value:", signif(x$p_value, 4)))
  if (!is.null(x$df) && !all(is.na(x$df)))
    lines <- c(lines, paste("Degrees of freedom:",
                            paste(signif(x$df, 4), collapse = ", ")))
  if (!is.null(x$note) && nzchar(x$note)) lines <- c(lines, x$note)
  paste(lines, collapse = "\n")
}

report_escape <- function(x) as.character(htmltools::htmlEscape(as.character(x)))

report_group_label <- function(keys) {
  if (identical(keys, "__first_group_key__"))
    "First group key in each sheet" else paste(keys, collapse = ", ")
}

report_table <- function(data) {
  if (is.null(data) || !nrow(data)) return("<p>No responses available.</p>")
  headers <- paste0("<th>", report_escape(names(data)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(data)), function(i) {
    cells <- vapply(seq_len(ncol(data)), function(j) {
      value <- data[i, j, drop = TRUE]
      if (length(value) == 0 || is.na(value)) value <- ""
      paste0("<td>", report_escape(value), "</td>")
    }, "")
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, "")
  paste0("<table><thead><tr>", headers, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}

report_plot_uri <- function(frame, graph, question) {
  if (!requireNamespace("base64enc", quietly = TRUE))
    stop("Install base64enc to include graphs in a report.")
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file))
  grDevices::png(file, width = 1200, height = 720, res = 120)
  tryCatch(plot_question(frame, graph, question),
           finally = grDevices::dev.off())
  paste0("data:image/png;base64,", base64enc::base64encode(file))
}

report_entry_html <- function(entry, surveys) {
  full <- analysis_frame(surveys, entry$question, entry$group_keys)
  view <- full
  if (identical(entry$group, "__tally__"))
    view$group[!is.na(view$group)] <- "All responses"
  else if (!identical(entry$group, "__all_groups__"))
    view <- full[!is.na(full$group) & full$group == entry$group, , drop = FALSE]
  type <- full$type[1]
  section <- c(
    paste0("<section class='question'><h2>", report_escape(entry$question),
           "</h2><p><strong>Type:</strong> ", report_escape(type),
           " &nbsp; <strong>Grouped by:</strong> ",
           report_escape(report_group_label(entry$group_keys)),
           " &nbsp; <strong>View:</strong> ",
           report_escape(if (identical(entry$group, "__tally__"))
             "All responses together" else if (identical(entry$group, "__all_groups__"))
               "Each group" else entry$group), "</p>")
  )
  if (nzchar(entry$note))
    section <- c(section, paste0("<p>", report_escape(entry$note), "</p>"))
  if ("counts" %in% entry$sections) {
    counts <- as.data.frame.matrix(
      table(Group = view$group, Status = view$status, useNA = "ifany"))
    counts <- data.frame(group = rownames(counts), counts,
                         check.names = FALSE, row.names = NULL)
    section <- c(section, "<h3>Response status counts</h3>",
                 report_table(counts))
  }
  section <- c(section, "<h3>Basic summary</h3>")
  if (type == "multiselect")
    section <- c(section,
      "<p>Counts are per option, once per respondent. Percentages can add above 100%.</p>")
  categorical <- present_question_summary(view)
  numeric <- summarize_numeric(view)
  ordered <- summarize_ordered(view)
  if (!is.null(categorical)) section <- c(section, report_table(categorical))
  if (!is.null(numeric)) section <- c(section, report_table(numeric))
  if (!is.null(ordered)) section <- c(section, report_table(ordered))
  if ("graph" %in% entry$sections) {
    graph_html <- tryCatch({
      if (!any(view$status == "valid" & !is.na(view$group)))
        stop("No valid responses to plot.")
      uri <- report_plot_uri(view, entry$graph, entry$question)
      paste0("<img alt='", report_escape(entry$graph), " for ",
             report_escape(entry$question), "' src='", uri, "'>")
    }, error = function(e)
      paste0("<p>", report_escape(conditionMessage(e)), "</p>"))
    section <- c(section, paste0("<h3>Graph: ",
                                  report_escape(entry$graph), "</h3>"),
                 graph_html)
  }
  if ("test" %in% entry$sections) {
    section <- c(section, "<h3>Group comparison tests</h3>")
    choices <- if (type == "multiselect" &&
                   identical(entry$choice, "__all_options__"))
      sort(unique(unlist(full$values[full$status == "valid"]))) else entry$choice
    if (type != "multiselect") choices <- NA_character_
    for (method in entry$tests) for (choice in choices) {
      selected_choice <- if (is.na(choice)) NULL else choice
      result <- tryCatch(
        comparison_question_test(full, method, selected_choice),
        error = function(e) list(error = conditionMessage(e)))
      section <- c(section,
        paste0("<h4>", report_escape(method),
               if (type == "multiselect") paste0(" — ", report_escape(choice))
               else "", "</h4>"),
        paste0("<p>", report_escape(test_description(method)), "</p>"),
        paste0("<pre>", report_escape(report_test_text(result)), "</pre>"))
    }
    if (type == "multiselect" && length(choices) > 1)
      section <- c(section,
        "<p>Each option is tested separately. P-values are not adjusted across options.</p>")
  }
  if ("free" %in% entry$sections && type == "free response") {
    valid <- view[view$status == "valid" & !is.na(view$group), , drop = FALSE]
    responses <- data.frame(source = valid$source, row = valid$row,
                            group = valid$group,
                            response = vapply(valid$values, `[[`, "", 1))
    section <- c(section, "<h3>Free responses</h3>",
                 report_table(responses))
  }
  paste0(paste(section, collapse = "\n"), "</section>")
}

report_body_html <- function(entries, surveys, title) {
  if (!length(entries)) return("<p>Select questions to preview the report.</p>")
  sections <- vapply(entries, report_entry_html, "", surveys = surveys)
  paste0("<div class='survey-report'><h1>", report_escape(title),
         "</h1><p>", length(surveys), " loaded sheets. ",
         "Group labels represent independent populations.</p>",
         paste(sections, collapse = "\n"), "</div>")
}

report_document_html <- function(entries, surveys, title) {
  css <- paste(
    "body{font-family:Arial,sans-serif;max-width:1100px;margin:40px auto;",
    "padding:0 24px;color:#222;line-height:1.4}",
    "section.question{border-top:1px solid #bbb;padding:22px 0}",
    "table{border-collapse:collapse;margin:12px 0 26px;width:100%}",
    "th,td{border:1px solid #ccc;padding:6px 9px;text-align:left}",
    "th{background:#f2f2f2}img{max-width:100%;height:auto}",
    "pre{white-space:pre-wrap;background:#f7f7f7;padding:12px}",
    sep = "")
  paste0("<!doctype html><html><head><meta charset='utf-8'><title>",
         report_escape(title), "</title><style>", css,
         "</style></head><body>",
         report_body_html(entries, surveys, title), "</body></html>")
}
