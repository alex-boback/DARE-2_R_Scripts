# Data and statistics for the Shiny app. No data are written to disk.

read_two_row_survey <- function(path, filename, sheet = 1) {
  ext <- tolower(tools::file_ext(filename))
  if (ext == "csv") {
    raw <- read.csv(path, header = FALSE, colClasses = "character",
                    check.names = FALSE, na.strings = character(), fill = TRUE,
                    blank.lines.skip = FALSE, fileEncoding = "UTF-8-BOM")
  } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Excel uploads require readxl: install.packages('readxl')")
    raw <- as.data.frame(readxl::read_excel(
      path, sheet = sheet, col_names = FALSE, col_types = "text",
      .name_repair = "minimal"))
  } else stop("Upload a CSV, XLS, or XLSX file.")
  if (nrow(raw) < 2) stop("The file needs a question row and a type row.")
  questions <- trimws(as.character(unlist(raw[1, ], use.names = FALSE)))
  types <- tolower(trimws(as.character(unlist(raw[2, ], use.names = FALSE))))
  types[types == "linkert"] <- "likert"
  allowed <- c("likert", "continuous", "multiselect", "single select",
               "group key", "free response")
  if (anyNA(questions) || any(!nzchar(questions)) || anyDuplicated(questions))
    stop("Question names in row 1 must be filled and unique.")
  if (anyNA(types) || any(!types %in% allowed))
    stop("Row 2 types must be: likert, continuous, multiselect, single select, group key, or free response.")
  data <- raw[-c(1, 2), , drop = FALSE]
  if (!nrow(data)) stop("The file has no response rows.")
  names(data) <- questions
  rownames(data) <- NULL
  list(data = data, types = setNames(types, questions))
}

load_survey_files <- function(uploads, existing = list()) {
  if (is.null(uploads) || !nrow(uploads)) stop("Upload at least one file.")
  surveys <- existing
  for (i in seq_len(nrow(uploads))) {
    filename <- uploads$name[i]
    path <- uploads$datapath[i]
    ext <- tolower(tools::file_ext(filename))
    sheets <- if (ext == "csv") 1 else {
      if (!ext %in% c("xls", "xlsx")) stop("Upload CSV, XLS, or XLSX files.")
      if (!requireNamespace("readxl", quietly = TRUE))
        stop("Excel uploads require readxl: install.packages('readxl')")
      readxl::excel_sheets(path)
    }
    for (sheet in sheets) {
      survey <- read_two_row_survey(path, filename, sheet)
      keys <- names(survey$types)[survey$types == "group key"]
      if (length(keys) != 1)
        stop(filename, " / ", sheet, " needs exactly one group key column.")
      survey$key <- keys
      survey$source <- if (ext == "csv") filename else paste(filename, sheet, sep = " / ")
      surveys[[length(surveys) + 1L]] <- survey
    }
  }
  all_questions <- unique(unlist(lapply(surveys, function(x) names(x$types))))
  for (question in all_questions) {
    types <- unique(unlist(lapply(surveys, function(x) x$types[question])))
    types <- types[!is.na(types)]
    if (length(types) > 1)
      stop("Question type differs between sources: ", question)
  }
  surveys
}

parse_question <- function(values, type, delimiter = ";") {
  out <- vector("list", length(values))
  status <- rep("valid", length(values))
  for (i in seq_along(values)) {
    raw <- trimws(as.character(values[i]))
    if (is.na(raw) || !nzchar(raw)) {
      status[i] <- "missing"
      next
    }
    tokens <- if (type == "multiselect")
      trimws(strsplit(raw, delimiter, fixed = TRUE)[[1]]) else raw
    if (!length(tokens) || any(!nzchar(tokens))) {
      status[i] <- "invalid"
      next
    }
    if (type == "likert" && (length(tokens) != 1 ||
                             !tokens %in% as.character(1:5))) {
      status[i] <- "invalid"
      next
    }
    if (type == "continuous" &&
        (length(tokens) != 1 || !is.finite(suppressWarnings(as.numeric(tokens))))) {
      status[i] <- "invalid"
      next
    }
    out[[i]] <- unique(tokens)
  }
  list(values = out, status = status)
}

analysis_frame <- function(surveys, question, delimiter = ";") {
  pieces <- lapply(surveys, function(survey) {
    if (!question %in% names(survey$types)) return(NULL)
    type <- survey$types[[question]]
    parsed <- parse_question(survey$data[[question]], type, delimiter)
    groups <- parse_question(survey$data[[survey$key]], "group key")
    group <- vapply(groups$values, function(x)
      if (length(x)) x[1] else NA_character_, "")
    status <- parsed$status
    status[is.na(group)] <- "missing group"
    data.frame(source = survey$source, row = seq_along(status),
               group = group, status = status, type = type,
               values = I(parsed$values))
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) stop("Question was not found in loaded data: ", question)
  result <- do.call(rbind, pieces)
  rownames(result) <- NULL
  result
}

available_tests <- function(type, groups) {
  if (groups < 2) return(character())
  if (type == "free response") return(character())
  if (type == "likert") {
    tests <- "Kruskal-Wallis"
    if (groups == 2) tests <- c("Mann-Whitney U", "Brunner-Munzel", tests)
    if (groups > 2) tests <- c(tests, "Dunn post-hoc")
    return(tests)
  }
  if (type == "continuous") {
    tests <- c("Kruskal-Wallis", "One-way ANOVA", "Welch ANOVA",
               "Fligner-Killeen")
    if (groups == 2) tests <- c("Mann-Whitney U", "Welch t-test",
                                 "Independent t-test", "Brunner-Munzel",
                                 "Yuen trimmed-mean test", tests)
    if (groups > 2) tests <- c(tests, "Robust trimmed-mean ANOVA",
                                "Tukey HSD post-hoc", "Games-Howell post-hoc",
                                "Dunn post-hoc")
    return(tests)
  }
  if (type == "multiselect") return(c("Fisher exact", "Chi-square"))
  c("Fisher exact", "Chi-square")
}

summarize_question <- function(frame) {
  if (frame$type[1] == "continuous") return(NULL)
  groups <- unique(frame$group[frame$status == "valid" & !is.na(frame$group)])
  if (!length(groups)) return(data.frame())
  valid <- frame[frame$status == "valid" & !is.na(frame$group), , drop = FALSE]
  if (frame$type[1] == "likert") {
    counts <- do.call(rbind, lapply(groups, function(g) {
      x <- as.numeric(unlist(valid$values[valid$group == g]))
      data.frame(group = g, response = as.character(1:5),
                 n = tabulate(as.integer(x), nbins = 5), valid_n = length(x))
    }))
  } else if (frame$type[1] == "free response") {
    counts <- do.call(rbind, lapply(groups, function(g)
      data.frame(group = g, response = "Answered",
                 n = sum(valid$group == g),
                 valid_n = sum(valid$group == g))))
  } else {
    choices <- sort(unique(unlist(valid$values)))
    if (!length(choices)) return(data.frame())
    counts <- do.call(rbind, lapply(groups, function(g) {
      rows <- valid$values[valid$group == g]
      data.frame(group = g, response = choices,
        n = vapply(choices, function(choice)
          sum(vapply(rows, function(x) choice %in% x, logical(1))), integer(1)),
        valid_n = length(rows))
    }))
  }
  counts$percent <- ifelse(counts$valid_n > 0,
                           100 * counts$n / counts$valid_n, NA_real_)
  rownames(counts) <- NULL
  counts
}

summarize_numeric <- function(frame) {
  if (!frame$type[1] %in% c("likert", "continuous")) return(NULL)
  valid <- frame[frame$status == "valid" & !is.na(frame$group), , drop = FALSE]
  groups <- unique(valid$group)
  if (!length(groups)) return(NULL)
  do.call(rbind, lapply(groups, function(g) {
    x <- as.numeric(unlist(valid$values[valid$group == g]))
    n <- length(x)
    se <- if (n > 1) sd(x) / sqrt(n) else NA_real_
    data.frame(group = g, n = n, mean = mean(x),
               sd = if (n > 1) sd(x) else NA_real_,
               median = median(x), iqr = IQR(x),
               sem = se,
               ci_low = if (n > 1) mean(x) - qt(.975, n - 1) * se else NA_real_,
               ci_high = if (n > 1) mean(x) + qt(.975, n - 1) * se else NA_real_,
               shapiro_p = if (n >= 3 && n <= 5000 && sd(x) > 0)
                 shapiro.test(x)$p.value else NA_real_)
  }))
}

graph_options <- function(type) {
  switch(type,
    likert = c("100% stacked bars", "Response bars", "Boxplot"),
    continuous = c("Boxplot", "Histogram", "Density curves", "Strip chart"),
    multiselect = c("Grouped bars", "Dot plot"),
    `single select` = c("Grouped bars", "Stacked bars", "Dot plot"),
    `free response` = c("Response counts", "Response lengths"),
    character())
}

plot_question <- function(frame, graph, question) {
  valid <- frame[frame$status == "valid" & !is.na(frame$group), , drop = FALSE]
  if (!nrow(valid)) stop("No valid responses to plot.")
  type <- frame$type[1]
  groups <- unique(valid$group)

  if (type == "continuous" || graph == "Boxplot" ||
      graph == "Histogram" || graph == "Density curves" ||
      graph == "Strip chart") {
    y <- as.numeric(unlist(valid$values))
    g <- factor(valid$group, levels = groups)
    if (graph == "Boxplot") {
      boxplot(y ~ g, xlab = "Group", ylab = question, col = "#78b5ad")
    } else if (graph == "Strip chart") {
      stripchart(y ~ g, method = "jitter", jitter = 0.15, vertical = TRUE,
                 pch = 16, col = "#247f7a", xlab = "Group", ylab = question)
    } else if (graph == "Histogram") {
      colors <- grDevices::hcl.colors(length(groups), "Set 2")
      limits <- range(y)
      if (diff(limits) == 0) limits <- limits + c(-0.5, 0.5)
      breaks <- pretty(limits, n = 10)
      for (i in seq_along(groups)) {
        x <- y[g == groups[i]]
        hist(x, breaks = breaks, freq = FALSE, xlim = range(breaks),
             col = grDevices::adjustcolor(colors[i], alpha.f = 0.4),
             border = colors[i], add = i > 1, main = question,
             xlab = question, ylab = "Density")
      }
      legend("topright", groups, fill = colors, bty = "n")
    } else if (graph == "Density curves") {
      curves <- lapply(groups, function(group) {
        x <- y[g == group]
        if (length(x) < 2 || sd(x) == 0) return(NULL)
        density(x)
      })
      keep <- !vapply(curves, is.null, logical(1))
      if (!any(keep)) {
        plot.new()
        text(.5, .5, "Each plotted group needs at least two varying values.")
      } else {
        colors <- grDevices::hcl.colors(sum(keep), "Set 2")
        curves <- curves[keep]
        plot(curves[[1]], xlim = range(unlist(lapply(curves, `[[`, "x"))),
             ylim = c(0, max(unlist(lapply(curves, `[[`, "y")))),
             main = question, xlab = question, ylab = "Density",
             col = colors[1], lwd = 2)
        if (length(curves) > 1) for (i in 2:length(curves))
          lines(curves[[i]], col = colors[i], lwd = 2)
        legend("topright", groups[keep], col = colors, lwd = 2, bty = "n")
      }
    } else stop("Unsupported graph for this question type.")
    return(invisible(NULL))
  }

  if (graph == "Response lengths") {
    lengths <- nchar(unlist(valid$values))
    boxplot(lengths ~ factor(valid$group, levels = groups),
            xlab = "Group", ylab = "Characters per response",
            col = "#78b5ad", main = question)
    return(invisible(NULL))
  }

  summary <- summarize_question(frame)
  responses <- unique(summary$response)
  values <- matrix(0, nrow = length(responses), ncol = length(groups),
                   dimnames = list(responses, groups))
  measure <- if (graph == "Response counts") summary$n else summary$percent
  for (i in seq_len(nrow(summary)))
    values[summary$response[i], summary$group[i]] <- measure[i]
  if (graph == "Dot plot") {
    labels <- paste(summary$group, summary$response, sep = " - ")
    op <- par(mar = c(5, min(18, max(8, max(nchar(labels)) / 2)), 4, 2))
    on.exit(par(op))
    dotchart(summary$percent, labels = labels, pch = 16, color = "#247f7a",
             xlab = "Percent of valid responses", main = question)
  } else {
    beside <- graph %in% c("Grouped bars", "Response bars")
    colors <- if (type == "likert")
      c("#b45050", "#d99577", "#d4d4d4", "#78b5ad", "#247f7a")
    else grDevices::hcl.colors(nrow(values), "Set 2")
    barplot(values, beside = beside, col = colors,
            legend.text = if (graph == "Response counts") NULL else rownames(values),
            xlab = "Group", ylab = if (graph == "Response counts")
              "Responses" else "Percent of valid responses",
            ylim = if (graph %in% c("100% stacked bars", "Stacked bars"))
              c(0, 100) else NULL, main = question)
  }
  invisible(NULL)
}

run_question_test <- function(frame, method, choice = NULL) {
  valid <- frame[frame$status == "valid" & !is.na(frame$group), , drop = FALSE]
  groups <- unique(valid$group)
  if (length(groups) < 2) stop("At least two groups need valid responses.")
  if (frame$type[1] %in% c("likert", "continuous")) {
    y <- as.numeric(unlist(valid$values))
    g <- factor(valid$group, levels = groups)
    data <- data.frame(y = y, g = g)
    if (method == "Mann-Whitney U") {
      if (length(groups) != 2) stop("Mann-Whitney U needs two groups.")
      fit <- wilcox.test(y ~ g, exact = FALSE)
    } else if (method == "Brunner-Munzel") {
      if (length(groups) != 2) stop("Brunner-Munzel needs two groups.")
      if (!requireNamespace("brunnermunzel", quietly = TRUE))
        stop("Install brunnermunzel to run this test.")
      fit <- brunnermunzel::brunnermunzel.test(y ~ g, data = data)
    } else if (method == "Yuen trimmed-mean test") {
      if (length(groups) != 2) stop("Yuen's test needs two groups.")
      if (!requireNamespace("WRS2", quietly = TRUE))
        stop("Install WRS2 to run this test.")
      fit <- WRS2::yuen(y ~ g, data = data)
      return(list(method = method, statistic = fit$test, df = fit$df,
                  p_value = fit$p.value, note = "20% trimmed means."))
    } else if (method == "Welch t-test") {
      if (length(groups) != 2) stop("Welch t-test needs two groups.")
      fit <- t.test(y ~ g)
    } else if (method == "Independent t-test") {
      if (length(groups) != 2) stop("Independent t-test needs two groups.")
      fit <- t.test(y ~ g, var.equal = TRUE)
    } else if (method == "Kruskal-Wallis") {
      fit <- kruskal.test(y ~ g)
    } else if (method == "Welch ANOVA") {
      fit <- oneway.test(y ~ g, var.equal = FALSE)
    } else if (method == "Fligner-Killeen") {
      fit <- fligner.test(y ~ g)
    } else if (method == "Robust trimmed-mean ANOVA") {
      if (!requireNamespace("WRS2", quietly = TRUE))
        stop("Install WRS2 to run this test.")
      fit <- WRS2::t1way(y ~ g, data = data)
      return(list(method = method, statistic = fit$test,
                  df = c(fit$df1, fit$df2), p_value = fit$p.value,
                  note = "20% trimmed means."))
    } else if (method == "One-way ANOVA") {
      fit <- summary(aov(y ~ g))[[1]]
      return(list(method = method, statistic = unname(fit[1, "F value"]),
                  df = unname(fit[, "Df"]),
                  p_value = unname(fit[1, "Pr(>F)"]),
                  note = "Assumes independent observations, approximately normal residuals, and equal variances."))
    } else if (method == "Tukey HSD post-hoc") {
      fit <- TukeyHSD(aov(y ~ g))
      return(list(method = method, pairwise = as.data.frame(fit$g),
                  note = "Pairwise comparisons with family-wise adjustment."))
    } else if (method %in% c("Games-Howell post-hoc", "Dunn post-hoc")) {
      if (!requireNamespace("PMCMRplus", quietly = TRUE))
        stop("Install PMCMRplus to run this test.")
      fit <- if (method == "Games-Howell post-hoc")
        PMCMRplus::gamesHowellTest(y ~ g, data = data)
      else
        PMCMRplus::kwAllPairsDunnTest(y ~ g, data = data,
                                      p.adjust.method = "bonferroni")
      return(list(method = method, pairwise = fit$p.value,
                  note = "Adjusted pairwise p-values."))
    } else stop("Select a supported test.")
  } else {
    if (frame$type[1] == "multiselect") {
      if (is.null(choice) || !nzchar(choice)) stop("Select a multiselect choice.")
      response <- vapply(valid$values, function(x)
        if (choice %in% x) "Selected" else "Not selected", "")
    } else {
      response <- vapply(valid$values, `[[`, "", 1)
    }
    table <- table(factor(valid$group, levels = groups), response)
    table <- table[, colSums(table) > 0, drop = FALSE]
    if (ncol(table) < 2) stop("The response has no variation across groups.")
    if (method == "Fisher exact") {
      fit <- fisher.test(table)
    } else if (method == "Chi-square") {
      fit <- suppressWarnings(chisq.test(table))
    } else stop("Select a supported test.")
  }
  list(method = method, statistic = unname(fit$statistic),
       df = if (!is.null(fit$parameter)) unname(fit$parameter) else NA_real_,
       p_value = fit$p.value,
       note = if (method == "Chi-square" && any(fit$expected < 5))
         "Some expected counts are below 5; consider Fisher exact." else "")
}
