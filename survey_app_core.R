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

empty_mapping <- function() data.frame(
  question = character(), period = character(), raw = character(),
  mapped = character(), stringsAsFactors = FALSE)

read_mapping_file <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext == "csv") {
    x <- read.csv(path, colClasses = "character", check.names = FALSE,
                  na.strings = character(), fileEncoding = "UTF-8-BOM")
  } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Excel mapping files require readxl.")
    x <- as.data.frame(readxl::read_excel(path, col_types = "text"))
  } else stop("Mapping file must be CSV, XLS, or XLSX.")
  required <- c("question", "period", "raw", "mapped")
  if (!all(required %in% names(x)))
    stop("Mapping file needs columns: question, period, raw, mapped.")
  x <- x[, required, drop = FALSE]
  for (name in required) x[[name]] <- trimws(as.character(x[[name]]))
  x$period[is.na(x$period) | x$period == ""] <- "All"
  if (anyNA(x$question) || anyNA(x$raw) || anyNA(x$mapped) ||
      any(!nzchar(x$question)) || any(!nzchar(x$raw)) ||
      any(!nzchar(x$mapped)) ||
      any(!x$period %in% c("All", "Period 1", "Period 2")))
    stop("Mappings need nonblank question, raw, and mapped values; period is All, Period 1, or Period 2.")
  x
}

widget_mapping <- function(text, question, period) {
  if (is.null(text) || !nzchar(trimws(text))) return(empty_mapping())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines[nzchar(trimws(lines))])
  parts <- strsplit(lines, "=>", fixed = TRUE)
  if (any(lengths(parts) != 2))
    stop("Each UI mapping line must be raw => mapped.")
  raw <- trimws(vapply(parts, `[[`, "", 1))
  mapped <- trimws(vapply(parts, `[[`, "", 2))
  if (any(!nzchar(raw)) || any(!nzchar(mapped)))
    stop("UI mappings cannot have a blank raw or mapped value.")
  data.frame(question = question, period = period, raw = raw, mapped = mapped)
}

apply_mapping <- function(tokens, mappings, question, period) {
  relevant <- mappings[mappings$question == question &
                         mappings$period %in% c("All", period), , drop = FALSE]
  if (!nrow(relevant)) return(tokens)
  # Period-specific rules win within a source; UI rules win over file rules.
  priority <- as.integer(relevant$period == period)
  if ("source" %in% names(relevant))
    priority <- priority + 2L * as.integer(relevant$source == "ui")
  relevant <- relevant[order(priority), , drop = FALSE]
  relevant <- relevant[!duplicated(relevant$raw, fromLast = TRUE), , drop = FALSE]
  replacement <- setNames(relevant$mapped, relevant$raw)
  hit <- tokens %in% names(replacement)
  tokens[hit] <- unname(replacement[tokens[hit]])
  tokens
}

parse_question <- function(values, type, question, period, mappings, delimiter) {
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
    tokens <- apply_mapping(tokens, mappings, question, period)
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

question_frame <- function(surveys, question, mappings, delimiter) {
  type <- surveys[[1]]$types[[question]]
  pieces <- lapply(seq_along(surveys), function(i) {
    parsed <- parse_question(surveys[[i]]$data[[question]], type, question,
                             paste("Period", i), mappings, delimiter)
    data.frame(period = paste("Period", i),
               row = seq_along(parsed$status), status = parsed$status,
               values = I(parsed$values))
  })
  do.call(rbind, pieces)
}

analysis_frame <- function(surveys, question, comparison, group_question,
                           mappings, delimiter) {
  type <- surveys[[1]]$types[[question]]
  target <- question_frame(surveys, question, mappings, delimiter)
  if (comparison == "Period") {
    target$group <- target$period
  } else {
    if (is.null(group_question) || !nzchar(group_question))
      stop("Select a group key.")
    groups <- question_frame(surveys, group_question, mappings, delimiter)
    target$group <- vapply(groups$values, function(x)
      if (length(x)) x[1] else NA_character_, "")
    target$status[groups$status != "valid"] <- "missing"
  }
  target$type <- type
  target
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
