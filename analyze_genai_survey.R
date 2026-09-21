# Compare independent survey samples from two periods.
# Source this file, define fields with survey_field(), then call analyze_survey().

survey_field <- function(name, type, columns, levels = NULL, maps = list(NULL, NULL),
                         delimiter = ";") {
  if (!type %in% c("single", "multi", "ordinal", "numeric"))
    stop("type must be single, multi, ordinal, or numeric")
  if (length(columns) != 2 || anyNA(columns) || any(!nzchar(columns)))
    stop("columns must contain one name for each period")
  if (type != "numeric" && (!length(levels) || anyDuplicated(levels)))
    stop("Categorical fields need distinct canonical levels")
  if (length(maps) != 2) stop("maps must contain one map per period")
  for (map in maps) if (!is.null(map) &&
    (is.null(names(map)) || anyDuplicated(names(map)) || anyNA(map) ||
     any(!map %in% levels)))
    stop("Maps need distinct raw names and values from levels")
  if (type == "multi" && (length(delimiter) != 1 || !nzchar(delimiter)))
    stop("delimiter must be a nonempty string")
  list(name = name, type = type, columns = columns, levels = levels,
       maps = maps, delimiter = delimiter)
}

read_survey <- function(path, sheet, skip) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    data <- read.csv(path, skip = skip, colClasses = "character",
                     check.names = FALSE, na.strings = "", fileEncoding = "UTF-8-BOM")
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Install readxl to use Excel: install.packages('readxl')")
    data <- as.data.frame(readxl::read_excel(
      path, sheet = sheet, skip = skip, col_types = "text", na = "",
      .name_repair = "minimal"), check.names = FALSE)
  } else stop("Expected .csv, .xlsx, or .xls: ", path)
  if (anyDuplicated(names(data))) stop("Duplicate column names in ", path)
  data
}

parse_field <- function(raw_values, field, period) {
  values <- vector("list", length(raw_values))
  status <- rep("valid", length(raw_values))
  map <- field$maps[[period]]
  for (i in seq_along(raw_values)) {
    raw <- trimws(raw_values[i])
    if (is.na(raw) || !nzchar(raw)) {
      status[i] <- "missing"
      next
    }
    if (field$type == "numeric") {
      number <- suppressWarnings(as.numeric(raw))
      if (is.finite(number)) values[[i]] <- number else status[i] <- "invalid"
      next
    }
    tokens <- if (field$type == "multi")
      trimws(strsplit(raw, field$delimiter, fixed = TRUE)[[1]]) else raw
    if (!length(tokens) || any(!nzchar(tokens))) {
      status[i] <- "invalid"
      next
    }
    if (!is.null(map)) tokens <- unname(map[match(tokens, names(map))])
    if (anyNA(tokens) || any(!tokens %in% field$levels))
      status[i] <- "invalid" else values[[i]] <- unique(tokens)
  }
  list(values = values, status = status)
}

# compare_field <- function(field, parsed, periods) {
  valid <- lapply(parsed, function(x) x$values[x$status == "valid"])
  n <- lengths(valid)
  test <- function(choice, method, p) data.frame(
    field = field$name, choice = choice, method = method,
    n_1 = n[1], n_2 = n[2], p_value = p)
  if (field$type %in% c("ordinal", "numeric")) {
    values <- lapply(valid, function(rows) {
      x <- unlist(rows)
      if (field$type == "ordinal") match(x, field$levels) else x
    })
    summary <- data.frame(field = field$name, period = periods, n_valid = n,
      mean = vapply(values, function(x) if (length(x)) mean(x) else NA_real_, 0.0),
      median = vapply(values, function(x) if (length(x)) median(x) else NA_real_, 0.0))
    p <- if (all(n > 0)) wilcox.test(values[[1]], values[[2]],
                                     exact = FALSE)$p.value else NA_real_
    return(list(summary = summary, tests = test(NA_character_, "Wilcoxon rank-sum", p)))
  }
  counts <- vapply(valid, function(rows)
    vapply(field$levels, function(choice)
      sum(vapply(rows, function(row) choice %in% row, logical(1))), integer(1)),
    integer(length(field$levels)))
  if (is.null(dim(counts))) counts <- matrix(counts, ncol = 2)
  summary <- data.frame(field = field$name,
    period = rep(periods, each = length(field$levels)),
    choice = rep(field$levels, 2), n = as.vector(counts),
    n_valid = rep(n, each = length(field$levels)),
    percent = 100 * as.vector(counts) /
      rep(ifelse(n > 0, n, NA_integer_), each = length(field$levels)))
  if (field$type == "multi") {
    tests <- do.call(rbind, lapply(seq_along(field$levels), function(j) {
      table <- rbind(counts[j, ], n - counts[j, ])
      p <- if (all(n > 0)) fisher.test(table)$p.value else NA_real_
      test(field$levels[j], "Fisher exact (selected vs not)", p)
    }))
  } else {
    table <- counts[rowSums(counts) > 0, , drop = FALSE]
    if (!all(n > 0) || nrow(table) < 2) {
      tests <- test(NA_character_, "Insufficient responses", NA_real_)
    } else if (nrow(table) == 2) {
      tests <- test(NA_character_, "Fisher exact", fisher.test(table)$p.value)
    } else {
      sparse <- any(suppressWarnings(chisq.test(table)$expected) < 5)
      p <- suppressWarnings(chisq.test(table, simulate.p.value = sparse,
                                       B = 10000)$p.value)
      tests <- test(NA_character_,
                    if (sparse) "Chi-square (10,000 simulations)" else "Chi-square", p)
    }
  }
  list(summary = summary, tests = tests)
}

analyze_survey <- function(period_1, period_2, fields,
                           period_names = c("Period 1", "Period 2"),
                           sheets = c(1, 1), skips = c(0, 0)) {
  if (length(period_names) != 2 || length(sheets) != 2 || length(skips) != 2)
    stop("period_names, sheets, and skips each need two entries")
  if (!length(fields) || anyDuplicated(vapply(fields, `[[`, "", "name")))
    stop("Supply fields with unique names")
  paths <- c(period_1, period_2)
  data <- lapply(1:2, function(i) read_survey(paths[i], sheets[i], skips[i]))
  summaries <- tests <- quality <- vector("list", length(fields))
  for (j in seq_along(fields)) {
    field <- fields[[j]]
    parsed <- lapply(1:2, function(i) {
      column <- field$columns[i]
      if (!column %in% names(data[[i]]))
        stop("Missing column '", column, "' for ", field$name, " in ", paths[i])
      parse_field(data[[i]][[column]], field, i)
    })
    result <- compare_field(field, parsed, period_names)
    summaries[[j]] <- result$summary
    tests[[j]] <- result$tests
    quality[[j]] <- do.call(rbind, lapply(1:2, function(i)
      data.frame(field = field$name, period = period_names[i],
                 total = length(parsed[[i]]$status),
                 valid = sum(parsed[[i]]$status == "valid"),
                 missing = sum(parsed[[i]]$status == "missing"),
                 invalid = sum(parsed[[i]]$status == "invalid"))))
  }
  results <- list(summary = do.call(rbind, summaries),
                  tests = do.call(rbind, tests),
                  response_counts = do.call(rbind, quality))
  for (name in names(results)) rownames(results[[name]]) <- NULL
  results$tests$p_adjusted <- p.adjust(results$tests$p_value, method = "BH")
  cat("\nResponse counts:\n")
  print(results$response_counts, row.names = FALSE)
  cat("\nSummaries:\n")
  print(results$summary, row.names = FALSE)
  cat("\nBetween-period tests (BH adjusted across all tests):\n")
  print(results$tests, row.names = FALSE)
  invisible(results)
}
