# GenAI survey analysis
# Install once: install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "stringr"))
# Run in RStudio:
#   source("analyze_genai_survey.R")
#   results <- analyze_survey(file.choose())
# Or: results <- analyze_survey("C:/data/survey.xlsx", sheet = "Sheet1")
# Documentation: https://readxl.tidyverse.org/reference/read_excel.html
# https://tidyr.tidyverse.org/reference/pivot_longer.html
# https://ggplot2.tidyverse.org/reference/ggsave.html

# 1. Answer definitions ------------------------------------------------------
# Q2 codes follow the order in the supplied survey screenshot. Edit this
# lookup if the survey has custom recode values.
Q2_CHOICES <- c(
  "1" = "ChatGPT",
  "2" = "Claude",
  "3" = "Google Gemini",
  "4" = "Microsoft Copilot",
  "5" = "Consensus, Elicit, or other AI research tools",
  "6" = "Other",
  "7" = "None"
)

Q3_CHOICES <- c(
  "Never",
  "Once or twice",
  "Occasionally (during 1 - 2 stages of the design process)",
  "Frequently (in several stages of the design process)",
  "Very frequently (across most/all stages of the design process)"
)

# Numeric export codes, using the displayed survey order.
Q3_CODES <- setNames(Q3_CHOICES, as.character(1:5))

LIKERT_LABELS <- c(
  "Strongly disagree", "Disagree", "Neither agree nor disagree",
  "Agree", "Strongly agree"
)

# 2. Small parsing helpers ---------------------------------------------------
normalize <- function(x) {
  stringr::str_to_lower(stringr::str_squish(x))
}

blank <- function(x) {
  is.na(x) | stringr::str_trim(x) == ""
}

clean_code <- function(x) {
  # Excel may supply a number or text such as "3.0". Normalize integer codes
  # without rounding invalid values such as 3.5.
  x <- stringr::str_trim(as.character(x))
  sub("^([0-9]+)\\.0+$", "\\1", x)
}

pct <- function(n, denominator) {
  # A zero denominator produces NA, rather than a misleading 0%.
  100 * n / ifelse(denominator > 0, denominator, NA_real_)
}

parse_q2 <- function(value, code_map = Q2_CHOICES) {
  value <- as.character(value)
  result <- list(choices = character(), status = "missing", issue = NA_character_)
  if (blank(value)) return(result)

  # Label exports are supported too. Protect the commas inside this one label.
  research_label <- unname(Q2_CHOICES["5"])
  protected <- stringr::str_replace_all(
    value,
    stringr::regex("Consensus\\s*,\\s*Elicit\\s*,\\s*or other AI research tools",
                   ignore_case = TRUE),
    "RESEARCH_TOOLS_CHOICE"
  )
  tokens <- unlist(strsplit(protected, "[,;|\\r\\n]+", perl = TRUE))
  tokens <- clean_code(tokens)
  tokens <- tokens[nzchar(tokens)]
  tokens[tokens == "RESEARCH_TOOLS_CHOICE"] <- research_label
  tokens <- sub(":$", "", tokens)

  # Example: "1,3,4" becomes ChatGPT, Google Gemini, Microsoft Copilot.
  coded <- tokens %in% names(code_map)
  tokens[coded] <- unname(code_map[tokens[coded]])
  positions <- match(normalize(tokens), normalize(unname(Q2_CHOICES)))

  if (!length(tokens) || anyNA(positions)) {
    result$status <- "invalid"
    result$issue <- "Unrecognized Q2 choice/code; entire Q2 response excluded"
    return(result)
  }

  # Repeated codes count only once per respondent.
  result$choices <- unique(unname(Q2_CHOICES[positions]))
  if ("None" %in% result$choices && length(result$choices) > 1) {
    result$status <- "invalid"
    result$issue <- "None selected with another choice; entire Q2 response excluded"
    return(result)
  }

  result$status <- "valid"
  result
}

parse_q3 <- function(value, code_map = Q3_CODES) {
  value <- as.character(value)
  result <- list(value = NA_character_, status = "missing", issue = NA_character_)
  if (blank(value)) return(result)

  value <- clean_code(value)
  if (value %in% names(code_map)) value <- unname(code_map[value])

  # Accept either the full choice or its short name, such as "Occasionally".
  short_name <- function(x) normalize(sub("\\s*\\(.*$", "", x))
  position <- match(short_name(value), short_name(Q3_CHOICES))
  if (is.na(position)) {
    result$status <- "invalid"
    result$issue <- "Unrecognized frequency or unmapped numeric code"
  } else {
    result$value <- Q3_CHOICES[position]
    result$status <- "valid"
  }
  result
}

parse_likert <- function(values) {
  normalized <- normalize(values)
  numeric_score <- suppressWarnings(as.numeric(normalized))
  text_score <- match(normalized, normalize(LIKERT_LABELS))
  text_score[normalized %in% c("neutral", "neither disagree nor agree")] <- 3L

  score <- ifelse(!is.na(numeric_score), numeric_score, text_score)
  status <- dplyr::case_when(
    blank(values) ~ "missing",
    normalized %in% c("n/a", "na", "not applicable") ~ "not_applicable",
    score %in% 1:5 ~ "valid",
    TRUE ~ "invalid"
  )
  score[status != "valid"] <- NA_real_
  data.frame(score = score, status = status)
}

# 3. Main analysis -----------------------------------------------------------
# Call this function after sourcing the script. Existing output files are
# overwritten when the same output_dir is used again.
analyze_survey <- function(path, sheet = 1,
                           output_dir = file.path(dirname(path), "survey_results"),
                           header_row = 1, metadata_rows = "auto",
                           q2_code_map = Q2_CHOICES, q3_code_map = Q3_CODES) {
  packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "stringr")
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) stop("Install packages first: ", paste(missing_packages, collapse = ", "))
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(stringr)
  })
  if (!file.exists(path)) stop("Excel file does not exist: ", path)
  if (length(header_row) != 1 || is.na(header_row) || header_row < 1 || header_row %% 1 != 0)
    stop("header_row must be a positive integer.")

  tools <- unname(Q2_CHOICES)
  frequency <- Q3_CHOICES
  # Both numeric codes and text labels are accepted by default.
  # Override the maps if the survey uses custom recode values.
  for (nm in c("q2_code_map", "q3_code_map")) {
    mapping <- get(nm)
    allowed <- if (nm == "q2_code_map") tools else frequency
    if (!is.null(mapping) && (is.null(names(mapping)) || anyDuplicated(names(mapping)) ||
                              anyNA(mapping) || any(!mapping %in% allowed)))
      stop(nm, " must be a named vector with values matching the documented labels.")
  }
  items <- data.frame(
    question = c(paste0("Q5_", 1:8), paste0("Q6_", 1:5)),
    variable = c("understand_user_needs", "clarify_objectives_constraints",
                 "conduct_literature_reviews", "generate_ideas", "code_debug",
                 "design_experiments", "analyze_visualize_data", "write_reports_presentations",
                 "prepared_to_evaluate_ai", "considered_ethics", "design_engagement",
                 "confident_effective_ethical_use", "ai_accuracy_reliability"),
    label = c("Understand clients'/users' needs", "Develop objectives and clarify constraints",
              "Conduct literature reviews", "Generate design ideas", "Code or debug programs",
              "Design experiments", "Analyze and visualize data", "Write reports or presentations",
              "Prepared to evaluate AI content", "Considered ethical implications",
              "More engaged in design", "Confident using AI effectively and ethically",
              "AI information accurate and reliable"), stringsAsFactors = FALSE
  )
  items$group <- substr(items$question, 1, 2)
  # Read the requested columns and remove leading survey-description rows.
  required <- c("Q2", "Q2_6_TEXT", "Q3", items$question)
  # Reading everything as text preserves raw responses and N/A markers.
  raw <- readxl::read_excel(path, sheet = sheet, skip = header_row - 1,
                           col_types = "text", na = "", .name_repair = "minimal")
  names(raw) <- trimws(names(raw))
  absent <- setdiff(required, names(raw))
  if (length(absent)) stop("Required columns not found: ", paste(absent, collapse = ", "),
                           ". Check header_row (the row containing Q2, Q3, Q5_1, etc.).")
  if (any(vapply(required, function(x) sum(names(raw) == x) != 1, logical(1))))
    stop("Required column names must appear exactly once.")
  raw <- as.data.frame(raw[, match(required, names(raw)), drop = FALSE])
  raw$source_row <- seq_len(nrow(raw)) + header_row
  # Only recognize leading metadata rows. Respondents are never dropped for
  # having all target questions blank; other columns may identify that person.
  is_metadata <- function(i) {
    x <- unlist(raw[i, required], use.names = FALSE)
    x <- x[!is.na(x)]
    sum(grepl("^(Which Generative AI|How frequently did you|Please indicate the extent)",
              trimws(x), ignore.case = TRUE)) >= 2 ||
      sum(grepl('"ImportId"\\s*:', x)) >= 2
  }
  if (identical(metadata_rows, "auto")) {
    n_drop <- 0L
    while (n_drop < nrow(raw) && is_metadata(n_drop + 1L)) n_drop <- n_drop + 1L
  } else {
    if (!is.numeric(metadata_rows) || length(metadata_rows) != 1 || is.na(metadata_rows) ||
        metadata_rows < 0 || metadata_rows %% 1 != 0 || metadata_rows > nrow(raw))
      stop("metadata_rows must be 'auto' or the number of rows after the header to skip.")
    n_drop <- as.integer(metadata_rows)
  }
  if (n_drop > 0) raw <- raw[-seq_len(n_drop), , drop = FALSE]
  if (!nrow(raw)) stop("No respondent rows remain after removing metadata.")
  raw$respondent_id <- seq_len(nrow(raw))
  N <- nrow(raw)
  issues <- data.frame(respondent_id = integer(), source_row = integer(),
                       question = character(), raw_value = character(), issue = character())
  add_issue <- function(i, question, value, message) {
    issues <<- rbind(issues, data.frame(respondent_id = raw$respondent_id[i],
      source_row = raw$source_row[i], question = question, raw_value = value, issue = message))
  }

  # Parse Q2 into one 0/1 column per choice. Missing/invalid rows stay NA.
  q2 <- matrix(NA_integer_, N, length(tools), dimnames = list(NULL, tools))
  q2_status <- rep("missing", N)
  q2_decoded <- rep(NA_character_, N)
  for (i in seq_len(N)) {
    parsed <- parse_q2(raw$Q2[i], q2_code_map)
    q2_status[i] <- parsed$status
    if (length(parsed$choices)) {
      q2_decoded[i] <- paste(parsed$choices, collapse = "; ")
    }
    if (!is.na(parsed$issue)) {
      add_issue(i, "Q2", raw$Q2[i], parsed$issue)
    }
    if (parsed$status == "valid") {
      q2[i, ] <- 0L
      q2[i, parsed$choices] <- 1L
    }
    if (!blank(raw$Q2_6_TEXT[i]) && !"Other" %in% parsed$choices) {
      add_issue(i, "Q2_6_TEXT", raw$Q2_6_TEXT[i],
                "Other text present without a recognized Other selection; text retained")
    }
  }

  # Parse Q3 into ordered frequency labels.
  q3 <- rep(NA_character_, N)
  q3_status <- rep("missing", N)
  for (i in seq_len(N)) {
    parsed <- parse_q3(raw$Q3[i], q3_code_map)
    q3[i] <- parsed$value
    q3_status[i] <- parsed$status
    if (!is.na(parsed$issue)) {
      add_issue(i, "Q3", raw$Q3[i], parsed$issue)
    }
  }

  # Do not present all-zero counts when every nonblank answer was rejected.
  check_parsed_answers <- function(question, values, statuses) {
    if (any(!blank(values)) && !any(statuses == "valid")) {
      examples <- head(unique(values[!blank(values)]), 5)
      stop(
        question, ": no nonblank responses could be parsed. Examples: ",
        paste(shQuote(examples), collapse = "; "),
        ". Reload this script and check the code mapping or metadata_rows. ",
        "No new summaries were saved; existing output files have not been updated.",
        call. = FALSE
      )
    }
  }
  check_parsed_answers("Q2", raw$Q2, q2_status)
  check_parsed_answers("Q3", raw$Q3, q3_status)

  # Parse Q5/Q6. Keep one row per respondent and question for summaries.
  long <- raw %>% select(respondent_id, source_row, all_of(items$question)) %>%
    pivot_longer(all_of(items$question), names_to = "question", values_to = "raw_value") %>%
    left_join(items, by = "question")
  labels <- LIKERT_LABELS
  ratings <- parse_likert(long$raw_value)
  long$score <- ratings$score
  long$status <- ratings$status
  bad <- which(long$status == "invalid")
  for (j in bad) add_issue(long$respondent_id[j], long$question[j], long$raw_value[j],
                           "Expected an integer 1-5, a Likert label, blank, or N/A")
  # Build a readable table with one row per respondent.
  cleaned <- raw %>% select(respondent_id, source_row) %>%
    mutate(tools_used_raw = raw$Q2, other_tool_text = raw$Q2_6_TEXT,
           tools_used_decoded = q2_decoded, q2_status = q2_status,
           usage_frequency_raw = raw$Q3,
           usage_frequency = q3, q3_status = q3_status)
  tool_vars <- c("tool_chatgpt", "tool_claude", "tool_gemini", "tool_copilot",
                 "tool_research", "tool_other", "tool_none")
  for (j in seq_along(tools)) cleaned[[tool_vars[j]]] <- q2[, j]
  cleaned <- left_join(cleaned, long %>% select(respondent_id, variable, score) %>%
                        pivot_wider(names_from = variable, values_from = score), by = "respondent_id")

  # Summaries: valid answers are the denominator for each question.
  q2_summary <- data.frame(tool = tools, n = colSums(q2, na.rm = TRUE),
                          valid_respondents = sum(q2_status == "valid")) %>%
    mutate(percent_valid = pct(n, valid_respondents), percent_all = pct(n, N))
  q3_summary <- data.frame(frequency = frequency,
                           n = as.integer(table(factor(q3, levels = frequency)))) %>%
    mutate(valid_respondents = sum(q3_status == "valid"),
           percent_valid = pct(n, valid_respondents), percent_all = pct(n, N))
  likert_summary <- long %>% group_by(question, label, group) %>% summarise(
    n_total = n(), n_valid = sum(status == "valid"), n_missing = sum(status == "missing"),
    n_not_applicable = sum(status == "not_applicable"), n_invalid = sum(status == "invalid"),
    mean = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE),
    median = if (all(is.na(score))) NA_real_ else median(score, na.rm = TRUE),
    sd = if (sum(!is.na(score)) < 2) NA_real_ else sd(score, na.rm = TRUE),
    percent_agree = pct(sum(score >= 4, na.rm = TRUE), sum(!is.na(score))), .groups = "drop")
  distribution <- expand_grid(question = items$question, score = 1:5) %>%
    left_join(long %>% filter(status == "valid") %>% count(question, score), by = c("question", "score")) %>%
    mutate(n = replace_na(n, 0L)) %>% left_join(items, by = "question") %>%
    left_join(likert_summary %>% select(question, n_valid), by = "question") %>%
    mutate(percent_valid = pct(n, n_valid), response = factor(score, levels = 1:5, labels = labels))
  question_status <- bind_rows(
    data.frame(question = "Q2", status = q2_status),
    data.frame(question = "Q3", status = q3_status),
    long %>% select(question, status)) %>% count(question, status) %>%
    complete(question = c("Q2", "Q3", items$question),
             status = c("valid", "missing", "not_applicable", "invalid"), fill = list(n = 0L)) %>%
    mutate(percent_all = pct(n, N))

  # Save CSV tables.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  save_csv <- function(x, name) write.csv(x, file.path(output_dir, paste0(name, ".csv")),
                                          row.names = FALSE, na = "", fileEncoding = "UTF-8")
  tables <- list(raw_selected = raw, cleaned_responses = cleaned, likert_long = long,
                 q2_tools_summary = q2_summary, q3_frequency_summary = q3_summary,
                 likert_summary = likert_summary, likert_distribution = distribution,
                 response_status = question_status, data_issues = issues,
                 item_dictionary = items,
                 other_tool_responses = cleaned %>% filter(!blank(other_tool_text)) %>%
                   select(respondent_id, source_row, other_tool_text))
  for (nm in names(tables)) save_csv(tables[[nm]], nm)
  # Draw and save four graphs.
  base_theme <- theme_minimal(base_size = 12) + theme(panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0),
    legend.position = "bottom")
  save_plot <- function(p, name, height = 5) {
    ggsave(file.path(output_dir, paste0(name, ".png")), p + base_theme,
           width = 11, height = height, dpi = 300, bg = "white")
  }
  p_tools <- ggplot(q2_summary, aes(x = n, y = factor(tool, levels = rev(tools)))) +
    geom_col(fill = "#287C8E", width = 0.7) + geom_text(aes(label = n), hjust = -0.2) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(labels = function(x) str_wrap(x, 35)) +
    labs(title = "GenAI tools used", x = "Respondents selecting each choice", y = NULL,
         caption = paste0("Valid Q2 responses: ", sum(q2_status == "valid"), " of ", N,
                          ". Multiple selections allowed; percentages can sum above 100%."))
  save_plot(p_tools, "q2_tools")
  p_freq <- ggplot(q3_summary, aes(x = n, y = factor(frequency, levels = rev(frequency)))) +
    geom_col(fill = "#5666A5", width = 0.7) + geom_text(aes(label = n), hjust = -0.2) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(labels = function(x) str_wrap(x, 42)) +
    labs(title = "Frequency of GenAI use", x = "Respondents", y = NULL,
         caption = paste0("Valid Q3 responses: ", sum(q3_status == "valid"), " of ", N, "."))
  save_plot(p_freq, "q3_frequency")
  for (g in c("Q5", "Q6")) {
    d <- distribution %>% filter(group == g)
    ordered_labels <- items$label[items$group == g]
    display_labels <- likert_summary %>% filter(group == g) %>%
      mutate(display = paste0(str_wrap(label, 43), " (n=", n_valid, ")"))
    d <- left_join(d, display_labels %>% select(question, display), by = "question")
    display_order <- display_labels$display[match(ordered_labels, display_labels$label)]
    p <- ggplot(d, aes(x = ifelse(is.na(percent_valid), 0, percent_valid),
                        y = factor(display, levels = rev(display_order)), fill = response)) +
      geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
      scale_fill_manual(values = c("#B34B52", "#E59B75", "#D6D6D6", "#73B5B0", "#267A78"), drop = FALSE) +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%")) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      labs(title = if (g == "Q5") "Q5: How GenAI supports design tasks" else "Q6: Experiences and confidence with GenAI",
           x = "Percentage of valid ratings", y = NULL, fill = NULL,
           caption = "1 = Strongly disagree; 5 = Strongly agree. Blank, N/A, and invalid ratings excluded.\nn=0 means no valid ratings; no bar is drawn.")
    save_plot(p, paste0(tolower(g), "_likert"), height = if (g == "Q5") 7 else 6)
  }
  writeLines(c(paste("Respondent rows:", N), paste("Leading metadata rows removed:", n_drop),
    "Only requested columns are analyzed. Respondent rows with all target answers blank are retained.",
    "Q2 percentages use valid Q2 respondents, not the number of selections. None is a valid choice.",
    "Q2 default codes follow the supplied choice order: 1 ChatGPT; 2 Claude; 3 Google Gemini; 4 Microsoft Copilot; 5 research tools; 6 Other; 7 None.",
    "None combined with another choice (for example 4,7) is preserved and flagged but excluded from Q2 summaries only.",
    "Q3 percentages use valid Q3 respondents. percent_all uses every retained respondent row.",
    "Q3 default codes follow the displayed order: 1 Never; 2 Once or twice; 3 Occasionally; 4 Frequently; 5 Very frequently.",
    "Likert summaries use available valid ratings for each item; missing values are never zero.",
    "Agreement means 4 or 5. Standard deviation is the sample SD; no composite scale is assumed.",
    "Means treat the 1-5 ratings numerically; medians and full distributions are also provided.",
    "Inspect data_issues.csv and response_status.csv, especially when the export uses numeric Q2/Q3 codes.",
    "N/A markers are counted separately from blanks. Unrecognized ratings are invalid and excluded.",
    "source_row identifies the worksheet row assuming header_row points to the actual header.",
    "Running again in the same output folder overwrites generated files."), file.path(output_dir, "analysis_notes.txt"))
  message("Analyzed ", N, " respondent rows; removed ", n_drop, " metadata rows. Results: ", normalizePath(output_dir))
  if (nrow(issues)) warning(nrow(issues), " data issues found; inspect data_issues.csv.", call. = FALSE)
  print(q2_summary)
  print(q3_summary)
  print(likert_summary)
  invisible(tables)
}
