# ============================================================
# FINAL SURVEY ANALYSIS PDF SCRIPT
# Portrait letter PDF, 3 questions per page
# Left: counts/statistics table | Right: readable graph
#
# Uses only inferential tests named in the supplied document:
# - Mann-Whitney U for ordinal, independent groups with similar
#   distribution shape/spread
# - Brunner-Munzel for ordinal, independent groups with different
#   distribution shape/spread
# - Counts and percentages only for nominal/multi-select questions
#
# Excludes:
# - Study Agreement
# - Did you use a generative AI tool for the assignment?
# ============================================================

# -----------------------------
# 1. FILE LOCATIONS
# -----------------------------
analysis_folder <- "C:/Users/qha2he/OneDrive - University of Virginia/Work/2026_fall"
setwd(analysis_folder)

excel_file <- file.path(analysis_folder, "Lit_review_survey_data.xlsx")
output_pdf <- file.path(analysis_folder, "survey_final.pdf")

if (!file.exists(excel_file)) {
  stop("Cannot find: ", excel_file)
}

message("Analysis folder: ", getwd())

# -----------------------------
# 2. PACKAGES
# -----------------------------
project_library <- file.path(analysis_folder, "r_packages")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library, winslash = "/"), .libPaths()))
}

required_packages <- c(
  "readxl", "dplyr", "ggplot2", "stringr", "purrr",
  "scales", "brunnermunzel"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org",
    type = "binary"
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(scales)
  library(grid)
  library(brunnermunzel)
})

options(dplyr.summarise.inform = FALSE, scipen = 999)

# -----------------------------
# 3. OPTIONAL SHAPE OVERRIDES
# -----------------------------
# The source document requires a visual comparison of ordinal
# distribution shape. By default, the script treats shape as similar
# unless spread differs according to Fligner-Killeen.
#
# After reviewing the graphs, place the exact wording of any ordinal
# question with clearly different distribution shapes in this vector.
# Example:
# different_shape_questions <- c("Exact question wording")
different_shape_questions <- character(0)

# -----------------------------
# 4. READ AND PARSE COUNTS SHEET
# -----------------------------
raw_counts <- readxl::read_excel(
  excel_file,
  sheet = "Question Counts",
  col_names = FALSE,
  .name_repair = "minimal"
)

counts <- raw_counts[-1, ]
names(counts) <- c(
  "Audience", "Question", "Row_Type",
  paste0("Option_", 1:11),
  "Suggested_Test", "Justification"
)

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[\u2018\u2019]", "'")
  x <- stringr::str_replace_all(x, "[\u201C\u201D]", "\"")
  stringr::str_squish(x)
}

counts <- counts %>% mutate(across(everything(), clean_text))
answer_rows <- which(counts$Row_Type == "Answers" & !is.na(counts$Audience))

parse_question <- function(i) {
  option_values <- unlist(
    counts[i, paste0("Option_", 1:11)],
    use.names = FALSE
  )
  keep <- !is.na(option_values) & option_values != ""
  options <- as.character(option_values[keep])

  get_counts <- function(row_index) {
    values <- suppressWarnings(as.numeric(unlist(
      counts[row_index, paste0("Option_", 1:11)],
      use.names = FALSE
    )))
    values[keep]
  }

  ai <- NULL
  non_ai <- NULL

  if (i + 1 <= nrow(counts) && identical(counts$Row_Type[i + 1], "AI counts")) {
    ai <- get_counts(i + 1)
  }
  if (i + 1 <= nrow(counts) && identical(counts$Row_Type[i + 1], "Non-AI counts")) {
    non_ai <- get_counts(i + 1)
  }
  if (i + 2 <= nrow(counts) && identical(counts$Row_Type[i + 2], "Non-AI counts")) {
    non_ai <- get_counts(i + 2)
  }

  list(
    audience = as.character(counts$Audience[i]),
    question = as.character(counts$Question[i]),
    options = options,
    ai = ai,
    non_ai = non_ai,
    suggested_test = as.character(counts$Suggested_Test[i])
  )
}

questions_all <- purrr::map(answer_rows, parse_question)

classification_matches <- purrr::keep(
  questions_all,
  ~ stringr::str_detect(
    .x$question,
    stringr::regex("Did you use a generative AI tool", ignore_case = TRUE)
  )
)

if (length(classification_matches) == 0) {
  stop("Could not find the AI-use classification question needed for group sizes.")
}

classification <- classification_matches[[1]]
N_AI <- sum(classification$ai, na.rm = TRUE)
N_NON_AI <- sum(classification$non_ai, na.rm = TRUE)

questions <- purrr::discard(
  questions_all,
  ~ stringr::str_detect(
    .x$question,
    stringr::regex(
      "Study Agreement|Did you use a generative AI tool",
      ignore_case = TRUE
    )
  )
)

message("AI users: ", N_AI)
message("Non-AI users: ", N_NON_AI)
message("Questions included: ", length(questions))

# -----------------------------
# 5. HELPER FUNCTIONS
# -----------------------------
`%||%` <- function(x, replacement) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) replacement else x
}

format_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("< .001")
  sprintf("%.3f", p)
}

format_num <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("NA")
  sprintf(paste0("%.", digits, "f"), x)
}

format_count_percent <- function(count, denominator) {
  if (is.na(count) || is.na(denominator) || denominator <= 0) return("NA")
  paste0(count, " (", scales::percent(count / denominator, accuracy = 1), ")")
}

is_multiselect <- function(q) {
  stringr::str_detect(
    q$question,
    stringr::regex("select all that apply", ignore_case = TRUE)
  ) || stringr::str_detect(
    q$suggested_test %||% "",
    stringr::regex("multi-select|optional chi-square", ignore_case = TRUE)
  )
}

is_ordinal <- function(q) {
  stringr::str_detect(
    q$suggested_test %||% "",
    stringr::regex("Mann-Whitney|Brunner-Munzel", ignore_case = TRUE)
  )
}

ordinal_scores <- function(labels) {
  low <- stringr::str_to_lower(labels)
  if (any(stringr::str_detect(low, "agree|disagree|neutral"))) {
    scores <- dplyr::case_when(
      stringr::str_detect(low, "strongly disagree") ~ 1,
      stringr::str_detect(low, "^disagree$") ~ 2,
      stringr::str_detect(low, "neutral|neither agree") ~ 3,
      stringr::str_detect(low, "^agree$") ~ 4,
      stringr::str_detect(low, "strongly agree") ~ 5,
      TRUE ~ NA_real_
    )
    if (!any(is.na(scores))) return(scores)
  }
  seq_along(labels)
}

mode_text <- function(values) {
  tab <- table(values)
  paste(names(tab)[tab == max(tab)], collapse = ", ")
}

# -----------------------------
# 6. COUNT/PERCENTAGE TABLE DATA
# -----------------------------
make_count_table <- function(q) {
  if (q$audience == "Both") {
    ai_counts <- as.numeric(q$ai)
    non_counts <- as.numeric(q$non_ai)
    ai_den <- if (is_multiselect(q)) N_AI else sum(ai_counts, na.rm = TRUE)
    non_den <- if (is_multiselect(q)) N_NON_AI else sum(non_counts, na.rm = TRUE)

    data.frame(
      Response = stringr::str_wrap(q$options, 24),
      `AI n (%)` = vapply(
        ai_counts,
        function(x) format_count_percent(x, ai_den),
        character(1)
      ),
      `Non-AI n (%)` = vapply(
        non_counts,
        function(x) format_count_percent(x, non_den),
        character(1)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    group_counts <- if (q$audience == "AI users") as.numeric(q$ai) else as.numeric(q$non_ai)
    group_n <- if (q$audience == "AI users") N_AI else N_NON_AI
    denominator <- if (is_multiselect(q)) group_n else sum(group_counts, na.rm = TRUE)

    data.frame(
      Response = stringr::str_wrap(q$options, 30),
      `n (%)` = vapply(
        group_counts,
        function(x) format_count_percent(x, denominator),
        character(1)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

# -----------------------------
# 7. ORDINAL STATISTICS
# -----------------------------
analyze_ordinal <- function(q) {
  scores <- ordinal_scores(q$options)
  ai_values <- rep(scores, times = as.integer(q$ai))
  non_values <- rep(scores, times = as.integer(q$non_ai))

  descriptive <- data.frame(
    Statistic = c("n", "Median", "IQR", "Mode", "Range"),
    AI = c(
      length(ai_values),
      format_num(median(ai_values), 1),
      format_num(IQR(ai_values), 1),
      mode_text(ai_values),
      paste0(min(ai_values), "-", max(ai_values))
    ),
    `Non-AI` = c(
      length(non_values),
      format_num(median(non_values), 1),
      format_num(IQR(non_values), 1),
      mode_text(non_values),
      paste0(min(non_values), "-", max(non_values))
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fligner <- tryCatch(
    stats::fligner.test(list(ai_values, non_values)),
    error = function(e) NULL
  )
  fligner_stat <- if (is.null(fligner)) NA_real_ else unname(fligner$statistic)
  fligner_p <- if (is.null(fligner)) NA_real_ else fligner$p.value

  different_shape <- q$question %in% different_shape_questions
  different_spread <- !is.na(fligner_p) && fligner_p < 0.05

  if (different_shape || different_spread) {
    bm <- brunnermunzel::brunnermunzel.test(ai_values, non_values)
    bm_stat <- unname(bm$statistic)
    bm_df <- if (is.null(bm$parameter)) NA_real_ else unname(bm$parameter)
    bm_est <- if (is.null(bm$estimate)) NA_real_ else unname(bm$estimate[1])
    bm_ci <- if (!is.null(bm$conf.int) && length(bm$conf.int) >= 2) {
      paste0(format_num(bm$conf.int[1], 3), " to ", format_num(bm$conf.int[2], 3))
    } else {
      "Not provided"
    }

    test <- data.frame(
      Statistic = c(
        "Test", "Justification", "BM statistic", "df", "p",
        "Probability estimate", "95% CI", "Fligner X2", "Fligner p"
      ),
      Value = c(
        "Brunner-Munzel",
        "Ordinal; independent; different shape/spread.",
        format_num(bm_stat, 3), format_num(bm_df, 2), format_p(bm$p.value),
        format_num(bm_est, 3), bm_ci,
        format_num(fligner_stat, 2), format_p(fligner_p)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    mw <- stats::wilcox.test(
      ai_values,
      non_values,
      exact = FALSE,
      correct = TRUE
    )

    # For two independent groups, wilcox.test() reports the rank-sum W.
    # Convert W to the Mann-Whitney U for the first group.
    rank_sum_w <- unname(mw$statistic)
    n1 <- length(ai_values)
    n2 <- length(non_values)
    U <- rank_sum_w - n1 * (n1 + 1) / 2
    rank_biserial <- 2 * U / (n1 * n2) - 1

    test <- data.frame(
      Statistic = c(
        "Test", "Justification", "U", "p", "Rank-biserial r",
        "Fligner X2", "Fligner p"
      ),
      Value = c(
        "Mann-Whitney U",
        "Ordinal; independent; similar shape/spread.",
        format_num(U, 1), format_p(mw$p.value), format_num(rank_biserial, 3),
        format_num(fligner_stat, 2), format_p(fligner_p)
      ),
      stringsAsFactors = FALSE
    )
  }

  list(descriptive = descriptive, test = test)
}
str(questions[[30]]$question)
str(questions[[30]]$options)
str(questions[[1]]$ai)
str(questions[[1]]$non_ai)
str(questions[[1]]$audience)
# -----------------------------
# 8. SAFE GRID TABLE DRAWING
# -----------------------------
# This draws tables directly with grid primitives. It intentionally
# avoids gridExtra/tableGrob, which caused the prior "language types"
# comparison error on the user's package versions.


# ============================================================
# SURVEY ANALYSIS HELPER FUNCTIONS
#
# Assumes these already exist:
#   questions
#   N_AI
#   N_NON_AI
#
# Required package:
#   brunnermunzel
# ============================================================




# ============================================================
# 2. INSPECT A QUESTION BEFORE ANALYZING IT
# ============================================================

inspect_question <- function(q) {
  
  cat("\nQUESTION:\n")
  cat(q$question, "\n\n")
  
  cat("Audience:", q$audience, "\n")
  
  if (!is.null(q$suggested_test)) {
    cat("Suggested test:", q$suggested_test, "\n")
  }
  
  cat("\n")
  
  result <- data.frame(
    Option = q$options,
    AI = q$ai,
    Non_AI = q$non_ai,
    check.names = FALSE
  )
  
  print(result, row.names = FALSE)
  
  invisible(result)
}


# ============================================================
# 3. DETECT MULTI-SELECT QUESTIONS
# ============================================================


# ============================================================
# 4. CONVERT ORDINAL ANSWER CHOICES TO NUMERIC RANKS
#
# Likert:
#   Strongly Disagree = 1
#   Disagree          = 2
#   Neutral           = 3
#   Agree             = 4
#   Strongly Agree    = 5
#
# Other ordinal questions:
#   assumes q$options are already ordered lowest -> highest
# ============================================================

ordinal_scores <- function(labels) {
  
  labels_lower <- tolower(trimws(labels))
  
  # Check whether this looks like a Likert scale
  is_likert <- any(
    grepl(
      "agree|disagree|neutral",
      labels_lower
    )
  )
  
  if (is_likert) {
    
    scores <- rep(NA_real_, length(labels_lower))
    
    scores[
      grepl("strongly disagree", labels_lower)
    ] <- 1
    
    scores[
      labels_lower == "disagree"
    ] <- 2
    
    scores[
      grepl("neutral|neither agree", labels_lower)
    ] <- 3
    
    scores[
      labels_lower == "agree"
    ] <- 4
    
    scores[
      grepl("strongly agree", labels_lower)
    ] <- 5
    
    if (!any(is.na(scores))) {
      return(scores)
    }
  }
  
  # Otherwise assume options are stored from lowest to highest
  seq_along(labels)
}


# ============================================================
# 5. RECONSTRUCT ORDINAL RESPONSES FROM COUNTS
#
# Example:
# scores = 1 2 3 4
# counts = 2 3 1 0
#
# becomes:
# 1 1 2 2 2 3
# ============================================================

get_ordinal_values <- function(q) {
  
  if (is.null(q$ai) || is.null(q$non_ai)) {
    stop(
      "This question does not contain both AI and Non-AI counts."
    )
  }
  
  scores <- ordinal_scores(q$options)
  
  ai_values <- rep(
    scores,
    times = as.integer(q$ai)
  )
  
  non_ai_values <- rep(
    scores,
    times = as.integer(q$non_ai)
  )
  
  list(
    scores = scores,
    ai = ai_values,
    non_ai = non_ai_values
  )
}


# ============================================================
# 6. BRUNNER-MUNZEL TEST
#
# Use for:
#   - ordinal questions
#   - Likert questions
#   - AI vs. Non-AI
#
# Good default when distributions may have different
# shapes or spreads.
# ============================================================

run_brunner_munzel <- function(q) {
  
  values <- get_ordinal_values(q)
  
  ai <- values$ai
  non_ai <- values$non_ai
  
  test <- brunnermunzel::brunnermunzel.test(
    ai,
    non_ai
  )
  
  result <- data.frame(
    Question = q$question,
    
    AI_n = length(ai),
    AI_median = median(ai),
    AI_IQR = IQR(ai),
    
    NonAI_n = length(non_ai),
    NonAI_median = median(non_ai),
    NonAI_IQR = IQR(non_ai),
    
    BM_statistic = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value,
    
    probability_estimate =
      unname(test$estimate[1]),
    
    CI_lower = test$conf.int[1],
    CI_upper = test$conf.int[2],
    
    check.names = FALSE
  )
  
  result
}


# ============================================================
# 7. MANN-WHITNEY U TEST
#
# Use for:
#   - ordinal questions
#   - Likert questions
#   - two independent groups
#
# Most appropriate when distributions are reasonably
# similar in shape/spread.
# ============================================================

run_mann_whitney <- function(q) {
  
  values <- get_ordinal_values(q)
  
  ai <- values$ai
  non_ai <- values$non_ai
  
  test <- wilcox.test(
    ai,
    non_ai,
    exact = FALSE,
    correct = TRUE
  )
  
  n1 <- length(ai)
  n2 <- length(non_ai)
  
  # R reports the two-sample statistic on the
  # Mann-Whitney U scale.
  U <- unname(test$statistic)
  
  # Rank-biserial correlation
  rank_biserial <-
    (2 * U) / (n1 * n2) - 1
  
  result <- data.frame(
    Question = q$question,
    
    AI_n = n1,
    AI_median = median(ai),
    AI_IQR = IQR(ai),
    
    NonAI_n = n2,
    NonAI_median = median(non_ai),
    NonAI_IQR = IQR(non_ai),
    
    U = U,
    p_value = test$p.value,
    rank_biserial = rank_biserial,
    
    check.names = FALSE
  )
  
  result
}


# ============================================================
# 8. FISHER'S EXACT TEST FOR MULTI-SELECT QUESTIONS
#
# Runs one 2x2 Fisher test for EACH answer option:
#
#              Selected    Not Selected
# AI
# Non-AI
#
# Then applies Holm correction to all p-values
# from that question.
# ============================================================

run_multiselect_fisher <- function(
    q,
    n_ai = N_AI,
    n_non_ai = N_NON_AI
) {
  
  if (is.null(q$ai) || is.null(q$non_ai)) {
    stop(
      "This question does not contain both AI and Non-AI counts."
    )
  }
  
  results <- vector(
    "list",
    length(q$options)
  )
  
  for (i in seq_along(q$options)) {
    
    ai_yes <- as.numeric(q$ai[i])
    non_ai_yes <- as.numeric(q$non_ai[i])
    
    ai_no <- n_ai - ai_yes
    non_ai_no <- n_non_ai - non_ai_yes
    
    if (
      ai_no < 0 ||
      non_ai_no < 0
    ) {
      stop(
        "A selected count is larger than the group size."
      )
    }
    
    tab <- matrix(
      c(
        ai_yes,
        ai_no,
        non_ai_yes,
        non_ai_no
      ),
      nrow = 2,
      byrow = TRUE
    )
    
    rownames(tab) <- c(
      "AI",
      "Non-AI"
    )
    
    colnames(tab) <- c(
      "Selected",
      "Not selected"
    )
    cat(tab)
    test <- fisher.test(tab)
    
    # Fisher's estimate can occasionally be missing
    # in extreme tables.
    odds_ratio <- if (
      length(test$estimate) > 0
    ) {
      unname(test$estimate)
    } else {
      NA_real_
    }
    
    results[[i]] <- data.frame(
      Option = q$options[i],
      
      AI_selected = ai_yes,
      AI_percent =
        ai_yes / n_ai * 100,
      
      NonAI_selected = non_ai_yes,
      NonAI_percent =
        non_ai_yes / n_non_ai * 100,
      
      Odds_Ratio = odds_ratio,
      p_value = test$p.value,
      
      check.names = FALSE
    )
  }
  
  results <- do.call(
    rbind,
    results
  )
  
  # Correct for running multiple tests
  # within the same survey question.
  results$p_holm <- p.adjust(
    results$p_value,
    method = "holm"
  )
  
  rownames(results) <- NULL
  
  results
}


# ============================================================
# 9. FISHER EXACT TEST FOR A SINGLE-CHOICE
#    NOMINAL QUESTION
#
# Use when:
#   - AI and Non-AI both answered
#   - respondent chooses ONE answer
#   - categories are NOT naturally ordered
#
# Example:
#   Future research strategy
#
# R's fisher.test() handles the full 2 x k table.
# Also calculates Cramer's V as an effect size.
# ============================================================

run_nominal_fisher <- function(q) {
  
  if (is.null(q$ai) || is.null(q$non_ai)) {
    stop(
      "This question does not contain both AI and Non-AI counts."
    )
  }
  
  tab <- rbind(
    AI = as.numeric(q$ai),
    Non_AI = as.numeric(q$non_ai)
  )
  
  colnames(tab) <- q$options
  
  # Remove answer categories chosen by nobody.
  tab <- tab[
    ,
    colSums(tab) > 0,
    drop = FALSE
  ]
  
  if (ncol(tab) < 2) {
    stop(
      "Not enough non-empty response categories to compare."
    )
  }
  
  # Exact overall association test
  fisher_result <- fisher.test(tab)
  
  # Pearson statistic used only to calculate Cramer's V
  chi_result <- suppressWarnings(
    chisq.test(
      tab,
      correct = FALSE
    )
  )
  
  n <- sum(tab)
  
  cramers_v <- sqrt(
    unname(chi_result$statistic) /
      (
        n *
          min(
            nrow(tab) - 1,
            ncol(tab) - 1
          )
      )
  )
  
  list(
    question = q$question,
    table = tab,
    p_value = fisher_result$p.value,
    cramers_v = cramers_v
  )
}


# ============================================================
# 10. DESCRIPTIVE RESULTS FOR ONE-GROUP QUESTIONS
#
# Works for:
#   - AI-only questions
#   - Non-AI-only questions
#
# Automatically detects most multi-select questions.
#
# For multi-select:
#   denominator = total number of students in group
#
# For single-choice:
#   denominator = total responses to question
# ============================================================

describe_question <- function(
    q,
    multiselect = NULL,
    n_ai = N_AI,
    n_non_ai = N_NON_AI
) {
  
  if (q$audience == "AI users") {
    
    counts <- as.numeric(q$ai)
    group <- "AI"
    group_size <- n_ai
    
  } else if (
    q$audience == "Non-AI users"
  ) {
    
    counts <- as.numeric(q$non_ai)
    group <- "Non-AI"
    group_size <- n_non_ai
    
  } else {
    
    stop(
      "describe_question() is intended for a one-group question."
    )
  }
  
  # Automatically detect multi-select unless manually specified
  if (is.null(multiselect)) {
    multiselect <- is_multiselect(q)
  }
  
  if (multiselect) {
    
    denominator <- group_size
    
  } else {
    
    denominator <- sum(
      counts,
      na.rm = TRUE
    )
  }
  
  result <- data.frame(
    Response = q$options,
    Count = counts,
    Percent =
      counts / denominator * 100,
    check.names = FALSE
  )
  
  attr(result, "question") <- q$question
  attr(result, "group") <- group
  
  result
}


# ============================================================
# 11. DESCRIPTIVE RESULTS FOR A ONE-GROUP
#     ORDINAL / LIKERT QUESTION
#
# Gives:
#   n
#   median
#   IQR
#   minimum rank
#   maximum rank
# ============================================================

describe_ordinal <- function(q) {
  
  scores <- ordinal_scores(
    q$options
  )
  
  if (q$audience == "AI users") {
    
    values <- rep(
      scores,
      times = as.integer(q$ai)
    )
    
    group <- "AI"
    
  } else if (
    q$audience == "Non-AI users"
  ) {
    
    values <- rep(
      scores,
      times = as.integer(q$non_ai)
    )
    
    group <- "Non-AI"
    
  } else {
    
    stop(
      "describe_ordinal() is intended for a one-group question."
    )
  }
  
  data.frame(
    Question = q$question,
    Group = group,
    n = length(values),
    Median = median(values),
    IQR = IQR(values),
    Min = min(values),
    Max = max(values),
    check.names = FALSE
  )
}


# RUN ALL ANALYSES AND STORE RESULTS IN ONE LIST
# ============================================================

results <- list(
  
  # -----------------------------
  # AI vs. Non-AI questions
  # -----------------------------
  
  q1 = run_multiselect_fisher(questions[[1]]),
  
  q2 = run_brunner_munzel(questions[[2]]),
  
  q3 = run_multiselect_fisher(questions[[3]]),
  
  q4 = run_brunner_munzel(questions[[4]]),
  
  q5 = run_brunner_munzel(questions[[5]]),
  
  q6 = run_multiselect_fisher(questions[[6]]),
  
  q7 = run_multiselect_fisher(questions[[7]]),
  
  q8 = run_nominal_fisher(questions[[8]]),
  
  q9 = run_brunner_munzel(questions[[9]]),
  
  
  # -----------------------------
  # AI-only questions
  # -----------------------------
  
  q10 = describe_question(questions[[10]]),
  
  q11 = describe_question(questions[[11]]),
  
  q12 = describe_question(questions[[12]]),
  
  q13 = describe_question(questions[[13]]),
  
  # Corrupted response labels - skip for now
  q14 = describe_ordinal(questions[[14]]),
  
  q15 = describe_question(questions[[15]]),
  
  q16 = describe_question(questions[[16]]),
  
  q17 = describe_question(questions[[17]]),
  
  q18 = describe_question(questions[[18]]),
  
  q19 = describe_question(questions[[19]]),
  
  q20 = describe_question(questions[[20]]),
  
  q21 = describe_question(questions[[21]]),
  
  q22 = describe_question(questions[[22]]),
  
  q23 = describe_question(questions[[23]]),
  
  
  # -----------------------------
  # AI-only ordinal / Likert
  # -----------------------------
  
  q24 = describe_ordinal(questions[[24]]),
  
  q25 = describe_ordinal(questions[[25]]),
  
  q26 = describe_ordinal(questions[[26]]),
  
  q27 = describe_ordinal(questions[[27]]),
  
  q28 = describe_ordinal(questions[[28]]),
  
  q29 = describe_ordinal(questions[[29]]),
  
  q30 = describe_ordinal(questions[[30]]),
  
  q31 = describe_ordinal(questions[[31]]),
  
  q32 = describe_ordinal(questions[[32]]),
  
  
  # -----------------------------
  # Confidence-change questions
  # -----------------------------
  
  q33 = describe_question(questions[[33]]),
  
  q34 = describe_question(questions[[34]]),
  
  q35 = describe_question(questions[[35]]),
  
  q36 = describe_question(questions[[36]]),
  
  q37 = describe_question(questions[[37]]),
  
  
  # -----------------------------
  # Non-AI-only questions
  # -----------------------------
  
  q38 = describe_question(questions[[38]]),
  
  q39 = describe_question(questions[[39]]),
  
  q40 = describe_question(questions[[40]])
)
str(results)
