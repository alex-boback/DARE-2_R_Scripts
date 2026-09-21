# Survey analysis Shiny app

Run the app in R or RStudio:

```r
install.packages(c("shiny", "readxl")) # once; readxl is needed for Excel
shiny::runApp("C:/2026 Fall/work/R")
```

If you move the project, use the new folder containing `app.R`. In RStudio,
opening `app.R` and clicking **Run App** also works.

## Spreadsheet format

Upload any number of CSV, XLS, or XLSX files on the **Load data** tab and click
**Add uploaded data**. You can add more files later; **Clear loaded data** starts
over. Every worksheet in an Excel file is loaded. Each file or worksheet
must have:

1. Row 1: unique question names.
2. Row 2: one type per question.
3. Row 3 onward: one response per row.
4. Exactly one column typed `group key`. Its cells contain the time-period or
   population label for each response, such as `Fall 2025` or `Fall 2026`.

For example:

```csv
Period,AI frequency,Tools used,Experience
group key,likert,multiselect,free response
Fall 2025,2,ChatGPT;Claude,Helpful for brainstorming
Fall 2026,4,ChatGPT,Useful for drafting
```

One spreadsheet can contain several time periods. Sheets can have different
question columns, but a question repeated across sheets must have the same name
and type. Analysis of a question uses all loaded sheets that contain it. The
group-key column may have a different question name on each sheet.

Supported types are `likert` (also accepts `linkert`), `continuous`,
`multiselect`, `single select`, `group key`, and `free response`. Likert
values must be 1–5; continuous values must be numeric. Multiselect values use
semicolon-separated choices. The app uses the values as written, with no value
mapping. Blank cells are missing; invalid Likert or continuous values are
excluded and counted.

## Analyze

The **Analyze** tab lets you choose a question, inspect its response counts,
summary, and graph, then run an applicable test across the group-key values in
all loaded sheets. Group-key values represent independent populations, not
respondent IDs. More than two time periods can be compared by tests that
support multiple groups.

The selected question's type appears below the question picker. Use **Show
responses from** to view one group or all groups in the response counts,
summary, graph, and free-response list. Statistical tests always compare all
loaded groups. The **Graph** picker offers charts suited to the question type:
Likert response bars, stacked bars, and boxplots; continuous boxplots,
histograms, density curves, and strip charts; categorical bars or dot plots;
and free-response counts or response lengths.

Likert questions offer Mann-Whitney U and Brunner-Munzel for two groups, and
Kruskal-Wallis with Dunn comparisons for more groups. Continuous questions
also offer independent and Welch t-tests, Yuen's trimmed-mean test, one-way,
Welch, and robust trimmed-mean ANOVA, Tukey and Games-Howell comparisons, and
the Fligner-Killeen spread check. Single-select questions offer Fisher exact
and chi-square. Multiselect questions offer those tests for one selected choice.
Free responses are displayed without an inferential test.

The optional tests require `brunnermunzel`, `WRS2`, or `PMCMRplus`. Install
the ones you need with:

```r
install.packages(c("brunnermunzel", "WRS2", "PMCMRplus"))
```

Paired and repeated-measures tests in [Selecting statistical tests.docx](<Selecting statistical tests.docx>)
are not offered because the spreadsheet format does not identify the same
respondent across periods. The original command-line comparison remains in
`analyze_genai_survey.R`.
