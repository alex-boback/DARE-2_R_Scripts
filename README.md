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
4. One or more columns typed `group key`. Examples include time period, age
   range, and gender.

For example:

```csv
Period,Age range,Gender,Confidence,AI frequency,Tools used,Experience
group key,group key,group key,linkert,ordered categorical: Never; Sometimes; Often,multiselect,free response
Fall 2025,18-24,Female,2,Sometimes,ChatGPT;Claude,Helpful for brainstorming
Fall 2026,25-34,Male,4,Often,ChatGPT,Useful for drafting
```

One spreadsheet can contain several time periods. Sheets can have different
question columns, but a question repeated across sheets must have the same name
and type. Analysis of a question uses all loaded sheets that contain it. To
group across several sheets by a named key, that key must have the same column
name and be marked `group key` in every sheet containing the question. If
sheets use different names for their sole group key, choose **First group key
in each sheet**.

Supported types are `linkert`, `ordered categorical`, `continuous`,
`multiselect`, `single select`, `group key`, and `free response`. Linkert
values must be 1–5; continuous values must be numeric. For an ordered
categorical question, put its levels in order in the type cell, separated by
semicolons: `ordered categorical: Low; Medium; High`. The same question
must declare the same levels in every sheet. Multiselect responses also use
semicolons to separate selected choices. The app uses the values as written,
with no value mapping. Blank or invalid responses are excluded and counted.

## Analyze

The **Analyze** tab lets you choose a question and one or more **Group by**
keys, inspect its response counts, summary, and graph, then run an applicable
test across the resulting groups in all loaded sheets. For example, choose
`Age range`, `Gender`, or both. Choosing both compares age-by-gender
combinations (such as `Age range=18-24 | Gender=Female`). Group-key values
represent independent populations, not respondent IDs. Tests that support
multiple groups can compare more than two combinations.

The selected question's type appears below the question picker. Use **Show
responses from** to view one group in the response counts,
summary, graph, and free-response list. Statistical tests always compare all
loaded groups. Select **All groups** to return to the combined view. The
**Graph** picker offers charts suited to the question type:
Linkert response bars, stacked bars, and boxplots; ordered categorical
response bars, stacked bars, and dot plots; continuous boxplots, histograms,
density curves, and strip charts; other categorical bars or dot plots;
and free-response counts or response lengths.

Linkert and ordered categorical questions offer Mann-Whitney U and
Brunner-Munzel for two groups, and Kruskal-Wallis with Dunn comparisons for
more groups. Ordered categorical tests use the declared level order.
Continuous questions
also offer independent and Welch t-tests, Yuen's trimmed-mean test, one-way,
Welch, and robust trimmed-mean ANOVA, Tukey and Games-Howell comparisons, and
the Fligner-Killeen spread check. Single-select questions offer Fisher exact
and chi-square. Multiselect questions offer those tests for one selected choice.
Free responses are displayed without an inferential test.

## Compile a report

Use the **Report** tab to build an ordered report one question at a time.
Choose a question, its **Group by** keys, and optionally a single resulting
group for its descriptive sections.
Select the response counts, summary, graph, statistical test, or free responses
you want to include. You can add notes for each question, remove an added
section, preview the full report, and download a self-contained HTML file.
Statistical tests in the report use all loaded groups, even when its
descriptive sections show one group. Reports are generated from the currently
loaded data; clearing data also clears the report selection.

The optional tests require `brunnermunzel`, `WRS2`, or `PMCMRplus`. Install
the ones you need with:

```r
install.packages(c("brunnermunzel", "WRS2", "PMCMRplus"))
```

Paired and repeated-measures tests in [Selecting statistical tests.docx](<Selecting statistical tests.docx>)
are not offered because the spreadsheet format does not identify the same
respondent across periods. The original command-line comparison remains in
`analyze_genai_survey.R`.
