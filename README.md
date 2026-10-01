# SASPy Learner

Interactive R Shiny study bench for people moving between SAS and Python (pandas).

It has three jobs:

1. A short guide to the ideas that do not survive a line-by-line translation.
2. Instant SAS → Python for the DATA steps and PROCs you see every day.
3. Instant Python → SAS for the matching pandas patterns.

The translators are pattern-based teachers, not a SAS compiler. Unrecognised syntax is kept as a `REVIEW` comment instead of being guessed.

Suggested GitHub repository name: **`saspy-learner`**

## What it converts

SAS → Python

- `PROC IMPORT` / `PROC EXPORT` / `PROC PRINT` / `PROC SORT` / `PROC MEANS` / `PROC SUMMARY` / `PROC FREQ`
- Simple `PROC SQL` (`SELECT`, `WHERE`, `GROUP BY`, `ORDER BY`)
- `DATA` step `SET`, `MERGE` with `BY`, `WHERE`, `KEEP`, `DROP`, `RENAME`, assignments, a single `IF`/`THEN`

Python → SAS

- `read_csv`, `read_excel`, `to_csv`, `to_excel`
- `query`, `head`, `sort_values`, `drop`, `rename`, `drop_duplicates`, `fillna`, `loc`
- `merge`, `groupby().agg`, `value_counts`, `crosstab`, `describe`, `np.where`

Not translated on purpose: hash objects, DS2, FCMP, IML, `PROC REPORT`, `PROC TABULATE`, ODS graphics, `RETAIN` / `LAG`, `first.by` / `last.by`, arrays, and macro metaprogramming. Those need a human.

## Run it

Requirements: R 4.1 or newer, and the packages `shiny` and `bslib`.

```r
install.packages(c("shiny", "bslib"))
setwd("path/to/saspy-learner")
shiny::runApp()
```

Or open `saspy-learner.Rproj` in RStudio and click **Run App**.

Load an example, click convert, read the review notes, then copy or download the result.

## Layout

```text
app.R                 Shiny UI and server
R/translate.R         SAS ↔ Python pattern translators
R/content.R           Guide modules, cheat sheet, examples
www/custom.css        Layout
examples/             Sample SAS and Python pipelines
```

## Publish later (optional)

GitHub stores the source. It does not run Shiny. To put the app on the web, use [shinyapps.io](https://www.shinyapps.io/) or Posit Connect:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp()
```

## Licence

MIT. See `LICENSE`.
