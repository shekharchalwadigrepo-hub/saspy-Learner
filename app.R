source("R/translate.R", local = TRUE)
source("R/content.R", local = TRUE)

library(shiny)
library(bslib)

modules <- guide_modules()
sheet <- cheat_sheet()
sas_ex <- sas_examples()
py_ex <- python_examples()

theme <- bs_theme(
  version = 5,
  bg = "#f4f6f8",
  fg = "#1c2430",
  primary = "#1b4f72",
  secondary = "#1f6b4a",
  base_font = font_google("Source Sans 3"),
  code_font = font_google("IBM Plex Mono"),
  heading_font = font_google("Source Sans 3")
)

guide_panels <- lapply(modules, function(mod) {
    accordion_panel(
      mod$title,
      p(mod$summary),
      div(
        class = "pair",
        div(tags$div(class = "sas-label", "SAS"), tags$pre(class = "snippet", mod$sas)),
        div(tags$div(class = "py-label", "Python"), tags$pre(class = "snippet", mod$python))
      ),
      tags$ul(class = "tip-list", lapply(mod$tips, tags$li))
    )
  })

ui <- page_navbar(
  title = "SASPy Learner",
  theme = theme,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(HTML(
      "function copyOut(id){var el=document.getElementById(id);if(!el)return;var text=el.innerText||el.textContent||'';navigator.clipboard.writeText(text);}"
    ))
  ),
  nav_panel(
    "Start",
    div(
      class = "app-hero",
      div(class = "kicker", "SAS to Python learner studio"),
      h1("Learn the mapping. Convert the common patterns."),
      p("A study bench for programmers moving between SAS and pandas. The guide explains the idea. The converters rewrite the statements you will see every day, and they flag anything a pattern match cannot honestly translate.")
    ),
    layout_column_wrap(
      width = 1/3,
      card(card_header("Guide"), card_body("Twelve short modules: data step versus DataFrame, filters, recodes, summaries, merges, dates, macros.")),
      card(card_header("SAS to Python"), card_body("Paste a DATA step or PROC. Get pandas, plus notes where SAS missing-value or MERGE rules differ.")),
      card(card_header("Python to SAS"), card_body("Paste a pandas pipeline. Get PROC IMPORT, DATA step, PROC MEANS, PROC FREQ, or a match-merge sketch."))
    ),
    div(
      class = "footer-note",
      "This is a learner, not a compiler. It will not translate hash objects, DS2, FCMP, IML, graph procedures, or arbitrary macro metaprogramming. Review every result before it touches a study."
    )
  ),
  nav_panel(
    "Guide",
    do.call(accordion, c(list(id = "guide_acc", open = FALSE), guide_panels))
  ),
  nav_panel(
    "SAS to Python",
    layout_columns(
      col_widths = c(6, 6),
      div(
        class = "panel",
        h3(class = "sas-label", "SAS"),
        selectInput("sas_example", "Load an example", choices = c("Custom", names(sas_ex))),
        textAreaInput("sas_code", NULL, rows = 16, width = "100%", resize = "vertical", placeholder = "Paste a DATA step or PROC ... RUN;"),
        checkboxInput("s2p_imports", "Add pandas and numpy imports", TRUE),
        actionButton("convert_s2p", "Convert to Python", class = "btn-primary"),
        downloadButton("download_py", "Download .py")
      ),
      div(
        class = "panel",
        h3(class = "py-label", "Python"),
        verbatimTextOutput("py_out"),
        actionButton("copy_py", "Copy Python", onclick = "copyOut('py_out')"),
        uiOutput("s2p_notes")
      )
    )
  ),
  nav_panel(
    "Python to SAS",
    layout_columns(
      col_widths = c(6, 6),
      div(
        class = "panel",
        h3(class = "py-label", "Python"),
        selectInput("py_example", "Load an example", choices = c("Custom", names(py_ex))),
        textAreaInput("py_code", NULL, rows = 16, width = "100%", resize = "vertical", placeholder = "Paste pandas code"),
        actionButton("convert_p2s", "Convert to SAS", class = "btn-primary"),
        downloadButton("download_sas", "Download .sas")
      ),
      div(
        class = "panel",
        h3(class = "sas-label", "SAS"),
        verbatimTextOutput("sas_out"),
        actionButton("copy_sas", "Copy SAS", onclick = "copyOut('sas_out')"),
        uiOutput("p2s_notes")
      )
    )
  ),
  nav_panel(
    "Cheat sheet",
    div(class = "panel", tableOutput("sheet"))
  ),
  nav_panel(
    "How to run",
    div(
      class = "panel",
      h3("On your machine"),
      tags$pre(class = "snippet", "install.packages(c(\"shiny\", \"bslib\"))\nshiny::runApp()"),
      h3("What the converter will handle"),
      tags$ul(
        tags$li("PROC IMPORT / EXPORT, PRINT, SORT, MEANS, SUMMARY, FREQ, a simple PROC SQL, CONTENTS"),
        tags$li("DATA step SET, simple MERGE BY, WHERE, KEEP, DROP, RENAME, assignments, a single IF/THEN"),
        tags$li("pandas read_csv, read_excel, to_csv, query, sort_values, drop, rename, merge, groupby.agg, value_counts, crosstab, fillna, loc, np.where")
      ),
      h3("What you still translate by hand"),
      tags$ul(
        tags$li("Arrays and DO loops that walk columns"),
        tags$li("RETAIN, LAG, and first.by / last.by logic"),
        tags$li("Formats, informats, and PROC FORMAT"),
        tags$li("Macro loops that generate code"),
        tags$li("Hash objects, PROC REPORT, PROC TABULATE, ODS graphics")
      )
    )
  )
)

server <- function(input, output, session) {
  py_state <- reactiveVal("")
  sas_state <- reactiveVal("")
  s2p_note_state <- reactiveVal(character())
  p2s_note_state <- reactiveVal(character())

  observeEvent(input$sas_example, {
    if (identical(input$sas_example, "Custom")) return()
    updateTextAreaInput(session, "sas_code", value = sas_ex[[input$sas_example]])
  }, ignoreInit = TRUE)

  observeEvent(input$py_example, {
    if (identical(input$py_example, "Custom")) return()
    updateTextAreaInput(session, "py_code", value = py_ex[[input$py_example]])
  }, ignoreInit = TRUE)

  observeEvent(input$convert_s2p, {
    result <- sas_to_python(input$sas_code %||% "", add_imports = isTRUE(input$s2p_imports))
    py_state(result$code)
    s2p_note_state(result$notes)
  })

  observeEvent(input$convert_p2s, {
    result <- python_to_sas(input$py_code %||% "")
    sas_state(result$code)
    p2s_note_state(result$notes)
  })

  output$py_out <- renderText(py_state())
  output$sas_out <- renderText(sas_state())
  output$sheet <- renderTable(sheet, striped = TRUE, spacing = "s", width = "100%")

  output$s2p_notes <- renderUI(note_ui(s2p_note_state()))
  output$p2s_notes <- renderUI(note_ui(p2s_note_state()))

  output$download_py <- downloadHandler(
    filename = function() "translated_from_sas.py",
    content = function(file) writeLines(py_state(), file)
  )
  output$download_sas <- downloadHandler(
    filename = function() "translated_from_python.sas",
    content = function(file) writeLines(sas_state(), file)
  )
}

note_ui <- function(notes) {
  if (!length(notes)) return(NULL)
  div(class = "note-box", tags$strong("Review notes"), tags$ul(lapply(notes, tags$li)))
}

app <- shinyApp(ui, server)
