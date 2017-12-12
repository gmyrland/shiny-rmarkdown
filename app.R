library(shiny)
library(shinythemes)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(knitr)

# default rmarkdown script
default <- readChar("www/default.Rmd", file.info("www/default.Rmd")$size)

# wrapper for textAreaInput
rmarkdownInput <- function (inputId, label, value = "", width = '100%',
                            height = NULL, cols = NULL, rows = NULL,
                            placeholder = NULL) {
  value <- restoreInput(id = inputId, default = value)
  style <- paste("max-width: 100%; width: ", validateCssUnit(width), ";",
              if (!is.null(height))
                paste0("height: ", validateCssUnit(height), ";"),
              "resize: vertical;")
  div(class = "form-group",
      tags$label(label, `for` = inputId),
      tags$textarea(id = inputId, class = "form-control",
        placeholder=placeholder, style=style, rows=rows, cols=cols, value))
}

# generate an html_fragement from rmarkdown text
render_html_fragment <- function(rmarkdown) {
  t <- tempfile(fileext = '.Rmd')
  cat(rmarkdown, file = t)
  on.exit(unlink(sub('.html$', '*', t)), add = TRUE)
  
  # render
  t <- render(
    input = t,
    runtime = "shiny",
    output_format = html_fragment(),
    envir = new.env()
  )
  # results
  withMathJax(HTML(readLines(t)))
}

# render various rmarkdown document types from text, write output to <file>
render_document <- function(rmarkdown, output_format, file) {
  t <- tempfile(fileext = '.Rmd')
  cat(rmarkdown, file = t)
  ext <- regmatches(file, regexpr("\\.[^\\.]*", file))
  on.exit(unlink(sub(paste0(ext, '$'), '*', t)), add = TRUE)
  
  # render
  t <- render(
    input = t,
    output_format = output_format,
    output_dir = 'www',
    envir = new.env()
  )
  file.copy(t, file)
}

## Shiny UI
ui <- navbarPage(
  theme = shinytheme('cosmo'),
  id="navbar",
  title="shiny-rmarkdown",
  windowTitle = "shiny-rmarkdown",
    tabPanel("Project",
      fluidRow(
        column(5,
          rmarkdownInput("rmarkdown", "Rmarkdown", default, rows=40)
        ),
        column(7,
          fluidRow(column(8, 
            downloadButton('pdf', 'pdf'),
            downloadButton('docx', 'docx'),
            downloadButton('html', 'html')
          ),
          column(4, 
            div(align="right",
                 checkboxInput("update", "Update continuously", value = TRUE)
            )
          )),
          hr(),
          uiOutput('rmarkdown')
        )
      )
    ),
    tabPanel("Guide",
      uiOutput("guide")
  )
)

## Shiny Server
server <- function(input, output, session) {
  # Reactives
  rmarkdown <- reactive({input$rmarkdown})
  doUpdate <- reactiveVal(0)
  observe({
    rmarkdown()
    if (isolate(!input$update)) return()
    isolate(doUpdate(doUpdate() + 1))
  })
  document <- eventReactive(doUpdate(), render_html_fragment(rmarkdown()))
  
  output$rmarkdown = renderUI({document()})
  output$pdf <- downloadHandler(
    filename = function() {"results.pdf"},
    content = function(file) render_document(rmarkdown(), pdf_document(), file),
    contentType = 'application/pdf'
  )
  output$docx <- downloadHandler(
    filename = function() {"results.docx"},
    content = function(file) render_document(rmarkdown(), word_document(), file),
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  output$html <- downloadHandler(
    filename = function() {"results.html"},
    content = function(file) render_document(rmarkdown(), html_document(), file),
    contentType = 'text/html'
  )
  output$guide <- renderUI(
    render_html_fragment(readChar("README.md", file.info("README.md")$size))
  )
  lapply(
    c("rmarkdown", "guide"),
    function(x) outputOptions(output, x, suspendWhenHidden=FALSE)
  )
  session$onSessionEnded(function() stopApp(returnValue=NULL))
}

shinyApp(ui, server)
