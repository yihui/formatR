library(shiny)
library(formatR)

shinyServer(function(input, output, session) {
  observe({
    res = try(tidy_source(
      text = input$src, output = FALSE, comment = input$arg_comment,
      blank = input$arg_blank, arrow = input$arg_assign,
      brace.newline = input$arg_brace, indent = input$arg_indent,
      width.cutoff = input$arg_width
    ))
    session$sendCustomMessage(
      'replace_textarea',
      if (inherits(res, 'try-error')) I(res) else paste(res$text.tidy, collapse = '\n')
    )
  })
})
