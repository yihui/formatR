library(shiny)
library(formatR)

shinyServer(function(input, output, session) {
  observe({
    res = try(tidy.source(
      text = input$src, output = FALSE, keep.comment = input$arg_comment,
      keep.blank.line = input$arg_blank, replace.assign = input$arg_assign,
      left.brace.newline = input$arg_brace, reindent.spaces = input$arg_indent,
      width.cutoff = input$arg_width
    ))
    session$sendCustomMessage(
      'replace_textarea',
      if (inherits(res, 'try-error')) I(res) else paste(res$text.tidy, collapse = '\n')
    )
  })
})
