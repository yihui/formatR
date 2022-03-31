library(shiny)

shinyServer(function(input, output, session) {
  observe({
    res = try(formatR::tidy_source(
      text = input$src, output = FALSE, comment = input$arg_comment,
      blank = input$arg_blank, arrow = input$arg_assign, pipe = input$arg_pipe,
      brace.newline = input$arg_brace, indent = input$arg_indent,
      args.newline = input$arg_anl, wrap = input$arg_wrap,
      width.cutoff = if (input$width_type == 'minimum') input$arg_width else I(input$arg_width)
    ))
    session$sendCustomMessage(
      'replace_textarea',
      if (inherits(res, 'try-error')) I(res) else paste(res$text.tidy, collapse = '\n')
    )
  })
})
