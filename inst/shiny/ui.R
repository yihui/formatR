library(shiny)

shinyUI(fluidPage(
  title = 'Tidy R Code with formatR (Yihui Xie)',
  helpText(),  # just a placeholder for a little bit top margin
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$script(src = 'shiny-handler.js'),
        tags$style(type = 'text/css', '.popover {max-width: 100%;}')
      ),
      helpText('This Shiny app uses the function', code('tidy_source()'),
               'in the', a(href = 'http://yihui.name/formatR', strong('formatR')),
               sprintf('(>= v%s)', packageVersion('formatR')),
               'package to reformat R code in the text box on the right.',
               a(list(icon('hand-o-right'), 'demo'), class = 'btn btn-small btn-info',
                 onclick = '$("textarea#src").val($("#demo").val()).trigger("change");')),
      checkboxInput('arg_comment', 'Preserve comments', TRUE),
      checkboxInput('arg_blank', 'Preserve blank lines', TRUE),
      checkboxInput('arg_assign', 'Replace = with <-', FALSE),
      checkboxInput('arg_brace', 'Put { on a new line', FALSE),
      numericInput ('arg_indent', 'Number of spaces for indentation', 4, min = 0),
      numericInput ('arg_width', 'Minimum line width', 70, min = 20, max = 500),
      submitButton ('Tidy My Code', icon('toggle-right'))
    ),
    mainPanel(
      tags$textarea(
        id = 'src', rows = 20,
        style = 'width: 99%; font-family: monospace; word-wrap: normal; white-space: pre;',
        placeholder = 'paste your R code here...'
      ),
      tags$textarea(
        id = 'demo', style = 'display: none;',
        paste(
          readLines(system.file('format', 'messy.R', package = 'formatR')),
          collapse = '\n'
        )
      )
    )
  )
))
