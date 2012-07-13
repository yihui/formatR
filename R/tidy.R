#' Reformat R code while preserving blank lines and comments
#'
#' This function returns reformatted source code; it tries to preserve blank
#' lines and comments, which is different with \code{\link[base]{parse}}. It can
#' also replace \code{=} with \code{<-} where \code{=} means assignments, and
#' reindent code by a specified number of spaces (default is 4).
#'
#' This function helps the users to tidy up their source code in a sense that
#' necessary indents and spaces will be added, but comments will be preserved if
#' \code{keep.comment = TRUE}. See the references to know how this function
#' actually works.
#' @param source a character string: location of the source code (default to be
#'   the clipboard; this means we can copy the code to clipboard and use
#'   \code{tidy.souce()} without specifying the argument \code{source})
#' @param keep.comment whether to keep comments (\code{TRUE} by default)
#' @param keep.blank.line whether to keep blank lines (\code{TRUE} by default)
#' @param keep.space whether to preserve the leading spaces in the single lines
#'   of comments (default \code{FALSE})
#' @param replace.assign whether to replace the assign operator \code{=} with
#'   \code{<-}
#' @param left.brace.newline whether to put the left brace \code{\{} to a new
#'   line (default \code{FALSE})
#' @param reindent.spaces number of spaces to indent the code (default 4)
#' @param output output to the console or a file using \code{\link[base]{cat}}?
#' @param text an alternative way to specify the input: if it is \code{NULL},
#'   the function will read the source code from the \code{source} argument;
#'   alternatively, if \code{text} is a character vector containing the source
#'   code, it will be used as the input and the \code{source} argument will be
#'   ignored
#' @param width.cutoff passed to \code{\link[base]{deparse}}: integer in [20,
#'   500] determining the cutoff at which line-breaking is tried (default to be
#'   \code{getOption("width")})
#' @param ... other arguments passed to \code{\link[base]{cat}}, e.g.
#'   \code{file} (this can be useful for batch-processing R scripts, e.g.
#'   \code{tidy.source(source = 'input.R', file = 'output.R')})
#' @return A list with components \item{text.tidy}{the reformatted code as a
#'   character vector} \item{text.mask}{the code containing comments, which are
#'   masked in assignments or with the weird operator}
#' @note Be sure to read the reference to know other limitations.
#' @author Yihui Xie <\url{http://yihui.name}> with substantial contribution
#'   from Yixuan Qiu <\url{http://yixuan.cos.name}>
#' @seealso \code{\link[base]{parse}}, \code{\link[base]{deparse}},
#'   \code{\link[base]{cat}}
#' @references \url{https://github.com/yihui/formatR/wiki/} (an introduction to
#'   this package, with examples and further notes)
#'
#'   The package vignette also contains some examples (see
#'   \code{vignette('formatR', package = 'formatR')}.
#'
#'   Hadley's style guide: \url{https://github.com/hadley/devtools/wiki/Style}
#' @keywords IO
#' @export
#' @example inst/examples/tidy.source.R
tidy.source = function(source = "clipboard", keep.comment = getOption('keep.comment', TRUE),
                       keep.blank.line = getOption('keep.blank.line', TRUE),
                       keep.space = getOption('keep.space', FALSE),
                       replace.assign = getOption('replace.assign', FALSE),
                       left.brace.newline = getOption('left.brace.newline', FALSE),
                       reindent.spaces = getOption('reindent.spaces', 4),
                       output = TRUE, text = NULL,
                       width.cutoff = getOption("width"), ...) {
  if (is.null(text)) {
    if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
      source = pipe("pbpaste")
    }
    text = readLines(source, warn = FALSE)
  }
  if (length(text) == 0L || all(grepl('^\\s*$', text))) {
    if (output) cat('\n', ...)
    return(list(text.tidy = '', text.mask = ''))
  }
  text.lines = text
  if (keep.comment) {
    if (!keep.space) text.lines = gsub("^[[:space:]]+|[[:space:]]+$", "", text.lines)
    head.comment = grepl('^[[:space:]]*#', text.lines)
    if (any(head.comment)) {
      text.lines[head.comment] = gsub('"', "'", text.lines[head.comment])
    }
    ## wrap long comments if you do not want to preserve leading spaces
    if (!keep.space) {
      head.comment = head.comment & !grepl("^\\s*#+'", text.lines)
      text.lines = reflow_comments(text.lines, head.comment, width.cutoff)
      head.comment = grepl('^[[:space:]]*#', text.lines)
    }
    text.lines[head.comment] =
      sprintf('invisible("%s%s%s")', begin.comment, text.lines[head.comment], end.comment)
    blank.line = grepl('^[[:space:]]*$', text.lines)
    if (any(blank.line) && keep.blank.line) {
      ## no blank lines before an 'else' statement!
      else.line = grep('^[[:space:]]*else(\\W|)', text.lines)
      for (i in else.line) {
        j = i - 1
        while (blank.line[j]) {
          blank.line[j] = FALSE; j = j - 1  # search backwards & rm blank lines
          warning('removed blank line ', j,
                  ' (you should not put an \'else\' in a separate line!)')
        }
      }
      text.lines[blank.line] = sprintf('invisible("%s%s")', begin.comment, end.comment)
    }
    text.lines = mask_inline(text.lines)
  }
  text.mask = tidy_block(text.lines, width.cutoff, replace.assign)
  text.tidy = if (keep.comment) unmask.source(text.mask) else text.mask
  text.tidy = reindent_lines(text.tidy, reindent.spaces)
  if (left.brace.newline) text.tidy = move_leftbrace(text.tidy, reindent.spaces)
  if (output) cat(paste(text.tidy, collapse = "\n"), "\n", ...)
  invisible(list(text.tidy = text.tidy, text.mask = text.mask))
}

## if you have variable names like this in your code, then you really beat me...
begin.comment = ".BeGiN_TiDy_IdEnTiFiEr_HaHaHa"
end.comment = ".HaHaHa_EnD_TiDy_IdEnTiFiEr"
pat.comment = paste('invisible\\("\\', begin.comment, '|\\', end.comment, '"\\)', sep = '')

# wrapper around parse() and deparse()
tidy_block = function(text, width = getOption('width'), arrow = FALSE) {
  exprs = base::parse(text = text, srcfile = NULL)
  exprs = if (arrow) replace_assignment(exprs) else as.list(exprs)
  sapply(exprs, function(e) paste(base::deparse(e, width), collapse = '\n'))
}

#' Restore the real source code from the masked text
#'
#' Remove the masks from the code to restore the real code.
#' @param text.mask the masked source code
#' @return the real source code (a character vector)
#' @author Yihui Xie <\url{http://yihui.name}>
#' @export
#' @keywords internal
#' @examples
#' library(formatR)
#'
#' src = c("    # a single line of comments is preserved",
#' '1+1', '  ', 'if(TRUE){',
#' "x=1  # inline comments!", '}else{',
#' "x=2;print('Oh no... ask the right bracket to go away!')}",
#' "2+2+2    # 'short comments'",
#' "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments",
#' "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")
#'
#' x = tidy.source(text = src, output = FALSE)$text.mask
#'
#' cat(x, sep = '\n')
#'
#' cat(unmask.source(x), sep = '\n')
unmask.source = function(text.mask) {
  ## if the comments were separated into the next line, then remove '\n' after
  ##   the identifier first to move the comments back to the same line
  text.mask = gsub("%InLiNe_IdEnTiFiEr%[ ]*\n", "%InLiNe_IdEnTiFiEr%", text.mask)
  ## move 'else ...' back to the last line
  text.mask = gsub('\n[[:space:]]*else', ' else', text.mask)
  text.tidy = gsub(pat.comment, '', text.mask)
  gsub(' %InLiNe_IdEnTiFiEr%[ ]*"([ ]*#[^"]*)"', "  \\1", text.tidy)
}


#' A weird operator for internal use only
#'
#' This operator is almost meaningless; it is used to mask the inline comments.
#' @rdname InLiNe_IdEnTiFiEr
#' @usage x %InLiNe_IdEnTiFiEr% y
#' @param x the argument before the operator
#' @param y the argument after the operator
#' @return \code{x} (i.e. this operator always returns the object on the
#'   left-hand-side)
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link{tidy.source}}
#' @export "%InLiNe_IdEnTiFiEr%"
#' @keywords internal
#' @examples
#' ## we can use it with *anything*
#'
#' 1 %InLiNe_IdEnTiFiEr% 2
#'
#' 1:10 %InLiNe_IdEnTiFiEr% "asdf"
#'
#' lm %InLiNe_IdEnTiFiEr% 'garbage'
#'
"%InLiNe_IdEnTiFiEr%" <- function(x, y) x


#' Modified versions of parse() and deparse()
#'
#' These two functions parse and deparse the masked source code.
#'
#' For \code{\link{parse.tidy}}, the source code is masked to preserve comments,
#' then this function uses \code{\link[base]{parse}} to return the parsed but
#' unevaluated expressions in a list.
#'
#' For \code{\link{deparse.tidy}}, it uses \code{\link[base]{deparse}} to turn
#' the unevaluated (and masked) expressions into character strings; the masks
#' will be removed to restore the real source code. See
#' \code{\link{unmask.source}}.
#' @param text the source code as a character string to be passed to
#'   \code{\link{tidy.source}}
#' @param ... for \code{\link{parse.tidy}}: other arguments to be passed to
#'   \code{\link{tidy.source}}; for \code{\link{deparse.tidy}}: arguments to be
#'   passed to \code{\link[base]{deparse}}
#' @return \code{\link{parse.tidy}} returns the unevaluated expressions;
#'   \code{\link{deparse.tidy}} returns the character strings
#' @author Yihui Xie <\url{http://yihui.name}>
#' @note These functions are mainly designed for the package \pkg{pgfSweave};
#'   they may not be useful to general users.
#' @seealso \code{\link[base]{parse}}, \code{\link[base]{deparse}},
#'   \code{\link{tidy.source}}
#' @export
#' @keywords internal
#' @examples
#' src = c("    # a single line of comments is preserved",
#' '1+1', '  ', 'if(TRUE){',
#' "x=1  # comments begin with at least 2 spaces!", '}else{',
#' "x=2;print('Oh no... ask the right bracket to go away!')}",
#' "1*3 # this comment will be dropped!",
#' "2+2+2    # 'short comments'",
#' "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments",
#' "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")
#'
#' (expr = parse.tidy(src))
#'
#' parse.tidy(src, keep.blank.line = TRUE)
#'
#' cat(deparse.tidy(expr))
#'
#' deparse.tidy(expr, width.cutoff = 50)
parse.tidy = function(text, ...) {
  tidy.res = tidy.source(text = text, output = FALSE, ...)
  base::parse(text = tidy.res$text.mask)
}
#' @param expr the unevaluated expressions (ideally as results from
#' \code{\link{parse.tidy}})
#' @rdname parse.tidy
#' @export
#' @keywords internal
deparse.tidy = function(expr, ...) {
  unmask.source(paste(base::deparse(expr, ...), collapse = '\n'))
}

#' Format the R scripts under a directory
#'
#' This function first looks for all the R scripts under a directory (using the
#' pattern \code{"\\\\.[RrSsQq]$"}), then uses \code{\link{tidy.source}} to tidy
#' these scripts. The original scripts will be overwritten with reformatted code
#' if reformatting was successful. You may need to back up the original
#' directory first if you do not fully understand the tricks
#' \code{\link{tidy.source}} is using.
#' @param path the directory
#' @param recursive whether to recursively look for R scripts under \code{path}
#' @param ... other arguments to be passed to \code{\link{tidy.source}}
#' @return NULL
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link{tidy.source}}
#' @export
#' @examples
#' library(formatR)
#'
#' path = tempdir()
#' file.copy(system.file('demo', package = 'base'), path, recursive=TRUE)
#' tidy.dir(path, recursive=TRUE)
tidy.dir = function(path = '.', recursive = FALSE, ...) {
  flist = list.files(path, pattern = '\\.[RrSsQq]$', full.names = TRUE, recursive = recursive)
  for (f in flist) {
    message('tidying ', f)
    try(tidy.source(f, file = f, ...))
  }
}
