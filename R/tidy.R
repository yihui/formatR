#' Reformat R code
#'
#' Read R code from a file or the clipboard and reformat it. This function is
#' based on \code{\link{parse}()} and \code{\link{deparse}()}, but it does
#' several other things, such as preserving blank lines and comments,
#' substituting the assignment operator \code{=} with \code{<-}, and
#' re-indenting code with a specified number of spaces.
#'
#' A value of the argument \code{width.cutoff} wrapped in \code{\link{I}()}
#' (e.g., \code{I(60)}) will be treated as the \emph{upper bound} of the line
#' width. The corresponding argument to \code{deparse()} is a lower bound, so
#' the function will perform a binary search for a width value that can make
#' \code{deparse()} return code with line width smaller than or equal to the
#' \code{width.cutoff} value. If the search fails, a warning will signal,
#' suppressible by global option \code{options(formatR.width.warning = FALSE)}.
#' @param source A character string: file path to the source code (defaults to
#'   the clipboard).
#' @param comment Whether to keep comments.
#' @param blank Whether to keep blank lines.
#' @param arrow Whether to substitute the assignment operator \code{=} with
#'   \code{<-}.
#' @param pipe Whether to substitute the \pkg{magrittr} pipe \code{\%>\%} with
#'   R's native pipe operator \code{|>}.
#' @param brace.newline Whether to put the left brace \code{\{} to a new line.
#' @param indent Number of spaces to indent the code.
#' @param wrap Whether to wrap comments to the linewidth determined by
#'   \code{width.cutoff} (roxygen comments will never be wrapped).
#' @param width.cutoff An integer in \code{[20, 500]}: if a line's character
#'   length is at or over this number, the function will try to break it into a
#'   new line. In other words, this is the \emph{lower bound} of the line width.
#'   See \sQuote{Details} if an upper bound is desired instead.
#' @param args.newline Whether to start the arguments of a function call on a
#'   new line instead of after the function name and \code{(} when the arguments
#'   cannot fit one line.
#' @param output Whether to output to the console or a file using
#'   \code{\link{cat}()}.
#' @param text An alternative way to specify the input: if \code{NULL}, the
#'   function will use the \code{source} argument; if a character vector
#'   containing the source code, the function will use this and ignore the
#'   \code{source} argument.
#' @param ... Other arguments passed to \code{\link{cat}()}, e.g. \code{file}
#'   (this can be useful for batch-processing R scripts, e.g.
#'   \code{tidy_source(source = 'input.R', file = 'output.R')}).
#' @return A list with components \item{text.tidy}{the reformatted code as a
#'   character vector} \item{text.mask}{the code containing comments, which are
#'   masked in assignments or with the weird operator}.
#' @note Be sure to read the reference to know other limitations.
#' @author Yihui Xie <\url{https://yihui.org}> with substantial contribution
#'   from Yixuan Qiu <\url{https://yixuan.blog}>
#' @seealso \code{\link{parse}()}, \code{\link{deparse}()}
#' @references \url{https://yihui.org/formatR/} (an introduction to this
#'   package, with examples and further notes)
#' @import stats utils
#' @export
#' @example inst/examples/tidy.source.R
tidy_source = function(
  source = 'clipboard', comment = getOption('formatR.comment', TRUE),
  blank = getOption('formatR.blank', TRUE),
  arrow = getOption('formatR.arrow', FALSE),
  pipe = getOption('formatR.pipe', FALSE),
  brace.newline = getOption('formatR.brace.newline', FALSE),
  indent = getOption('formatR.indent', 4),
  wrap = getOption('formatR.wrap', TRUE),
  width.cutoff = getOption('formatR.width', getOption('width')),
  args.newline = getOption('formatR.args.newline', FALSE),
  output = TRUE, text = NULL, ...
) {
  if (is.null(text)) {
    if (source == 'clipboard' && Sys.info()['sysname'] == 'Darwin') {
      source = pipe('pbpaste'); on.exit(close(source), add = TRUE)
      # use readChar() instead of readLines() in case users didn't copy the last
      # \n into clipboard, e.g., https://github.com/yihui/formatR/issues/54
      text = readChar(source, getOption('formatR.clipboard.size', 1e5))
      text = unlist(strsplit(text, '\n'))
    } else {
      text = readLines(source, warn = FALSE)
    }
  }
  enc = special_encoding(text)
  if (length(text) == 0L || all(grepl('^\\s*$', text))) {
    if (output) cat('\n', ...)
    return(list(text.tidy = text, text.mask = text))
  }
  if (blank) {
    one = one_string(text) # record how many line breaks before/after
    n1 = attr(regexpr('^\n*', one), 'match.length')
    n2 = attr(regexpr('\n*$', one), 'match.length')
  }
  on.exit(.env$line_break <- NULL, add = TRUE)
  if (width.cutoff > 500) width.cutoff[1] = 500
  if (width.cutoff < 20) width.cutoff[1] = 20
  # insert enough spaces into infix operators such as %>% so the lines can be
  # broken after the operators
  spaces = rep_chars(width.cutoff)
  text = mask_comments(text, comment, blank, wrap, arrow, pipe, args.newline, spaces)
  text.mask = tidy_block(
    text, width.cutoff, rep_chars(indent), brace.newline, wrap, args.newline, spaces
  )
  text.tidy = if (comment) unmask_source(text.mask) else text.mask
  # restore new lines in the beginning and end
  if (blank) text.tidy = c(rep('', n1), text.tidy, rep('', n2))
  if (output) cat(text.tidy, sep = '\n', ...)
  invisible(list(
    text.tidy = restore_encoding(text.tidy, enc),
    text.mask = restore_encoding(text.mask, enc)
  ))
}

# some tokens that should be rare to appear in code from real world, mainly to
# protect comments and blank lines
begin.comment = '.BeGiN_TiDy_IdEnTiFiEr_HaHaHa'
end.comment = '.HaHaHa_EnD_TiDy_IdEnTiFiEr'
pat.comment = sprintf('invisible\\("\\%s|\\%s"\\)', begin.comment, end.comment)
mat.comment = sprintf('invisible\\("\\%s([^"]*)\\%s"\\)', begin.comment, end.comment)
inline.comment = ' %\b%[ ]*"([ ]*#[^"]*)"'
blank.comment = sprintf('invisible("%s%s")', begin.comment, end.comment)
blank.comment2 = paste0('^\\s*', gsub('\\(', '\\\\(', blank.comment), '\\s*$')

# first, perform a (semi-)binary search to find the greatest cutoff width such
# that the width of the longest line <= `width`; if the search fails, use
# brute-force to try all possible widths
deparse2 = function(
  expr, width, spaces = '', indent = '    ',
  warn = getOption('formatR.width.warning', TRUE)
) {
  wmin = 20  # if deparse() can't manage it with width.cutoff <= 20, issue a warning
  wmax = min(500, width + 10)  # +10 because a larger width may result in smaller actual width

  r = seq(wmin, wmax)
  k = setNames(rep(NA, length(r)), as.character(r))  # results of width checks
  d = p = list()  # deparsed results and lines exceeding desired width

  # pattern for pipe operators like %>%
  pat.infix = paste0('(%)(', infix_ops, ') {', width, '}(%)$')
  check_width = function(w) {
    i = as.character(w)
    if (!is.na(x <- k[i])) return(x)
    x = deparse(expr, w)
    x = trimws(x, 'right')
    d[[i]] <<- x
    x2 = grep(pat.comment, x, invert = TRUE, value = TRUE)  # don't check comments
    x2 = gsub(pat.infix, '\\1\\2\\3', x2)  # remove extra spaces in %>% operators
    x2 = restore_infix(x2)
    x2 = reindent_lines(x2, indent)
    x2 = restore_arg_breaks(x2, width, spaces, indent, split = TRUE)
    p[[i]] <<- x2[exceed_width(x2, width)]
    k[i] <<- length(p[[i]]) == 0
  }

  # if the desired width happens to just work, return the result
  if (check_width(w <- width)) return(d[[as.character(w)]])

  repeat {
    if (!any(is.na(k))) break  # has tried all possibilities
    if (wmin >= wmax) break
    w = ceiling((wmin + wmax)/2)
    if (check_width(w)) wmin = w else wmax = wmax - 2
  }

  # try all the rest of widths if no suitable width has been found
  if (!any(k, na.rm = TRUE)) for (i in r[is.na(k)]) check_width(i)
  r = r[which(k)]
  if ((n <- length(r)) > 0) return(d[[as.character(r[n])]])

  i = as.character(width)
  if (warn) warning(
    'Unable to find a suitable cut-off to make the line widths smaller than ',
    width, ' for the line(s) of code:\n', one_string('  ', p[[i]]),
    call. = FALSE
  )
  d[[i]]
}

# wrapper around parse() and deparse()
tidy_block = function(
  text, width = getOption('width'), indent = '    ',
  brace.newline = FALSE, wrap = TRUE, args.newline = FALSE, spaces = rep_chars(width)
) {
  exprs = parse_source(text)
  if (length(exprs) == 0) return(character(0))
  deparse = if (inherits(width, 'AsIs')) {
    function(x, width) deparse2(x, width, spaces, indent)
  } else base::deparse
  unlist(lapply(as.list(exprs), function(e) {
    x = deparse(e, width)
    x = trimws(x, 'right')
    x = reindent_lines(x, indent)
    # remove white spaces on blank lines
    x = gsub(blank.comment2, '', x)
    x = reflow_comments(x, width, wrap)
    if (brace.newline) x = move_leftbrace(x)
    x = restore_infix(x)
    x = one_string(x)
    # restore anonymous functions
    if (!brace.newline)
      x = gsub('( %\\\\\b%)\\s+(\\{)', '\\1 \\2', x)  # remove possible \n before {
    x = gsub('`\\\\\\\\`(\\(.*?\\)) %\\\\\b%', '\\\\\\1', x)
    if (args.newline) x = restore_arg_breaks(x, width, spaces, indent)
    x
  }))
}

# Restore the real source code from the masked text
unmask_source = function(x) {
  if (length(x) == 0) return(x)
  m = .env$line_break
  if (!is.null(m)) x = gsub(m, '\n', x)
  # if the comments were separated into the next line, then remove '\n' after
  # the identifier first to move the comments back to the same line
  x = gsub('(%\b%)[ ]*\n', '\\1', x)
  # move 'else ...' back to the last line
  x = gsub('\n\\s*else(\\s+|$)', ' else\\1', x)
  if (any(grepl('\\\\\\\\', x)) || any(grepl(inline.comment, x))) {
    m = gregexpr(inline.comment, x)
    regmatches(x, m) = lapply(regmatches(x, m), restore_bs)
  }
  # inline comments should be terminated by $ or \n
  x = gsub(paste(inline.comment, '(\n|$)', sep = ''), '  \\1\\2', x)
  # the rest of inline comments should be appended by \n
  gsub(inline.comment, '  \\1\n', x)
}


#' Format all R scripts under a directory, or specified R scripts
#'
#' Look for all R scripts under a directory (using the pattern
#' \code{"[.][RrSsQq]$"}), then tidy them with \code{\link{tidy_source}()}. If
#' successful, the original scripts will be overwritten with reformatted ones.
#' Please back up the original directory first if you do not fully understand
#' the tricks used by \code{\link{tidy_source}()}. \code{tidy_file()} formats
#' scripts specified by file names.
#' @param path The path to a directory containning R scripts.
#' @param recursive Whether to recursively look for R scripts under \code{path}.
#' @param ... Other arguments to be passed to \code{\link{tidy_source}()}.
#' @param file A vector of filenames.
#' @return Invisible \code{NULL}.
#' @author Yihui Xie (\code{tidy_dir}) and Ed Lee (\code{tidy_file})
#' @seealso \code{\link{tidy_source}()}
#' @export
#' @examples
#' library(formatR)
#'
#' path = tempdir()
#' file.copy(system.file('demo', package = 'base'), path, recursive=TRUE)
#' tidy_dir(path, recursive=TRUE)
tidy_dir = function(path = '.', recursive = FALSE, ...) {
  tidy_file(list.files(
    path, pattern = '[.][RrSsQq]$', full.names = TRUE, recursive = recursive
  ), ...)
}

#' @export
#' @rdname tidy_dir
tidy_file = function(file, ...) {
  for (f in file) {
    message("tidying ", f)
    try(tidy_source(f, file = f, ...))
  }
}

#' Reformat R code in RStudio IDE
#'
#' If any R code is selected in the RStudio source editor, this function
#' reformats the selected code; otherwise it reformats the current open file (if
#' it is unsaved, it will be automatically saved).
#' @param ... Arguments to be passed to \code{\link{tidy_source}()}, among which
#'   the \code{indent} argument will respect the value you set for the number of
#'   spaces for indentation in RStudio.
#' @note If the output is not what you want, you can undo the change in the
#'   editor (Ctrl + Z or Command + Z).
#' @export
#' @examplesIf interactive()
#' formatR::tidy_rstudio()
#' formatR::tidy_rstudio(args.newline = TRUE)
tidy_rstudio = function(...) {
  ctx = rstudio_context()
  if (is.null(getOption('formatR.indent'))) {
    opts = options(formatR.indent = rstudioapi::readRStudioPreference('num_spaces_for_tab', 4))
    on.exit(options(opts), add = TRUE)
  }
  if (length(ctx$selection) == 1 && !identical(txt <- ctx$selection[[1]]$text, '')) {
    res = tidy_source(text = txt, output = FALSE, ...)$text.tidy
    rstudioapi::modifyRange(ctx$selection[[1]]$range, one_string(res), ctx$id)
  } else {
    rstudioapi::documentSave(ctx$id)
    res = tidy_source(ctx$path, output = FALSE, ...)$text.tidy
    writeLines(enc2utf8(res), ctx$path, useBytes = TRUE)
  }
}

rstudio_context = function() {
  ctx = rstudioapi::getSourceEditorContext()
  if (is.null(ctx)) stop('There is no open document in the RStudio source editor.')
  ctx
}

#' Substitute the \pkg{magrittr} pipe with R's native pipe operator
#'
#' Parse the R code in the RStudio editor, identify \code{\%>\%}, and substitute
#' with \code{|>}.
#' @note Currently this function only works inside the RStudio IDE, and may be
#'   extended in future to deal with arbitrary R code elsewhere.
#' @export
#' @examplesIf interactive()
#' formatR::tidy_pipe()
tidy_pipe = function() {
  ctx = rstudio_context()
  d = parse_data(ctx$contents)
  i = d$token == 'SPECIAL' & d$text == '%>%'
  if (!any(i)) return(invisible())
  d = d[i, c('line1', 'col1', 'line2', 'col2')]
  d[, 4] = d[, 4] + 1
  r = unname(as.list(as.data.frame(t(d))))
  rstudioapi::modifyRange(r, '|>', ctx$id)
}
