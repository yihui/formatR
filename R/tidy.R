#' Reformat R code while preserving blank lines and comments
#'
#' This function returns reformatted source code; it tries to preserve blank
#' lines and comments, which is different with \code{\link{parse}()} and
#' \code{\link{deparse}()}. It can also replace \code{=} with \code{<-} where
#' \code{=} means assignments, and reindent code by a specified number of spaces
#' (default is 4).
#'
#' If the value of the argument \code{width.cutoff} is wrapped in
#' \code{\link{I}()} (e.g., \code{I(60)}), it will be treated as the \emph{upper
#' bound} on the line width. The corresponding argument to \code{deparse()} is
#' actually a lower bound, and so the function will perform a binary search for
#' a width value that can make \code{deparse()} return code with line width
#' smaller than or equal to the \code{width.cutoff} value. If the search fails
#' to find such a value, it will emit a warning, which can be suppressed by the
#' global option \code{options(formatR.width.warning = FALSE)}.
#' @param source a character string: location of the source code (default to be
#'   the clipboard; this means we can copy the code to clipboard and use
#'   \code{tidy_source()} without specifying the argument \code{source})
#' @param comment whether to keep comments (\code{TRUE} by default)
#' @param blank whether to keep blank lines (\code{TRUE} by default)
#' @param arrow whether to replace the assign operator \code{=} with \code{<-}
#' @param brace.newline whether to put the left brace \code{\{} to a new line
#'   (default \code{FALSE})
#' @param indent number of spaces to indent the code (default 4)
#' @param wrap whether to wrap comments to the linewidth determined by
#'   \code{width.cutoff} (note that roxygen comments will never be wrapped)
#' @param width.cutoff Passed to \code{\link{deparse}()}: an integer in
#'   \code{[20, 500]} determining the cutoff at which line-breaking is tried
#'   (default to be \code{getOption("width")}). In other words, this is the
#'   \emph{lower bound} of the line width. See \sQuote{Details} if an upper
#'   bound is desired instead.
#' @param output output to the console or a file using \code{\link{cat}()}?
#' @param text an alternative way to specify the input: if it is \code{NULL},
#'   the function will read the source code from the \code{source} argument;
#'   alternatively, if \code{text} is a character vector containing the source
#'   code, it will be used as the input and the \code{source} argument will be
#'   ignored
#' @param ... other arguments passed to \code{\link{cat}()}, e.g. \code{file}
#'   (this can be useful for batch-processing R scripts, e.g.
#'   \code{tidy_source(source = 'input.R', file = 'output.R')})
#' @return A list with components \item{text.tidy}{the reformatted code as a
#'   character vector} \item{text.mask}{the code containing comments, which are
#'   masked in assignments or with the weird operator}
#' @note Be sure to read the reference to know other limitations.
#' @author Yihui Xie <\url{https://yihui.org}> with substantial contribution
#'   from Yixuan Qiu <\url{https://yixuan.blog}>
#' @seealso \code{\link{parse}()}, \code{\link{deparse}()}
#' @references \url{https://yihui.org/formatR/} (an introduction to this
#'   package, with examples and further notes)
#' @import utils
#' @export
#' @example inst/examples/tidy.source.R
tidy_source = function(
  source = 'clipboard', comment = getOption('formatR.comment', TRUE),
  blank = getOption('formatR.blank', TRUE),
  arrow = getOption('formatR.arrow', FALSE),
  brace.newline = getOption('formatR.brace.newline', FALSE),
  indent = getOption('formatR.indent', 4),
  wrap = getOption('formatR.wrap', TRUE),
  width.cutoff = getOption('formatR.width', getOption('width')),
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
    one = paste(text, collapse = '\n') # record how many line breaks before/after
    n1 = attr(regexpr('^\n*', one), 'match.length')
    n2 = attr(regexpr('\n*$', one), 'match.length')
  }
  on.exit(.env$line_break <- NULL, add = TRUE)
  if (width.cutoff > 500) width.cutoff[1] = 500
  if (width.cutoff < 20) width.cutoff[1] = 20
  # insert enough spaces into infix operators such as %>% so the lines can be
  # broken after the operators
  spaces = paste(rep(' ', width.cutoff), collapse = '')
  if (comment) text = mask_comments(text, width.cutoff, blank, wrap, spaces)
  text.mask = tidy_block(
    text, width.cutoff, arrow && length(grep('=', text)), indent, brace.newline
  )
  text.tidy = if (comment) unmask_source(text.mask, spaces) else text.mask
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
deparse2 = function(expr, width, warn = getOption('formatR.width.warning', TRUE)) {
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
    x = gsub('\\s+$', '', x)
    d[[i]] <<- x
    x2 = grep(pat.comment, x, invert = TRUE, value = TRUE)  # don't check comments
    x2 = gsub(pat.infix, '\\1\\2\\3', x2)  # remove extra spaces in %>% operators
    p[[i]] <<- x2[nchar(x2, type = 'width') > width]
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
    width, ' for the line(s) of code:\n', paste0('  ', p[[i]], collapse = '\n'),
    call. = FALSE
  )
  d[[i]]
}

# wrapper around parse() and deparse()
tidy_block = function(
  text, width = getOption('width'), arrow = FALSE, indent = 4, brace.newline = FALSE
) {
  exprs = parse_only(text)
  if (length(exprs) == 0) return(character(0))
  exprs = if (arrow) replace_assignment(exprs) else as.list(exprs)
  deparse = if (inherits(width, 'AsIs')) deparse2 else base::deparse
  unlist(lapply(exprs, function(e) {
    x = deparse(e, width)
    x = reindent_lines(x, indent)
    if (brace.newline) x = move_leftbrace(x)
    # remove white spaces on blank lines
    x = gsub(blank.comment2, '', x)
    paste(x, collapse = '\n')
  }))
}

# Restore the real source code from the masked text
unmask_source = function(text.mask, spaces) {
  if (length(text.mask) == 0) return(text.mask)
  m = .env$line_break
  if (!is.null(m)) text.mask = gsub(m, '\n', text.mask)
  # if the comments were separated into the next line, then remove '\n' after
  # the identifier first to move the comments back to the same line
  text.mask = gsub('(%\b%)[ ]*\n', '\\1', text.mask)
  # move 'else ...' back to the last line
  text.mask = gsub('\n\\s*else(\\s+|$)', ' else\\1', text.mask)
  if (any(grepl('\\\\\\\\', text.mask)) &&
      (any(grepl(mat.comment, text.mask)) || any(grepl(inline.comment, text.mask)))) {
    m = gregexpr(mat.comment, text.mask)
    regmatches(text.mask, m) = lapply(regmatches(text.mask, m), restore_bs)
    m = gregexpr(inline.comment, text.mask)
    regmatches(text.mask, m) = lapply(regmatches(text.mask, m), restore_bs)
  }
  text.tidy = gsub(pat.comment, '', text.mask)
  # restore infix operators such as %>%
  text.tidy = gsub(paste0('(%)(', infix_ops, ')', spaces, '(%)\\s*(\n)'), '\\1\\2\\3\\4', text.tidy)
  # inline comments should be terminated by $ or \n
  text.tidy = gsub(paste(inline.comment, '(\n|$)', sep = ''), '  \\1\\2', text.tidy)
  # the rest of inline comments should be appended by \n
  gsub(inline.comment, '  \\1\n', text.tidy)
}


#' Format all R scripts under a directory, or specified R scripts
#'
#' \code{tidy_dir()} first looks for all the R scripts under a directory (using
#' the pattern \code{"[.][RrSsQq]$"}), then uses \code{\link{tidy_source}()} to
#' tidy these scripts. The original scripts will be overwritten with reformatted
#' code if reformatting was successful. You may need to back up the original
#' directory first if you do not fully understand the tricks used by
#' \code{\link{tidy_source}()}. \code{tidy_file()} formats specified R scripts.
#' @param path the directory
#' @param recursive whether to recursively look for R scripts under \code{path}
#' @param ... other arguments to be passed to \code{\link{tidy_source}()}
#' @param file a vector of filenames
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
