#' Reformat R code while preserving blank lines and comments
#'
#' This function returns reformatted source code; it tries to preserve blank
#' lines and comments, which is different with \code{\link{parse}} and
#' \code{\link{deparse}}. It can also replace \code{=} with \code{<-} where
#' \code{=} means assignments, and reindent code by a specified number of spaces
#' (default is 4).
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
#' @param output output to the console or a file using \code{\link{cat}}?
#' @param text an alternative way to specify the input: if it is \code{NULL},
#'   the function will read the source code from the \code{source} argument;
#'   alternatively, if \code{text} is a character vector containing the source
#'   code, it will be used as the input and the \code{source} argument will be
#'   ignored
#' @param width.cutoff passed to \code{\link{deparse}}: integer in [20, 500]
#'   determining the cutoff at which line-breaking is tried (default to be
#'   \code{getOption("width")})
#' @param ... other arguments passed to \code{\link{cat}}, e.g. \code{file}
#'   (this can be useful for batch-processing R scripts, e.g.
#'   \code{tidy_source(source = 'input.R', file = 'output.R')})
#' @return A list with components \item{text.tidy}{the reformatted code as a
#'   character vector} \item{text.mask}{the code containing comments, which are
#'   masked in assignments or with the weird operator}
#' @note Be sure to read the reference to know other limitations.
#' @author Yihui Xie <\url{https://yihui.name}> with substantial contribution
#'   from Yixuan Qiu <\url{http://yixuan.cos.name}>
#' @seealso \code{\link{parse}}, \code{\link{deparse}}
#' @references \url{https://yihui.name/formatR} (an introduction to this
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
  output = TRUE, text = NULL,
  width.cutoff = getOption('width'), ...
) {
  if (is.null(text)) {
    if (source == 'clipboard' && Sys.info()['sysname'] == 'Darwin') {
      source = pipe('pbpaste')
    }
    text = readLines(source, warn = FALSE)
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
  if (comment) text = mask_comments(text, width.cutoff, blank, wrap)
  text.mask = tidy_block(text, width.cutoff, arrow && length(grep('=', text)))
  text.tidy = if (comment) unmask_source(text.mask) else text.mask
  text.tidy = reindent_lines(text.tidy, indent)
  if (brace.newline) text.tidy = move_leftbrace(text.tidy)
  # restore new lines in the beginning and end
  if (blank) text.tidy = c(rep('', n1), text.tidy, rep('', n2))
  if (output) cat(text.tidy, sep = '\n', ...)
  invisible(list(
    text.tidy = restore_encoding(text.tidy, enc),
    text.mask = restore_encoding(text.mask, enc)
  ))
}

## if you have variable names like this in your code, then you really beat me...
begin.comment = '.BeGiN_TiDy_IdEnTiFiEr_HaHaHa'
end.comment = '.HaHaHa_EnD_TiDy_IdEnTiFiEr'
pat.comment = sprintf('invisible\\("\\%s|\\%s"\\)', begin.comment, end.comment)
mat.comment = sprintf('invisible\\("\\%s([^"]*)\\%s"\\)', begin.comment, end.comment)
inline.comment = ' %InLiNe_IdEnTiFiEr%[ ]*"([ ]*#[^"]*)"'
blank.comment = sprintf('invisible("%s%s")', begin.comment, end.comment)

# wrapper around parse() and deparse()
tidy_block = function(text, width = getOption('width'), arrow = FALSE) {
  exprs = parse_only(text)
  if (length(exprs) == 0) return(character(0))
  exprs = if (arrow) replace_assignment(exprs) else as.list(exprs)
  sapply(exprs, function(e) paste(base::deparse(e, width), collapse = '\n'))
}

# Restore the real source code from the masked text
unmask_source = function(text.mask) {
  if (length(text.mask) == 0) return(text.mask)
  m = .env$line_break
  if (!is.null(m)) text.mask = gsub(m, '\n', text.mask)
  ## if the comments were separated into the next line, then remove '\n' after
  ##   the identifier first to move the comments back to the same line
  text.mask = gsub('%InLiNe_IdEnTiFiEr%[ ]*\n', '%InLiNe_IdEnTiFiEr%', text.mask)
  ## move 'else ...' back to the last line
  text.mask = gsub('\n\\s*else(\\s+|$)', ' else\\1', text.mask)
  if (any(grepl('\\\\\\\\', text.mask)) &&
      (any(grepl(mat.comment, text.mask)) || any(grepl(inline.comment, text.mask)))) {
    m = gregexpr(mat.comment, text.mask)
    regmatches(text.mask, m) = lapply(regmatches(text.mask, m), restore_bs)
    m = gregexpr(inline.comment, text.mask)
    regmatches(text.mask, m) = lapply(regmatches(text.mask, m), restore_bs)
  }
  text.tidy = gsub(pat.comment, '', text.mask)
  # inline comments should be terminated by $ or \n
  text.tidy = gsub(paste(inline.comment, '(\n|$)', sep = ''), '  \\1\\2', text.tidy)
  # the rest of inline comments should be appended by \n
  gsub(inline.comment, '  \\1\n', text.tidy)
}


#' Format all R scripts under a directory, or specified R scripts
#'
#' \code{tidy_dir()} first looks for all the R scripts under a directory (using
#' the pattern \code{"[.][RrSsQq]$"}), then uses \code{\link{tidy_source}} to
#' tidy these scripts. The original scripts will be overwritten with reformatted
#' code if reformatting was successful. You may need to back up the original
#' directory first if you do not fully understand the tricks used by
#' \code{\link{tidy_source}}. \code{tidy_file()} formats specified R scripts.
#' @param path the directory
#' @param recursive whether to recursively look for R scripts under \code{path}
#' @param ... other arguments to be passed to \code{\link{tidy_source}}
#' @param file a vector of filenames
#' @return Invisible \code{NULL}.
#' @author Yihui Xie (\code{tidy_dir}) and Ed Lee (\code{tidy_file})
#' @seealso \code{\link{tidy_source}}
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
