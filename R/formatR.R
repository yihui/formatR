##' `Tidy up' R code while preserving comments
##'
##' This function has nothing to do with code optimization; it just
##' returns parsed source code, but also tries to preserve comments,
##' which is different with \code{\link[base]{parse}}. See `Details'.
##'
##' This function helps the users to tidy up their source code in a
##' sense that necessary indents and spaces will be added, etc. See
##' \code{\link[base]{parse}}. But comments will be preserved if
##' \code{keep.comment = TRUE}.
##'
##' The method to preserve comments is to protect them as strings in
##' disguised assignments. For example, there is a single line of
##' comments in the source code:
##' \preformatted{  # asdf}
##'
##' It will be first masked as
##'
##' \preformatted{.IDENTIFIER1 <- "  # asdf.IDENTIFIER2"}
##'
##' which is a legal R expression, so \code{\link[base]{parse}} can
##' deal with it and will no longer remove the disguised comments.  In
##' the end the identifiers will be removed to restore the original
##' comments, i.e. the strings \code{'.IDENTIFIER1 <- "'} and
##' \code{'.IDENTIFIER2"'} are replaced with empty strings.
##'
##' ``Inline'' comments are handled differently: two spaces will be added
##' before the hash symbol \code{#}, e.g.
##' \preformatted{1+1#  comments}
##'
##' will become
##' \preformatted{1+1  #  comments}
##'
##' Inline comments are first disguised as a weird operation with its
##' preceding R code, which is essentially meaningless but
##' syntactically correct!  For example,
##' \preformatted{1+1 \%InLiNe_IdEnTiFiEr\% "#  comments"}
##'
##' then \code{\link[base]{parse}} will deal with this expression;
##' again, the disguised comments will not be removed. In the end,
##' inline comments will be freed as well (remove the operator
##' \code{\%InLiNe_IdEnTiFiEr\%} and surrounding double quotes).
##'
##' All these special treatments to comments are due to the fact that
##' \code{\link[base]{parse}} and \code{\link[base]{deparse}} can tidy
##' the R code at the price of dropping all the comments.
##'
##' @param source a string: location of the source code (default to be
##' the clipboard; this means we can copy the code to clipboard and
##' use \code{tidy.souce()} without specifying the argument
##' \code{source})
##' @param keep.comment logical value: whether to keep comments or
##' not? (\code{TRUE} by default)
##' @param keep.blank.line logical value: whether to keep blank lines
##' or not? (\code{FALSE} by default)
##' @param keep.space logical: whether to preserve the leading spaces
##' in the single lines of comments (default \code{FALSE})
##' @param replace.assign logical: whether to replace the assign
##' operator \code{=} with \code{<-}
##' @param output output to the console or a file using
##' \code{\link[base]{cat}}?
##' @param text an alternative way to specify the input: if it is
##' \code{NULL}, the function will read the source code from the
##' \code{source} argument; alternatively, if \code{text} is a
##' character vector containing the source code, it will be used as
##' the input and the \code{source} argument will be ignored
##' @param width.cutoff passed to \code{\link[base]{deparse}}: integer
##' in [20, 500] determining the cutoff at which line-breaking is
##' tried (default to be \code{0.75 * getOption("width")})
##' @param ... other arguments passed to \code{\link[base]{cat}},
##' e.g. \code{file} (this can be useful for batch-processing R
##' scripts, e.g. \code{tidy.source(source = 'input.R', file =
##' 'output.R')})
##' @return A list with components \item{text.tidy}{The reformatted
##' code as a character vector.} \item{text.mask}{The code containing
##' comments, which are masked in assignments or with the weird
##' operator.}  \item{begin.comment,end.comment}{ identifiers used to
##' mark the comments }
##'
##' @note When \code{keep.comment == TRUE}, \emph{all your double
##' quotes in the comments will be replaced by single quotes!!} For
##' example,
##'
##' \preformatted{1 + 1  # here is "comment"}
##'
##' will become
##' \preformatted{1 + 1  # here is 'comment'}
##'
##' There are hidden options which can control the behaviour of this
##' function: the argument \code{keep.comment} gets its value from
##' \code{options('keep.comment')} by default;  \code{keep.blank.line}
##' from \code{options('keep.blank.line')}, \code{keep.space} from
##' \code{options('keep.space')}, and \code{replace.assign} from
##' \code{options('replace.assign')}. If these options are
##' \code{NULL}, the default values will be \code{TRUE}, \code{FALSE},
##' \code{FALSE} and \code{FALSE} respectively.
##'
##' Also note that if \code{keep.space} is \code{FALSE}, single lines
##' of long comments will be wrapped into shorter ones automatically.
##' Otherwise, long comments will not be wrapped, so they may exceed
##' the page margin, and \code{\\\\t} will be replaced with
##' \code{\\t}. Roxygen comments will not be wrapped in any case.
##'
##' @section Warning: The best strategy to avoid failure is to put
##' comments in whole lines or after \emph{complete} R
##' expressions. Here are some examples which could make
##' \code{\link{tidy.source}} fail:
##'
##' \preformatted{1 + 2 +## comments after an incomplete line
##'
##'   3 + 4}
##'
##' And the comments right after the curly brace will be moved to the
##' next line, e.g.
##'
##' \preformatted{if (TRUE) {## comments
##'
##' }}
##'
##' will become
##' \preformatted{if (TRUE) {
##'
##'     ## comments
##'
##' }}
##'
##' @author Yihui Xie <\url{http://yihui.name}> with substantial
##' contribution from Yixuan Qiu <\url{http://yixuan.cos.name}>
##' @seealso \code{\link[base]{parse}}, \code{\link[base]{deparse}},
##' \code{\link[base]{cat}}
##' @references
##' \url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}
##' @keywords IO
##' @export
##' @examples
##' library(formatR)
##'
##' ## use the 'text' argument
##' src = c("    # a single line of comments is preserved",
##' '1+1', '  ', 'if(TRUE){',
##' "x=1  # inline comments", '}else{',
##' "x=2;print('Oh no... ask the right bracket to go away!')}",
##' '1*3 # one space before this comment will become two!',
##' "2+2+2    # 'short comments'", "   ",
##' "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments",
##' "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line",
##' "\t\t## tabs/spaces before comments: use keep.space=TRUE to keep them",
##' "'a character string with \t in it'",
##' "# note tabs will be converted to spaces when keep.space = TRUE",
##' paste("## here is a", paste(rep("long", 20), collapse = ' '), "comment"))
##'
##' ## source code
##' cat(src, sep = '\n')
##'
##' ## the formatted version
##' tidy.source(text = src)
##'
##' ## other options: preserve leading spaces
##' tidy.source(text = src, keep.space = TRUE)
##'
##' ## preserve blank lines
##' tidy.source(text = src, keep.blank.line = TRUE)
##'
##' ## discard comments!
##' tidy.source(text = src, keep.comment = FALSE)
##'
##' ## wanna see the gory truth??
##' tidy.source(text = src, output = FALSE)$text.mask
##'
##'
##' ## tidy up the source code of image demo
##' x = file.path(system.file(package = "graphics"), "demo", "image.R")
##'
##' # to console
##' tidy.source(x)
##'
##' # to a file
##' f = tempfile()
##' tidy.source(x, keep.blank.line = TRUE, file = f)
##'
##' ## check the original code here and see the difference
##' file.show(x)
##' file.show(f)
##'
##' ## use global options
##' options(keep.comment = TRUE, keep.blank.line = FALSE)
##' tidy.source(x)
##'
##' ## if you've copied R code into the clipboard
##' if (interactive()) {
##' tidy.source("clipboard")
##' ## write into clipboard again
##' tidy.source("clipboard", file = "clipboard")
##' }
##'
##' ## the if-else structure
##' tidy.source(text=c('{if(TRUE)1 else 2; if(FALSE){1+1',"## comments",'} else 2}'))
tidy.source = function(source = "clipboard", keep.comment,
    keep.blank.line, keep.space, replace.assign, output = TRUE, text = NULL,
    width.cutoff = 0.75 * getOption("width"), ...) {
    if (is.null(text)) {
        if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
            source = pipe("pbpaste")
        }
        text.lines = readLines(source, warn = FALSE)
    } else {
        text.lines = text
    }
    if (identical(text.lines, '')) {
        if (output) cat('\n', ...)
        return('')
    }
    if (missing(keep.comment)) {
        keep.comment = if (is.null(getOption('keep.comment'))) TRUE else getOption('keep.comment')
    }
    if (missing(keep.blank.line)) {
        keep.blank.line = if (is.null(getOption('keep.blank.line'))) FALSE else getOption('keep.blank.line')
    }
    if (missing(keep.space)) {
        keep.space = if (is.null(getOption('keep.space'))) FALSE else getOption('keep.space')
    }
    if (missing(replace.assign)) {
        replace.assign = if (is.null(getOption('replace.assign'))) FALSE else getOption('replace.assign')
    }
    tidy.block = function(block.text) {
        exprs = base::parse(text = block.text)
        n = length(exprs)
        res = character(n)
        for (i in 1:n) {
            dep = paste(base::deparse(exprs[i], width.cutoff), collapse = "\n")
            res[i] = substring(dep, 12, nchar(dep) - 1)
        }
        return(res)
    }
    if (isTRUE(keep.comment)) {
        ## if you have variable names like this in your code, then you really beat me...
        begin.comment = ".BeGiN_TiDy_IdEnTiFiEr_HaHaHa"
        end.comment = ".HaHaHa_EnD_TiDy_IdEnTiFiEr"
        if (!keep.space)
            text.lines = gsub("^[[:space:]]+|[[:space:]]+$", "", text.lines)
        head.comment = grepl('^[[:space:]]*#', text.lines)
        if (any(head.comment)) {
            text.lines[head.comment] = gsub("\"", "'", text.lines[head.comment])
            text.lines[head.comment] = gsub("\\", "\\\\", text.lines[head.comment], fixed = TRUE)
        }
        ## wrap long comments if you do not want to preserve leading spaces
        if (!keep.space) {
            ## don't wrap roxygen comments
            head.idx = which(head.comment & !grepl("^[[:space:]]*#*#'", text.lines))
            k = 0 # k records how far the wrapped comments have pushed the index
            for (i in head.idx) {
                j = i + k               # j records the real index
                tmp = strwrap(text.lines[j], width = width.cutoff, prefix = "# ",
                              exdent = 2, initial = '')
                if (length(tmp) > 1) {
                    text.lines[j] = tmp[1]
                    text.lines = append(text.lines, tmp[-1], j)
                }
                k = k + length(tmp) - 1
            }
            head.comment = grepl('^[[:space:]]*#', text.lines)
        }
        text.lines[head.comment] = sprintf("%s<-\"%s%s\"",
                begin.comment, text.lines[head.comment], end.comment)
        blank.line = grepl('^[[:space:]]*$', text.lines)
        if (any(blank.line) && isTRUE(keep.blank.line))
            text.lines[blank.line] = sprintf("%s<-\"%s\"", begin.comment, end.comment)
        ## replace end-of-line comments to cheat R
        enc = options(encoding = "native.enc")
        out = attr(parser(text = text.lines), 'data')
        options(enc)
        out = subset(out, out$terminal)
        if (nrow(out) > 0) {
            if (replace.assign) {
                out$text[out$token.desc=='EQ_ASSIGN'] = '<-'
            }
            ## is inline comment?
            idx1 = c(FALSE, diff(out$line1)==0) & (out$token.desc=='COMMENT')
            ## is last line '{'?
            idx2 = c(FALSE, (out$text == '{')[-length(idx1)])
            out$text[idx1] = gsub('"', "'", out$text[idx1])
            idx = idx1 & (!idx2)
            out$text[idx] = sprintf(' %%InLiNe_IdEnTiFiEr%% "%s"', out$text[idx])
            idx = idx1 & idx2
            out$text[idx] = sprintf('%s<-\"%s%s\"', begin.comment, out$text[idx], end.comment)
            text.lines = tapply(out$text, out$line1, paste, collapse=' ')
        }
        text.mask = tidy.block(text.lines)
        text.tidy = unmask.source(text.mask, replace.tab = keep.space)
    }
    else {
        text.tidy = text.mask = tidy.block(text.lines)
        begin.comment = end.comment = ""
    }
    if (output) cat(paste(text.tidy, collapse = "\n"), "\n", ...)
    invisible(list(text.tidy = text.tidy, text.mask = text.mask,
        begin.comment = begin.comment, end.comment = end.comment))
}

##' Restore the real source code from the masked text
##'
##' Remove the masks from the code to restore the real code.
##' @param text.mask the masked source code
##' @param replace.tab whether to replace \code{\\\\t} with \code{\\t}
##' @return the real source code (a character vector)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples
##' library(formatR)
##'
##' src = c("    # a single line of comments is preserved",
##' '1+1', '  ', 'if(TRUE){',
##' "x=1  # inline comments!", '}else{',
##' "x=2;print('Oh no... ask the right bracket to go away!')}",
##' "2+2+2    # 'short comments'",
##' "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments",
##' "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")
##'
##' x = tidy.source(text = src, output = FALSE)$text.mask
##'
##' cat(x, sep = '\n')
##'
##' cat(unmask.source(x), sep = '\n')
unmask.source = function(text.mask, replace.tab = FALSE) {
    idx = grepl('\\.BeGiN_TiDy_IdEnTiFiEr_HaHaHa <- ', text.mask)
    text.mask[idx] = gsub('\\\\', '\\', text.mask[idx], fixed = TRUE)
    text.tidy = gsub("\\.BeGiN_TiDy_IdEnTiFiEr_HaHaHa <- \"|\\.HaHaHa_EnD_TiDy_IdEnTiFiEr\"", "", text.mask)
    ## if the comments were separated into the next line, then remove '\n' after
    ##   the identifier first to move the comments back to the same line
    text.tidy = gsub(" %InLiNe_IdEnTiFiEr%[ ]*[^\n]*\"([ ]*#[^\"]*)\"", "  \\1",
                     gsub("%InLiNe_IdEnTiFiEr%[ ]*\n", "%InLiNe_IdEnTiFiEr%", text.tidy))
    ## move 'else ...' back to the last line
    text.tidy = gsub('\n[[:space:]]*else', ' else', text.tidy)
    if (replace.tab) gsub('\\\\t', '\t', text.tidy) else text.tidy
}


##' A weird operator for internal use only
##'
##' This operator is almost meaningless; it is used to mask the inline
##' comments.
##'
##' @name \%InLiNe_IdEnTiFiEr\%
##' @rdname InLiNe_IdEnTiFiEr
##' @usage x %InLiNe_IdEnTiFiEr% y
##' @param x the argument before the operator
##' @param y the argument after the operator
##' @return \code{x} (i.e. this operator always returns the object on
##' the left-hand-side)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{tidy.source}}
##' @export "%InLiNe_IdEnTiFiEr%"
##' @examples
##' ## we can use it with *anything*
##'
##' 1 %InLiNe_IdEnTiFiEr% 2
##'
##' 1:10 %InLiNe_IdEnTiFiEr% "asdf"
##'
##' lm %InLiNe_IdEnTiFiEr% 'garbage'
##'
"%InLiNe_IdEnTiFiEr%" <- function(x, y) {
    x
}



##' Modified versions of parse() and deparse()
##'
##' These two functions parse and deparse the masked source code.
##'
##' For \code{\link{parse.tidy}}, the source code is masked to
##' preserve comments, then this function uses
##' \code{\link[base]{parse}} to return the parsed but unevaluated
##' expressions in a list.
##'
##' For \code{\link{deparse.tidy}}, it uses
##' \code{\link[base]{deparse}} to turn the unevaluated (and masked)
##' expressions into character strings; the masks will be removed to
##' restore the real source code. See \code{\link{unmask.source}}.
##' @param text the source code as a character string to be passed to
##' \code{\link{tidy.source}}
##' @param ... for \code{\link{parse.tidy}}: other arguments to be passed to
##' \code{\link{tidy.source}}; for \code{\link{deparse.tidy}}:
##' arguments to be passed to \code{\link[base]{deparse}}
##' @return \code{\link{parse.tidy}} returns the unevaluated
##' expressions; \code{\link{deparse.tidy}} returns the character
##' strings
##' @author Yihui Xie <\url{http://yihui.name}>
##' @note These functions are mainly designed for the package
##' \pkg{pgfSweave}; they may not be useful to general users.
##' @seealso \code{\link[base]{parse}}, \code{\link[base]{deparse}},
##' \code{\link{tidy.source}}
##' @export
##' @examples
##' src = c("    # a single line of comments is preserved",
##' '1+1', '  ', 'if(TRUE){',
##' "x=1  # comments begin with at least 2 spaces!", '}else{',
##' "x=2;print('Oh no... ask the right bracket to go away!')}",
##' '1*3 # this comment will be dropped!',
##' "2+2+2    # 'short comments'",
##' "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments",
##' "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")
##'
##' (expr = parse.tidy(src))
##'
##' parse.tidy(src, keep.blank.line = TRUE)
##'
##' cat(deparse.tidy(expr))
##'
##' deparse.tidy(expr, width.cutoff = 50)
##'
parse.tidy = function(text, ...) {
    tidy.res = tidy.source(text = text, output = FALSE, ...)
    base::parse(text = tidy.res$text.mask)
}
##' @param expr the unevaluated expressions (ideally as results from
##' \code{\link{parse.tidy}})
##' @rdname parse.tidy
##' @export
deparse.tidy = function(expr, ...) {
    unmask.source(paste(base::deparse(expr, ...), collapse = '\n'))
}

##' Format the R scripts under a directory
##'
##' This function first look for all the R scripts under a directory
##' (using the pattern \code{"\\\\.[RrSsQq]$"}), then uses
##' \code{\link{tidy.source}} to tidy these scripts. The original
##' scripts will be overwritten with formatted code. You may need to
##' back up the original directory first if you do not fully
##' understand the tricks \code{\link{tidy.source}} is using.
##' @param path the directory
##' @param recursive whether to recursively look for R scripts under
##' \code{path}
##' @param ... other arguments to be passed to
##' \code{\link{tidy.source}}
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{tidy.source}}
##' @export
##' @examples
##' library(formatR)
##'
##' path = tempdir()
##' file.copy(system.file('demo', package = 'base'), path, recursive=TRUE)
##' tidy.dir(path, recursive=TRUE)
tidy.dir = function(path = '.', recursive = FALSE, ...) {
    flist = list.files(path, pattern = '\\.[RrSsQq]$', full.names = TRUE, recursive = recursive)
    for (f in flist) {
        message('tidying ', f)
        try(tidy.source(f, file = f, ...))
    }
}
