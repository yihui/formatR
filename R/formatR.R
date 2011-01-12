##' `Tidy up' R code and try to preserve comments.
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
##' \preformatted{.SOME_IDENTIFIER = "  # asdf.ANOTHER_IDENTIFIER"}
##'
##' which is a legal R expression, so \code{\link[base]{parse}} can
##' deal with it and will no longer remove the disguised comments.
##' In the end the identifiers will be removed to restore the original
##' comments, i.e. the strings \code{'.SOME_IDENTIFIER = "'} and
##' \code{'.ANOTHER_IDENTIFIER"'} are replaced with empty strings.
##'
##' ``Inline'' comments are identified as ``two or more spaces'' plus
##' the hash symbol \code{#} without any following double quotes in
##' the line, e.g.
##' \preformatted{1+1  #  comments}
##'
##' or
##' \preformatted{1+1    # comments}
##'
##' This might be dangerous to your source code, for instance,
##' \preformatted{a = 'I am a string   #yes'}
##'
##' does not contain comments (\code{#} is inside a string), but this
##' function will treat it as if it does! If you need to use the hash
##' symbol in a string, you must put it in double quotes, e.g.
##' \preformatted{a = "I am a string   #yes"}
##'
##' Inline comments are first disguised as a weird operation with its
##' preceding R code, which is essentially meaningless but
##' syntactically correct!  For example,
##' \preformatted{1+1 %InLiNe_IdEnTiFiEr% "   # comments"}
##'
##' then \code{\link[base]{parse}} will deal with this expression;
##' again, the disguised comments will not be removed. In the end,
##' inline comments will be freed as well (remove the operator
##' \code{%InLiNe_IdEnTiFiEr%} and surrounding double quotes).
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
##' @note When \code{keep.comment == TRUE}, \emph{you must not use
##' double quotes in your inline comments -- only single quotes are
##' allowed!!} For example, the code below will make the function
##' discard the comments:
##' \preformatted{1 + 1  # here is the "comment"}
##'
##' Instead, you have to write like this to protect the comments:
##' \preformatted{1 + 1  # here is the 'comment'}
##'
##' There are hidden options which can control the behaviour of this
##' function: the argument \code{keep.comment} gets its value from
##' \code{options('keep.comment')} by default;  \code{keep.blank.line}
##' from \code{options('keep.blank.line')}, and \code{keep.space} from
##' \code{options('keep.space')}. If these options are \code{NULL},
##' the default values will be \code{TRUE}, \code{FALSE} and
##' \code{FALSE} respectively.
##'
##' Also note that if \code{keep.space} is \code{FALSE}, single lines
##' of long comments will be wrapped into shorter ones automatically.
##' Otherwise, long comments will not be wrapped, so they may exceed
##' the page margin, and \code{\\\\t} will be replaced with
##' \code{\\t}.
##'
##' \subsection{Warning}{ The best strategy to avoid failure is to put
##' comments in whole lines or after \emph{complete} R
##' expressions. Here are some examples which could make
##' \code{\link{tidy.source}} fail:
##' \preformatted{if (TRUE) {  ## comments right after a curly brace } }
##'
##' \preformatted{1 + 2 +   ## comments after an incomplete line
##'
##'   3 + 4}
##'
##' }
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
##' "x=1  # comments begin with at least 2 spaces!", '}else{',
##' "x=2;print('Oh no... ask the right bracket to go away!')}",
##' '1*3 # this comment will be dropped!',
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
##'
tidy.source = function(source = "clipboard", keep.comment,
    keep.blank.line, keep.space, output = TRUE, text = NULL,
    width.cutoff = 0.75 * getOption("width"), ...) {
    if (is.null(text)) {
        if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
            source = pipe("pbpaste")
        }
        text.lines = readLines(source, warn = FALSE)
    } else {
        text.lines = text
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
        }
        ## wrap long comments if you do not want to preserve leading spaces
        if (!keep.space) {
            head.idx = which(head.comment)
            k = 0 # k records how far the wrapped comments have pushed the index
            for (i in head.idx) {
                j = i + k               # j records the real index
                tmp = strwrap(text.lines[j], width = width.cutoff, prefix = "# ",
                              exdent = 2, initial = '')
                text.lines[j] = tmp[1]
                if (length(tmp) > 1) text.lines = append(text.lines, tmp[-1], j)
                k = k + length(tmp) - 1
            }
            head.comment = grepl('^[[:space:]]*#', text.lines)
        }
        text.lines[head.comment] = sprintf("%s=\"%s%s\"",
                begin.comment, text.lines[head.comment], end.comment)
        blank.line = grepl('^[[:space:]]*$', text.lines)
        if (any(blank.line) && isTRUE(keep.blank.line))
            text.lines[blank.line] = sprintf("%s=\"%s\"", begin.comment, end.comment)
        ## replace end-of-line comments to cheat R
        text.lines[!head.comment] = sub("([ ]{2,}#[^\"]*)$", " %InLiNe_IdEnTiFiEr% \"\\1\"", text.lines[!head.comment])
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

##' Restore the real source code from the masked text.
##'
##'
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
##' "x=1  # comments begin with at least 2 spaces!", '}else{',
##' "x=2;print('Oh no... ask the right bracket to go away!')}",
##' '1*3 # this comment will be dropped!',
##' "2+2+2    # 'short comments'",
##' "lm(y~x1+x2)  ### only 'single quotes' are allowed in comments",
##' "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1  ## comments after a long line")
##'
##' x = tidy.source(text = src, output = FALSE)$text.mask
##'
##' cat(x, sep = '\n')
##'
##' cat(unmask.source(x), sep = '\n')
##'
unmask.source = function(text.mask, replace.tab = FALSE) {
    text.tidy = gsub("\\.BeGiN_TiDy_IdEnTiFiEr_HaHaHa = \"|\\.HaHaHa_EnD_TiDy_IdEnTiFiEr\"", "", text.mask)
    ## if the comments were separated into the next line, then remove '\n' after
    ##   the identifier first to move the comments back to the same line
    text.tidy = gsub(" %InLiNe_IdEnTiFiEr%[ ]*[^\n]*\"([ ]{2,}#[^\"]*)\"", "\\1",
    gsub("%InLiNe_IdEnTiFiEr%[ ]*\n", "%InLiNe_IdEnTiFiEr%", text.tidy))
    if (replace.tab) gsub('\\\\t', '\t', text.tidy) else text.tidy
}

##' A GUI to format R code.
##' Create a GUI (via GTK+ by default) to format R code.
##'
##' This function calls \code{\link{tidy.source}} to format R code.
##' Spaces and indent will be added to the code automatically.
##'
##' We can either open an R source file or directly write R code in the text
##' widget. Click the ``convert'' button, and the code will become tidy. See
##' \code{\link{tidy.source}} for more details.
##'
##' @param guiToolkit the GUI toolkit to use
##' @return the text widget is returned
##' @note
##' By default, the interface is based on GTK+ (R package \bold{RGtk2}), but
##' other options (\bold{tcltk}, \bold{rJava} and \bold{Qt}) are possible too. See the
##' examples below. Note the ``Font'' button is only for the GTK+ interface.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{tidy.source}}
##' @export
##' @references
##'   \url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}
##' @examples
##'
##' if (interactive() && require('gWidgetsRGtk2')) {
##'
##' ## a GUI will show up on loading if one of the gWidgets
##' ##   toolkit is present (e.g. via library(gWidgetsRGtk2))
##' library(formatR)
##'
##' g = formatR()
##'
##' ## we have control over the text widget, e.g. set or get the text
##'
##' svalue(g) = c("# a single line of comments is preserved", "1+1",
##'     "if(TRUE){", paste("x=1 ",
##'     "# comments begin with at least 2 spaces!"), "}else{",
##'     "x=2;print('Oh no... ask the right bracket to go away!')}",
##'     "1*3 # this comment will be dropped!")
##'
##' ## click 'Convert' now, and see
##'
##' cat(svalue(g), sep = '\n')   # get its value
##'
##' ## tcl/tk interface: need gWidgetstcltk package
##' formatR('tcltk')
##'
##' ## Java interface: need gWidgetsrJava package
##' formatR('rJava')
##'
##' }
##'
formatR = function(guiToolkit = 'RGtk2') {
    options(guiToolkit = guiToolkit)
    stopifnot(require(paste('gWidgets', guiToolkit, sep = ''), character.only = TRUE))

    g = ggroup(horizontal = FALSE, container = gwindow("Tidy R Source"))
    g1 = ggroup(container = g, expand = TRUE)
    g2 = ggroup(container = g)

    txt = gtext(container = g1, wrap = FALSE, font.attr = c(family = "monospace",
        size = "medium"), expand = TRUE)
    tag(txt, "font.attr") = c(family = "monospace", size = "medium",
        weight = "light")
    tag(txt, "tidy.opt") = list(keep.comment = TRUE, keep.blank.line = TRUE,
        width.cutoff = 60)
    gbutton("Open", container = g2, handler = function(h, ...) {
        s = gfile("Open R Source")
        if (!is.na(s)) {
            svalue(txt) = readLines(s)
            tag(txt, "src.file") = s
            tooltip(txt) = s
        }
    })
    gbutton("Convert", container = g2, handler = function(h,
        ...) {
        tidy.opt = tag(txt, "tidy.opt")
        src = svalue(txt)
        Encoding(src) = 'UTF-8'
        src = unlist(strsplit(src, '\n', fixed = TRUE))
        text.tidy = tidy.source(text = src,
            keep.comment = tidy.opt$keep.comment,
            keep.blank.line = tidy.opt$keep.blank.line,
            width.cutoff = tidy.opt$width.cutoff,
            output = FALSE)$text.tidy
        svalue(txt) = text.tidy
    })
    gbutton("Save", container = g2, handler = function(h, ...) {
        s = tag(txt, "src.file")
        if (is.null(s))
            s = gfile("Save", type = "save")
        if (!is.na(s))
            writeLines(svalue(txt), s)
    })
    gbutton("Save-as", container = g2, handler = function(h,
        ...) {
        s = gfile("Save as", type = "save")
        if (!is.na(s))
            writeLines(svalue(txt), s)
    })
    gbutton("Execute", container = g2, handler = function(h,
        ...) {
        src = svalue(txt)
        Encoding(src) = 'UTF-8'
        zz = textConnection(src)
        source(zz, max.deparse.length = Inf, echo = TRUE)
        close(zz)
    })
    if (getOption("guiToolkit") == "RGtk2") {
        gbutton("Select-font", container = g2, handler = function(h,
            ...) {
            w = gwindow("Font Specification", )
            g = ggroup(horizontal = FALSE, container = w)
            tbl = glayout(container = g, expand = TRUE, spacing = 0)
            ft = tag(txt, "font.attr")
            tbl[1, 1, expand = TRUE] = (gf.size <- gframe("Size",
                container = tbl))
            r.size = gradio(tag(txt)$tags$sizes, which(ft["size"] ==
                tag(txt)$tags$sizes), horizontal = TRUE, container = gf.size)
            tbl[2, 1, expand = TRUE] = (gf.weight <- gframe("Weight",
                container = tbl))
            r.weight = gradio(tag(txt)$tags$weight, which(ft["weight"] ==
                tag(txt)$tags$weight), horizontal = TRUE, container = gf.weight)
            g1 = ggroup(container = g)
            b.ok = gbutton("OK", container = g1, handler = function(h,
                ...) {
                ft = c(family = "monospace", size = svalue(r.size),
                  weight = svalue(r.weight))
                font(txt) = ft
                tag(txt, "font.attr") = ft
                dispose(w)
            })
            b.cancel = gbutton("Cancel", container = g1, handler = function(h,
                ...) {
                dispose(w)
            })
        })
    }
    gbutton("Preferences", container = g2, handler = function(h,
        ...) {
        w = gwindow("Preferences")
        g = ggroup(horizontal = FALSE, container = w)
        tbl = glayout(container = g, expand = TRUE, spacing = 0)
        tidy.opt = tag(txt, "tidy.opt")
        tbl[1, 1, expand = TRUE] = (gf.kc <- gframe("Keep Comments?",
            container = tbl))
        r.kc = gradio(c("TRUE", "FALSE"), which(as.logical(tidy.opt$keep.comment) ==
            c(TRUE, FALSE)), horizontal = TRUE, container = gf.kc)
        tbl[2, 1, expand = TRUE] = (gf.kb <- gframe("Keep Blank Lines?",
            container = tbl))
        r.kb = gradio(c("TRUE", "FALSE"), which(as.logical(tidy.opt$keep.blank.line) ==
            c(TRUE, FALSE)), horizontal = TRUE, container = gf.kb)
        tbl[3, 1, expand = TRUE] = (gf.wi <- gframe("Text Width",
            container = tbl))
        r.wi = gedit(as.character(tidy.opt$width.cutoff), container = gf.wi,
            coerce.with = as.integer)
        g1 = ggroup(container = g)
        b.ok = gbutton("OK", container = g1, handler = function(h,
            ...) {
            if (is.na(svalue(r.wi)))
                gmessage("Please input an integer for the text width!",
                  "Error", icon = "error")
            else {
                tag(txt, "tidy.opt") = list(keep.comment = as.logical(svalue(r.kc)),
                  keep.blank.line = as.logical(svalue(r.kb)),
                  width.cutoff = svalue(r.wi))
                dispose(w)
            }
        })
        b.cancel = gbutton("Cancel", container = g1, handler = function(h,
            ...) {
            dispose(w)
        })
    })
    invisible(txt)
}


##' A weird operator for internal use only.
##' This operator is almost meaningless; it is used to mask the inline comments.
##'
##' @name %InLiNe_IdEnTiFiEr%
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



##' Modified versions of parse() and deparse().
##'
##' the source code is masked to preserve comments, then this function
##' uses \code{\link[base]{parse}} to return the parsed but
##' unevaluated expressions in a list.
##'
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

##' Modified versions of parse() and deparse()
##'
##' it uses \code{\link[base]{deparse}} to turn the unevaluated (and
##' masked) expressions into character strings; the masks will be
##' removed to restore the real source code. See \code{\link{unmask.source}}.
##' @param expr the unevaluated expressions (ideally as results from
##' \code{\link{parse.tidy}})
##' @rdname parse.tidy
##' @export
deparse.tidy = function(expr, ...) {
    unmask.source(paste(base::deparse(expr, ...), collapse = '\n'))
}

##' Format the R scripts under a directory.
##'
##' This function first look for all the R scripts under a directory
##' (using the pattern \code{"\\\\.[RrSsQq]$"}), then uses
##' \code{\link{tidy.source}} to tidy these scripts. The original
##' scripts will be overwritten with formatted code. You may need to
##' back up the original directory first if you do not fully
##' understand the tricks \code{\link{tidy.source}} is using.
##' @param path the directory
##' @param recursive whether to recursively look for R scripts under \code{path}
##' @param ... other arguments to be passed to \code{\link{tidy.source}}
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{tidy.source}}
##' @export
##' @examples
##' library(formatR)
##'
##' if (interactive()) {
##' path = tempdir()
##' file.copy(system.file('demo', package = 'base'), path, recursive=TRUE)
##' tidy.dir(path, recursive=TRUE)
##' }
##'
tidy.dir = function(path = '.', recursive = FALSE, ...) {
    flist = list.files(path, pattern = '\\.[RrSsQq]$', full.names = TRUE, recursive = recursive)
    for (f in flist) {
        message('tidying ', f)
        tidy.source(f, file = f, ...)
    }
}
