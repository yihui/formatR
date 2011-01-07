##' `Tidy up' R code and try to preserve comments.
##' This function has nothing to do with code optimization; it just
##' returns parsed source code, but also tries to preserve comments,
##' which is different with \code{\link[base]{parse}}. See `Details'.
##' (Please read the HTML help page instead of the text help to avoid
##' confusion)
##'
##' This function helps the users to tidy up their source code in a
##' sense that necessary indents and spaces will be added, etc. See
##' \code{\link[base]{parse}}. But comments will be preserved if
##' \code{keep.comment = TRUE}.
##'
##' The method to preserve comments is to protect them as strings in
##' disguised assignments. For example, there is a single line of
##' comments in the source code:
##'
##' \verb{  # asdf}
##'
##' It will be first masked as
##'
##' \verb{SOME_IDENTIFIER = "  # asdfANOTHER_IDENTIFIER"}
##'
##' which is a legal R expression, so \code{\link[base]{parse}} can
##' deal with it and will no longer remove the disguised comments.
##' In the end the identifiers will be removed to restore the original
##' comments, i.e. the strings \code{'SOME_IDENTIFIER = "'} and
##' \code{'ANOTHER_IDENTIFIER"'} are replaced with empty strings.
##'
##' ``Inline'' comments are identified as ``two or more spaces'' plus
##' the hash symbol \code{#} in your source code, e.g.
##'
##' \verb{1+1  #  comments}
##'
##' or
##'
##' \verb{1+1    # comments}
##'
##' This might be dangerous to your source code, for instance,
##'
##' \verb{a = "I'm a string   #yes"}
##'
##' does not contain comments, but this function will treat it as if
##' it does!
##'
##' Inline comments are first disguised as a sum with its preceding R
##' code, which is essentially meaningless but syntactically correct!
##' For example,
##'
##' \verb{1+1 + "   # comments"}
##'
##' then \code{\link[base]{parse}} will deal with this expression;
##' again, the disguised comments will not be removed. In the end,
##' inline comments will be freed as well (remove the plus sign and
##' surrounding double quotes).
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
##' @return A list with components \item{text.tidy}{The parsed code as
##' a character vector.} \item{text.mask}{The code containing
##' comments, which are masked in assignments.} \item{begin.comment,
##' end.comment}{ identifiers used to mark the comments }
##'
##' @note When \code{keep.comment == TRUE}, \emph{you must not use
##' double quotes in your inline comments -- only single quotes are
##' allowed!!} For example, the code below will make the function
##' fail:
##'
##' \verb{1 + 1  # here is the "comment"}
##'
##' Instead, you have to write:
##'
##' \verb{1 + 1  # here is the 'comment'}
##'
##' There are hidden options which can control the behaviour of this
##' function: the argument \code{keep.comment} gets its value from
##' \code{options('keep.comment')} by default;  \code{keep.blank.line}
##' from \code{options('keep.blank.line')}, and \code{keep.space} from
##' \code{options('keep.space')}. If these options are \code{NULL},
##' the default values will be \code{TRUE}, \code{FALSE} and
##' \code{FALSE} respectively.
##' @author Yihui Xie <\url{http://yihui.name}> with substantial
##' contribution from Yixuan Qiu <\url{http://yixuan.cos.name}>
##' @seealso \code{\link[base]{parse}}, \code{\link[base]{deparse}},
##' \code{\link[base]{cat}}
##' @references
##' \url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}
##' @keywords IO
##' @export
##' @examples
##'
##' ## use the 'text' argument
##' src = c(' # a single line of comments is preserved',
##' '1+1', '  ', 'if(TRUE){',
##' paste('x=1  ', '# comments begin with at least 2 spaces!'), '}else{',
##' "x=2;print('Oh no... ask the right bracket to go away!')}",
##' '1*3 # this comment will be dropped!')
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
##' \dontrun{
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
        begin.comment = "BeGiN_TiDy_IdEnTiFiEr_HaHaHa"
        end.comment = "HaHaHa_EnD_TiDy_IdEnTiFiEr"
        if (!keep.space)
            text.lines = gsub("^[[:space:]]+|[[:space:]]+$", "", text.lines)
        head.comment = grepl('^[[:space:]]*#', text.lines)
        if (any(head.comment)) {
            text.lines[head.comment] = gsub("\"", "'", text.lines[head.comment])
            text.lines[head.comment] = sprintf("%s=\"%s%s\"",
                begin.comment, text.lines[head.comment], end.comment)
        }
        blank.line = grepl('^[[:space:]]*$', text.lines)
        if (any(blank.line) && isTRUE(keep.blank.line))
            text.lines[blank.line] = sprintf("%s=\"%s\"", begin.comment, end.comment)
        ## replace end-of-line comments by + 'comments' to cheat R
        text.lines[!head.comment] = sub("([ ]{2,}#.*)$", " + \"\\1\"", text.lines[!head.comment])
        text.mask = tidy.block(text.lines)
        text.tidy = gsub(sprintf("%s = \"|%s\"", begin.comment,
            end.comment), "", text.mask)
        text.tidy = gsub(" \\+[ ]{0,1}[\n ]*\"([ ]{2,}#[^\"]*)\"", "\\1", text.tidy)
    }
    else {
        text.tidy = text.mask = tidy.block(text.lines)
        begin.comment = end.comment = ""
    }
    if (output) cat(paste(text.tidy, collapse = "\n"), "\n", ...)
    invisible(list(text.tidy = text.tidy, text.mask = text.mask,
        begin.comment = begin.comment, end.comment = end.comment))
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
