##' `Tidy up' R code and partially preserve comments.
##' Actually this function has nothing to do with code optimization; it just
##' returns parsed source code, but some comments can be preserved, which is
##' different with \code{\link[base]{parse}}. See `Details'.
##'
##' This function helps the users to tidy up their source code in a sense that
##' necessary indents and spaces will be added, etc. See
##' \code{\link[base]{parse}}. But comments which are in single lines will be
##' preserved if \code{keep.comment = TRUE} (inline comments will be removed).
##'
##' The method to preserve comments is to protect them as strings in disguised
##' assignments: combine \code{end.comment} to the end of a comment line and
##' 'assign' the whole line to \code{begin.comment}, so the comment line will
##' not be removed when parsed. At last, we remove the identifiers
##' \code{begin.comment} and \code{end.comment}.
##'
##' @param source a string: location of the source code (default to be the
##'   clipboard; this means we can copy the code to clipboard and use
##'   \code{tidy.souce()} without specifying the argument \code{source})
##' @param keep.comment logical value: whether to keep comments or not?
##' (\code{TRUE} by default)
##' @param keep.blank.line logical value: whether to keep blank lines or not?
##' (\code{FALSE} by default)
##' @param begin.comment,end.comment identifiers to mark the comments
##' @param output output to the console or a file using
##'   \code{\link[base]{cat}}?
##' @param width.cutoff passed to \code{\link[base]{deparse}}: integer in [20,
##'   500] determining the cutoff at which line-breaking is tried (default to be
##' \code{0.75 * getOption("width")})
##' @param \dots other arguments passed to \code{\link[base]{cat}}, e.g.
##'   \code{file}
##' @return A list with components \item{text.tidy}{The parsed code as a
##'   character vector.} \item{text.mask}{The code containing comments, which
##'   are masked in assignments.} \item{begin.comment, end.comment}{
##'   identifiers used to mark the comments }
##'
##' @note For Mac users, this function will automatically set \code{source} to
##'   be \code{pipe("pbpaste")} so that we still don't need to specify this
##'   argument if we want to read the code form the clipboard.
##'
##' There are hidden options which can control the behaviour of this function:
##' the argument \code{keep.comment} gets value from \code{options('keep.comment')}
##' by default; and \code{keep.blank.line} from \code{options('keep.blank.line')}.
##' @author Yihui Xie <\url{http://yihui.name}> with substantial contribution
##'   from Yixuan Qiu <\url{http://yixuan.cos.name}>
##' @seealso \code{\link[base]{parse}}, \code{\link[base]{deparse}},
##'   \code{\link[base]{cat}}
##' @references
##'   \url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}
##' @keywords IO
##' @export
##' @examples
##'
##' ## tidy up the source code of image demo
##' x = file.path(system.file(package = "graphics"), "demo", "image.R")
##' # to console
##' tidy.source(x)
##' # to a file
##' f = tempfile()
##' tidy.source(x, keep.blank.line = TRUE, file = f)
##' ## check the original code here and see the difference
##' file.show(x)
##' file.show(f)
##' ## use global options
##' options(keep.comment = TRUE, keep.blank.line = FALSE)
##' tidy.source(x)
##' ## if you've copied R code into the clipboard
##' \dontrun{
##' tidy.source("clipboard")
##' ## write into clipboard again
##' tidy.source("clipboard", file = "clipboard")
##' }
##'
tidy.source = function(source = "clipboard", keep.comment,
    keep.blank.line, begin.comment, end.comment, output = TRUE,
    width.cutoff = 0.75 * getOption("width"), ...) {
    if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
        source = pipe("pbpaste")
    }
    if (missing(keep.comment)) {
        keep.comment = if (is.null(getOption('keep.comment'))) TRUE else getOption('keep.comment')
    }
    if (missing(keep.blank.line)) {
        keep.blank.line = if (is.null(getOption('keep.blank.line'))) FALSE else getOption('keep.blank.line')
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
    text.lines = readLines(source, warn = FALSE)
    if (isTRUE(keep.comment)) {
        identifier = function() paste(sample(LETTERS), collapse = "")
        if (missing(begin.comment))
            begin.comment = identifier()
        if (missing(end.comment))
            end.comment = identifier()
        text.lines = gsub("^[[:space:]]+|[[:space:]]+$", "",
            text.lines)
        while (length(grep(sprintf("%s|%s", begin.comment, end.comment),
            text.lines))) {
            begin.comment = identifier()
            end.comment = identifier()
        }
        head.comment = substring(text.lines, 1, 1) == "#"
        if (any(head.comment)) {
            text.lines[head.comment] = gsub("\"", "'", text.lines[head.comment])
            text.lines[head.comment] = sprintf("%s=\"%s%s\"",
                begin.comment, text.lines[head.comment], end.comment)
        }
        blank.line = text.lines == ""
        if (any(blank.line) && isTRUE(keep.blank.line))
            text.lines[blank.line] = sprintf("%s=\"%s\"", begin.comment,
                end.comment)
        text.mask = tidy.block(text.lines)
        text.tidy = gsub(sprintf("%s = \"|%s\"", begin.comment,
            end.comment), "", text.mask)
    }
    else {
        text.tidy = text.mask = tidy.block(text.lines)
        begin.comment = end.comment = ""
    }
    if (output) cat(paste(text.tidy, collapse = "\n"), "\n", ...)
    invisible(list(text.tidy = text.tidy, text.mask = text.mask,
        begin.comment = begin.comment, end.comment = end.comment))
}


##' GUI to format R code.
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
##' @return Invisible \code{NULL}.
##' @note Inline comments will be removed, as documented in
##'   \code{\link{tidy.source}}.
##'
##' For the time being, multi-byte characters cannot be handled correctly.
##'
##' By default, the interface is based on GTK+ (R package \bold{RGtk2}), but
##' other options (\bold{tcltk}, \bold{rJava} and \bold{Qt}) are possible too. See the
##' examples below. Note the ``Font'' button is only for the GTK+ interface
##' and it can only set font styles for the selected texts.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{tidy.source}}
##' @export
##' @references
##'   \url{http://yihui.name/en/2010/04/formatr-farewell-to-ugly-r-code/}
##' @examples
##'
##' \dontrun{
##'
##' ## a GUI will show up on loading if one of the gWidgets
##' ##   toolkit is present (e.g. via library(gWidgetsRGtk2))
##' library(formatR)
##'
##' ## manually call the GUI
##' formatR()
##'
##' ## tcl/tk interface: need gWidgetstcltk package
##' options(guiToolkit = "tcltk")
##' formatR()
##'
##' ## Java interface: need gWidgetsrJava package
##' options(guiToolkit = "rJava")
##' formatR()
##'
##' }
##'
formatR = function(guiToolkit = 'RGtk2') {
    # TODO: gtext() cannot handle multi-byte characters? encoding problems?
    # I cannot find a solution yet...
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
        con = tempfile()
        enc = getOption("encoding")
        options(encoding = "native.enc")
        on.exit(options(encoding = enc))
        originalCode <- svalue(txt)
        Encoding(originalCode) <- "UTF-8"
        writeLines(originalCode, con)
        tidy.opt = tag(txt, "tidy.opt")
        text.tidy = tidy.source(con, keep.comment = tidy.opt$keep.comment,
            keep.blank.line = tidy.opt$keep.blank.line, width.cutoff = tidy.opt$width.cutoff,
            output = FALSE)$text.tidy
        ## Encoding works on some platforms for multi-byte characters...
        ## Encoding(text.tidy) = "UTF-8"
        svalue(txt) = text.tidy
        unlink(con)
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
        con = tempfile()
        enc = getOption("encoding")
        options(encoding = "")
        writeLines(svalue(txt), con)
        tidy.opt = tag(txt, "tidy.opt")
        text.tidy = tidy.source(con, keep.comment = tidy.opt$keep.comment,
            keep.blank.line = tidy.opt$keep.blank.line, width.cutoff = tidy.opt$width.cutoff,
            output = FALSE)
        zz = textConnection(text.tidy$text.mask)
        options(encoding = enc)
        x = capture.output(source(zz, max.deparse.length = Inf,
            echo = TRUE))
        x = gsub(sprintf("%s = \"|%s\"", text.tidy$begin.comment,
            text.tidy$end.comment), "", x)
        # try(rm(list = text.tidy$begin.comment, pos = 1))
        cat(paste(x, collapse = "\n"), "\n")
        close(zz)
        unlink(con)
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
    invisible(NULL)
}
