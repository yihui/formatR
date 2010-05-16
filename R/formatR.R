formatR <- function() {
    # TODO: gtext() cannot handle multi-byte characters? encoding problems?
    # I cannot find a solution yet...
    tidy.source = function(source = "clipboard", keep.comment = TRUE,
        keep.blank.line = TRUE, begin.comment, end.comment, output = TRUE,
        width.cutoff = 60L, ...) {
        if (source == "clipboard" && Sys.info()["sysname"] ==
            "Darwin") {
            source = pipe("pbpaste")
        }
        tidy.block = function(block.text) {
            exprs = base::parse(text = block.text)
            n = length(exprs)
            res = character(n)
            for (i in 1:n) {
                dep = paste(base::deparse(exprs[i], width.cutoff),
                  collapse = "\n")
                res[i] = substring(dep, 12, nchar(dep) - 1)
            }
            return(res)
        }
        text.lines = readLines(source, warn = FALSE)
        if (keep.comment) {
            identifier = function() paste(sample(LETTERS), collapse = "")
            if (missing(begin.comment))
                begin.comment = identifier()
            if (missing(end.comment))
                end.comment = identifier()
            text.lines = gsub("^[[:space:]]+|[[:space:]]+$",
                "", text.lines)
            while (length(grep(sprintf("%s|%s", begin.comment,
                end.comment), text.lines))) {
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
            if (any(blank.line) & keep.blank.line)
                text.lines[blank.line] = sprintf("%s=\"%s\"",
                  begin.comment, end.comment)
            text.mask = tidy.block(text.lines)
            text.tidy = gsub(sprintf("%s = \"|%s\"", begin.comment,
                end.comment), "", text.mask)
        }
        else {
            text.tidy = text.mask = tidy.block(text.lines)
            begin.comment = end.comment = ""
        }
        if (output)
            cat(paste(text.tidy, collapse = "\n"), "\n", ...)
        invisible(list(text.tidy = text.tidy, text.mask = text.mask,
            begin.comment = begin.comment, end.comment = end.comment))
    }

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
        writeLines(svalue(txt), con)
        tidy.opt = tag(txt, "tidy.opt")
        text.tidy = tidy.source(con, keep.comment = tidy.opt$keep.comment,
            keep.blank.line = tidy.opt$keep.blank.line, width.cutoff = tidy.opt$width.cutoff,
            output = FALSE)$text.tidy
        ## Encoding works on some platforms for multi-byte characters...
        Encoding(text.tidy) = "UTF-8"
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
