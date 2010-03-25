formatR <- function() {
    options(guiToolkit = "RGtk2")
    # TODO: gtext() cannot handle multi-byte characters? encoding problems?
    # I cannot find a solution yet...
    g = ggroup(horizontal = FALSE, container = gwindow("Tidy R Source"))
    g1 = ggroup(container = g, expand = TRUE)
    g2 = ggroup(container = g)
    txt = gtext("", container = g1, wrap = FALSE, expand = TRUE)
    tag(txt, "font.attr") = c(family = "monospace", sizes = "medium",
        styles = "normal", weights = "light")
    tag(txt, "tidy.opt") = list(keep.comment = TRUE, keep.blank.line = TRUE,
        width.cutoff = 60)
    tag(txt, "enc.from") = ""
    tag(txt, "enc.to") = ""
    gbutton("open", container = g2, handler = function(h, ...) {
        s = gfile("Open R Source")
        if (!is.na(s))
            svalue(txt) = readLines(s)
        tag(txt, "src.file") = s
        tooltip(txt) = s
    })
    gbutton("convert", container = g2, handler = function(h,
        ...) {
        con = tempfile()
        enc = getOption("encoding")
        options(encoding = "")
        writeLines(svalue(txt), con)
        svalue(txt) = ""
        tidy.opt = tag(txt, "tidy.opt")
        text.tidy = tidy.source(con, keep.comment = tidy.opt$keep.comment,
            keep.blank.line = tidy.opt$keep.blank.line, width.cutoff = tidy.opt$width.cutoff,
            output = FALSE)$text.tidy
        # Encoding(text.tidy)='UTF-8'
        # text.tidy=iconv(text.tidy,from='UTF-8',to='GB2312')
        insert(txt, text.tidy, font.attr = tag(txt, "font.attr"))
        options(encoding = enc)
        unlink(con)
    })
    gbutton("save", container = g2, handler = function(h, ...) {
        s = tag(txt, "src.file")
        if (is.null(s))
            s = gfile("Save", type = "save")
        if (!is.na(s))
            writeLines(svalue(txt), s)
    })
    gbutton("save-as", container = g2, handler = function(h,
        ...) {
        s = gfile("Save as", type = "save")
        if (!is.na(s))
            writeLines(svalue(txt), s)
    })
    gbutton("execute", container = g2, handler = function(h,
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
        rm(list = text.tidy$begin.comment, pos = 1)
        cat(paste(x, collapse = "\n"), "\n")
        close(zz)
        unlink(con)
    })
    gbutton("select-font", container = g2, handler = function(h,
        ...) {
        w = gwindow("Font Specification", )
        g = ggroup(horizontal = FALSE, container = w)
        tbl = glayout(container = g, expand = TRUE, spacing = 0)
        ft = tag(txt, "font.attr")
        tbl[1, 1, expand = TRUE] = (gf.size <- gframe("Size",
            container = tbl))
        r.size = gradio(tag(txt)$tags$sizes, which(ft["sizes"] ==
            tag(txt)$tags$sizes), horizontal = TRUE, container = gf.size)
        tbl[2, 1, expand = TRUE] = (gf.weight <- gframe("Weight",
            container = tbl))
        r.weight = gradio(tag(txt)$tags$weights, which(ft["weights"] ==
            tag(txt)$tags$weights), horizontal = TRUE, container = gf.weight)
        tbl[3, 1, expand = TRUE] = (gf.style <- gframe("Style",
            container = tbl))
        r.style = gradio(tag(txt)$tags$styles, which(ft["styles"] ==
            tag(txt)$tags$styles), horizontal = TRUE, container = gf.style)
        g1 = ggroup(container = g)
        b.ok = gbutton("ok", container = g1, handler = function(h,
            ...) {
            tmp = svalue(txt)
            svalue(txt) = ""
            ft = c(family = "monospace", sizes = svalue(r.size),
                weights = svalue(r.weight), styles = svalue(r.style))
            insert(txt, tmp, font.attr = ft)
            tag(txt, "font.attr") = ft
            dispose(w)
        })
        b.cancel = gbutton("cancel", container = g1, handler = function(h,
            ...) {
            dispose(w)
        })
    })
    gbutton("preferences", container = g2, handler = function(h,
        ...) {
        w = gwindow("Preferences")
        g = ggroup(horizontal = FALSE, container = w)
        tbl = glayout(container = g, expand = TRUE, spacing = 0)
        tidy.opt = tag(txt, "tidy.opt")
        tbl[1, 1, expand = TRUE] = (gf.kc <- gframe("Keep Comments?",
            container = tbl))
        r.kc = gradio(c(TRUE, FALSE), which(as.logical(tidy.opt$keep.comment) ==
            c(TRUE, FALSE)), horizontal = TRUE, container = gf.kc)
        tbl[2, 1, expand = TRUE] = (gf.kb <- gframe("Keep Blank Lines?",
            container = tbl))
        r.kb = gradio(c(TRUE, FALSE), which(as.logical(tidy.opt$keep.blank.line) ==
            c(TRUE, FALSE)), horizontal = TRUE, container = gf.kb)
        tbl[3, 1, expand = TRUE] = (gf.wi <- gframe("Text Width",
            container = tbl))
        r.wi = gedit(as.character(tidy.opt$width.cutoff), container = gf.wi,
            coerce.with = as.integer)
        tbl[4, 1, expand = TRUE] = (gf.enc <- gframe("Encoding Conversion",
            container = tbl))
        glabel("From:", container = gf.enc)
        r.enc.from = gdroplist(c("", iconvlist()), editable = TRUE,
            container = gf.enc)
        glabel("To:", container = gf.enc)
        r.enc.to = gdroplist(c("", iconvlist()), editable = TRUE,
            container = gf.enc)
        g1 = ggroup(container = g)
        b.ok = gbutton("ok", container = g1, handler = function(h,
            ...) {
            if (is.na(svalue(r.wi)))
                gmessage("Please input an integer for the text width!",
                  "Error", icon = "error")
            else {
                tag(txt, "tidy.opt") = list(keep.comment = as.logical(svalue(r.kc)),
                  keep.blank.line = as.logical(svalue(r.kb)),
                  width.cutoff = svalue(r.wi))
                tag(txt, "enc.from") = svalue(r.enc.from)
                tag(txt, "enc.to") = svalue(r.enc.to)
                dispose(w)
            }
        })
        b.cancel = gbutton("cancel", container = g1, handler = function(h,
            ...) {
            dispose(w)
        })
    })
    focus(txt)
    invisible(NULL)
}

