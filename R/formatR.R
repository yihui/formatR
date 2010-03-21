formatR <-
function(guiToolkit = "RGtk2") {
    options(guiToolkit = guiToolkit)
    # TODO: gtext() cannot handle multi-byte characters? encoding problems?
    enc = getOption("encoding")
    options(encoding = "")
    g = ggroup(horizontal = FALSE, container = gwindow("Tidy R Source"))
    g1 = ggroup(container = g, expand = TRUE)
    g2 = ggroup(container = g)
    txt = gtext("", container = g1, wrap = FALSE, expand = TRUE)
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
        writeLines(svalue(txt), con)
        svalue(txt) = tidy.source(con)$text.tidy
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
    gbutton("select-font", container = g2, handler = function(h, 
        ...) {
        # TODO: sz=tag(txt)$tags$sizes
        font(txt) = c(family = "monospace", sizes = "large")
    })
    gbutton("preferences", container = g2, handler = function(h, 
        ...) {
        gmessage("Not implemented yet!")
    })
    options(encoding = enc)
    invisible(NULL)
}

