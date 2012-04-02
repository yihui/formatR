#' A GUI to format R code
#' 
#' Create a GUI (via GTK+ by default) to format R code.
#' 
#' This function calls \code{\link{tidy.source}} to format R code. Spaces and 
#' indent will be added to the code automatically.
#' 
#' We can either open an R source file or directly write R code in the text 
#' widget. Click the \samp{convert} button, and the code will become tidy. See 
#' \code{\link{tidy.source}} for more details.
#' 
#' @param guiToolkit the GUI toolkit to use
#' @return the text widget is returned
#' @note By default, the interface is based on GTK+ (R package \pkg{RGtk2}), but
#'   other options (\pkg{tcltk}, \pkg{rJava} and \bold{Qt}) are possible too.
#'   See the examples below. Note the \samp{Font} button is only for the GTK+
#'   interface.
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link{tidy.source}}
#' @export
#' @references \url{https://github.com/yihui/formatR/wiki/} (screenshots)
#' @examples \dontrun{
#' library('gWidgetsRGtk2')
#' 
#' ## a GUI will show up on loading if one of the gWidgets
#' ##   toolkit is present (e.g. via library(gWidgetsRGtk2))
#' library(formatR)
#' 
#' g = tidy.gui()
#' 
#' ## we have control over the text widget, e.g. set or get the text
#' 
#' svalue(g) = c("# a single line of comments is preserved", "1+1",
#'     "if(TRUE){", paste("x=1 ",
#'     "# inline comments!"), "}else{",
#'     "x=2;print('Oh no... ask the right bracket to go away!')}",
#'     "1*3 # another inline comment")
#' 
#' ## click 'Convert' now, and see
#' 
#' cat(svalue(g), sep = '\n')   # get its value
#' 
#' ## tcl/tk interface: need gWidgetstcltk package
#' tidy.gui('tcltk')
#' 
#' }
tidy.gui = function(guiToolkit = 'RGtk2') {
  options(guiToolkit = guiToolkit)
  require(paste('gWidgets', guiToolkit, sep = ''), character.only = TRUE)
  
  g = ggroup(horizontal = FALSE, container = gwindow("Tidy R Source"))
  g1 = ggroup(container = g, expand = TRUE)
  g2 = ggroup(container = g)
  
  txt = gtext(container = g1, wrap = FALSE, 
              font.attr = c(family = "monospace", size = "medium"), expand = TRUE)
  tag(txt, "font.attr") = c(family = "monospace", size = "medium", weight = "light")
  tag(txt, "tidy.opt") = list(keep.comment = TRUE, keep.blank.line = TRUE,
                              keep.space = FALSE, replace.assign = FALSE, width.cutoff = 60)
  gbutton("Open", container = g2, handler = function(h, ...) {
    s = gfile("Open R Source")
    if (!is.na(s)) {
      svalue(txt) = readLines(s)
      tag(txt, "src.file") = s
      tooltip(txt) = s
    }
  })
  gbutton("Convert", container = g2, handler = function(h, ...) {
    tidy.opt = tag(txt, "tidy.opt")
    src = svalue(txt)
    Encoding(src) = 'UTF-8'
    src = unlist(strsplit(src, '\n', fixed = TRUE))
    text.tidy = tidy.source(text = src,
                            keep.comment = tidy.opt$keep.comment,
                            keep.blank.line = tidy.opt$keep.blank.line,
                            keep.space = tidy.opt$keep.space,
                            replace.assign = tidy.opt$replace.assign,
                            width.cutoff = tidy.opt$width.cutoff,
                            output = FALSE)$text.tidy
    Encoding(text.tidy) = 'UTF-8'
    enc = options(encoding = "UTF-8")
    svalue(txt) = text.tidy
    options(enc)
  })
  gbutton("Save", container = g2, handler = function(h, ...) {
    s = tag(txt, "src.file")
    if (is.null(s)) s = gfile("Save", type = "save")
    if (!is.na(s)) writeLines(svalue(txt), s)
  })
  gbutton("Save-as", container = g2, handler = function(h, ...) {
    s = gfile("Save as", type = "save")
    if (!is.na(s)) writeLines(svalue(txt), s)
  })
  gbutton("Execute", container = g2, handler = function(h, ...) {
    src = svalue(txt)
    Encoding(src) = 'UTF-8'
    zz = textConnection(src)
    source(zz, max.deparse.length = Inf, echo = TRUE)
    close(zz)
  })
  if (guiToolkit == "RGtk2") {
    gbutton("Select-font", container = g2, handler = function(h, ...) {
      w = gwindow("Font Specification", )
      g = ggroup(horizontal = FALSE, container = w)
      tbl = glayout(container = g, expand = TRUE, spacing = 0)
      ft = tag(txt, "font.attr")
      tbl[1, 1, expand = TRUE] = (gf.size <- gframe("Size", container = tbl))
      r.size = gradio(tag(txt)$tags$sizes, 
                      which(ft["size"] == tag(txt)$tags$sizes), 
                      horizontal = TRUE, container = gf.size)
      tbl[2, 1, expand = TRUE] = (gf.weight <- gframe("Weight", container = tbl))
      r.weight = gradio(tag(txt)$tags$weight, 
                        which(ft["weight"] == tag(txt)$tags$weight), 
                        horizontal = TRUE, container = gf.weight)
      g1 = ggroup(container = g)
      b.ok = gbutton("OK", container = g1, handler = function(h, ...) {
        ft = c(family = "monospace", size = svalue(r.size), weight = svalue(r.weight))
        font(txt) = ft
        tag(txt, "font.attr") = ft
        dispose(w)
      })
      b.cancel = gbutton("Cancel", container = g1, handler = function(h, ...) {
        dispose(w)
      })
    })
  }
  gbutton("Preferences", container = g2, handler = function(h, ...) {
    w = gwindow("Preferences")
    g = ggroup(horizontal = FALSE, container = w)
    tbl = glayout(container = g, expand = TRUE, spacing = 0)
    tidy.opt = tag(txt, "tidy.opt")
    tbl[1, 1, expand = TRUE] = (gf.kc <- gframe("Keep Comments?", container = tbl))
    r.kc = gradio(c("TRUE", "FALSE"), 
                  ifelse(as.logical(tidy.opt$keep.comment), 1, 2), 
                  horizontal = TRUE, container = gf.kc)
    tbl[2, 1, expand = TRUE] = (gf.kb <- gframe("Keep Blank Lines?", container = tbl))
    r.kb = gradio(c("TRUE", "FALSE"), 
                  ifelse(as.logical(tidy.opt$keep.blank.line), 1, 2), 
                  horizontal = TRUE, container = gf.kb)
    tbl[3, 1, expand = TRUE] = (gf.ks <- gframe("Keep Spaces?", container = tbl))
    r.ks = gradio(c("TRUE", "FALSE"), 
                  ifelse(as.logical(tidy.opt$keep.space), 1, 2),
                  horizontal = TRUE, container = gf.ks)
    tbl[4, 1, expand = TRUE] = (gf.ra <- gframe("Replace '=' with '<-' in assigning operations?",
                                                container = tbl))
    r.ra = gradio(c("TRUE", "FALSE"), 
                  ifelse(as.logical(tidy.opt$replace.assign), 1, 2),
                  horizontal = TRUE, container = gf.ra)
    tbl[5, 1, expand = TRUE] = (gf.wi <- gframe("Text Width", container = tbl))
    r.wi = gedit(as.character(tidy.opt$width.cutoff), container = gf.wi,
                 coerce.with = as.integer)
    g1 = ggroup(container = g)
    b.ok = gbutton("OK", container = g1, handler = function(h, ...) {
      if (is.na(svalue(r.wi))) {
        gmessage("Please input an integer for the text width!", "Error", icon = "error")
      } else {
        tag(txt, "tidy.opt") = list(keep.comment = as.logical(svalue(r.kc)),
                                    keep.blank.line = as.logical(svalue(r.kb)),
                                    keep.space = as.logical(svalue(r.ks)),
                                    replace.assign = as.logical(svalue(r.ra)),
                                    width.cutoff = svalue(r.wi))
        dispose(w)
      }
    })
    b.cancel = gbutton("Cancel", container = g1, handler = function(h, ...) {
      dispose(w)
    })
  })
  invisible(txt)
}
