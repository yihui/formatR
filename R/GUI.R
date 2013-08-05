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
#' @param guiToolkit the GUI toolkit to use
#' @return the text widget is returned
#' @note By default, the interface is based on GTK+ (R package \pkg{RGtk2}), but
#'   other options (\pkg{tcltk}, \pkg{rJava} and \bold{Qt}) are possible too.
#'   See the examples below. Note the \samp{Font} button is only for the GTK+
#'   interface.
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
#' }
tidy.gui = function(guiToolkit = 'RGtk2') {
  options(guiToolkit = guiToolkit)
  library(paste('gWidgets', guiToolkit, sep = ''), character.only = TRUE)

  g = ggroup(horizontal = FALSE, container = gwindow('Tidy R Source'))
  g1 = ggroup(container = g, expand = TRUE)
  g2 = ggroup(container = g)

  txt = gtext(container = g1, wrap = FALSE,
              font.attr = c(family = 'monospace', size = 'medium'), expand = TRUE)
  font.attr = c(style = 'normal', size = 'medium', weight = 'light')
  gbutton('Open', container = g2, handler = function(h, ...) {
    s = gfile('Open R Source')
    if (!is.na(s)) {
      svalue(txt) = readLines(s)
      tag(txt, 'src.file') = s
      tooltip(txt) = s
    }
  })
  gbutton('Convert', container = g2, handler = function(h, ...) {
    src = svalue(txt)
    Encoding(src) = 'UTF-8'
    text.tidy = tidy.source(text = src, output = FALSE)$text.tidy
    Encoding(text.tidy) = 'UTF-8'
    enc = options(encoding = 'UTF-8')
    svalue(txt) = text.tidy
    options(enc)
  })
  gbutton('Save', container = g2, handler = function(h, ...) {
    s = tag(txt, 'src.file')
    if (is.null(s)) s = gfile('Save', type = 'save')
    if (!is.na(s)) writeLines(svalue(txt), s)
  })
  gbutton('Save-as', container = g2, handler = function(h, ...) {
    s = gfile('Save as', type = 'save')
    if (!is.na(s)) writeLines(svalue(txt), s)
  })
  gbutton('Execute', container = g2, handler = function(h, ...) {
    src = svalue(txt)
    Encoding(src) = 'UTF-8'
    zz = textConnection(src)
    source(zz, max.deparse.length = Inf, echo = TRUE)
    close(zz)
  })
  if (guiToolkit == 'RGtk2') {
    gbutton('Select-font', container = g2, handler = function(h, ...) {
      w = gwindow('Font Specification', )
      g = ggroup(horizontal = FALSE, container = w)
      tbl = glayout(container = g, expand = TRUE, spacing = 0)
      tbl[1, 1, expand = TRUE] = (gf.size <- gframe('Size', container = tbl))
      r.size = gradio(gui.sizes, which(font.attr['size'] == gui.sizes),
                      horizontal = TRUE, container = gf.size)
      tbl[2, 1, expand = TRUE] = (gf.weight <- gframe('Weight', container = tbl))
      r.weight = gradio(gui.weights, which(font.attr['weight'] == gui.weights),
                        horizontal = TRUE, container = gf.weight)
      g1 = ggroup(container = g)
      b.ok = gbutton('OK', container = g1, handler = function(h, ...) {
        font.attr <<- c(family = 'monospace', size = svalue(r.size), weight = svalue(r.weight))
        font(txt) = font.attr
        dispose(w)
      })
      b.cancel = gbutton('Cancel', container = g1, handler = function(h, ...) {
        dispose(w)
      })
    })
  }
  gbutton('Preferences', container = g2, handler = function(h, ...) {
    w = gwindow('Preferences')
    g = ggroup(horizontal = FALSE, container = w)
    tbl = glayout(container = g, expand = TRUE, spacing = 0)
    tbl[1, 1, expand = TRUE] = (gf.kc <- gframe('Keep Comments?'))
    r.kc = gradio(c('TRUE', 'FALSE'), ifelse(getOption('keep.comment', TRUE), 1, 2),
                  horizontal = TRUE, container = gf.kc)
    tbl[2, 1, expand = TRUE] = (gf.kb <- gframe('Keep Blank Lines?'))
    r.kb = gradio(c('TRUE', 'FALSE'), ifelse(getOption('keep.blank.line', TRUE), 1, 2),
                  horizontal = TRUE, container = gf.kb)
    tbl[4, 1, expand = TRUE] = (gf.ra <- gframe("Replace '=' with '<-' in assigning operations?"))
    r.ra = gradio(c('TRUE', 'FALSE'), ifelse(getOption('replace.assign', FALSE), 1, 2),
                  horizontal = TRUE, container = gf.ra)
    tbl[5, 1, expand = TRUE] = (gf.wi <- gframe('Text Width'))
    r.wi = gedit(getOption('width'), container = gf.wi, coerce.with = as.integer)
    tbl[6, 1, expand = TRUE] = (gf.rs <- gframe('Number of Spaces to Indent'))
    r.rs = gedit(getOption('reindent.spaces', 4), container = gf.rs, coerce.with = as.integer)
    g1 = ggroup(container = g)
    b.ok = gbutton('OK', container = g1, handler = function(h, ...) {
      if (is.na(svalue(r.wi))) {
        gmessage('Please input an integer for the text width!', 'Error', icon = 'error')
      } else {
        options(keep.comment = as.logical(svalue(r.kc)),
                keep.blank.line = as.logical(svalue(r.kb)),
                replace.assign = as.logical(svalue(r.ra)),
                reindent.spaces = svalue(r.rs),
                width = svalue(r.wi))
        dispose(w)
      }
    })
    b.cancel = gbutton('Cancel', container = g1, handler = function(h, ...) {
      dispose(w)
    })
  })
  invisible(txt)
}

gui.styles = c('normal', 'oblique', 'italic')
gui.sizes = c('xx-large', 'x-large', 'large', 'medium', 'small', 'x-small', 'xx-small')
gui.weights = c('ultralight', 'heavy', 'light', 'book', 'semibold', 'ultraheavy',
                'bold', 'thin', 'ultrabold', 'medium')
