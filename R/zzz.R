.First.lib <- function(lib, pkg) {
    if (is.null(getOption("guiToolkit")))
        options(guiToolkit = "RGtk2")
    if (interactive())
        formatR()
}
