.onLoad <- function(lib, pkg) {
    if (interactive()) {
        if (!is.null(getOption("guiToolkit")))
            try(formatR(getOption("guiToolkit")))
    }
}
