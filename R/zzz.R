.First.lib <- function(lib, pkg) {
    if (interactive()) {
        if (is.null(getOption("guiToolkit"))) {
            pa = FALSE
            for (p in c("RGtk2", "rJava", "tcltk")) {
                if (require(paste("gWidgets", p, sep = ""), character.only = TRUE)) {
                  options(guiToolkit = p)
                  pa = TRUE
                  formatR()
                  break
                }
                else {
                  packageStartupMessage("Package ", paste("gWidgets",
                    p, sep = ""), " is not available...")
                }
            }
            if (!pa)
                packageStartupMessage("You need to install one of packages 'gWidgetsRGtk2', 'gWidgetsrJava' \n",
                  "    and 'gWidgetstcltk' to create the GUI.\n",
                  "For more information, see ?formatR")
        }
        else formatR()
    }
}
