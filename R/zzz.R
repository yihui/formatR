.First.lib <- function(lib, pkg) {
    if (interactive()) {
        if (is.null(getOption("guiToolkit"))) {
            packageStartupMessage("You need to install/load one of packages 'gWidgetsRGtk2',\n",
                                  "   'gWidgetsrJava', 'gWidgetstcltk' or 'gWidgetsQt'\n",
                                  " and set options(guiToolkit = 'XXX') to create the GUI.\n",
                                  "For more information, see ?formatR")

        } else formatR(getOption("guiToolkit"))
    }
}
