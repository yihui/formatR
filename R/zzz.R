.First.lib <- function(lib, pkg) {
    if (is.null(getOption("guiToolkit"))) {
        if (require("gWidgetsRGtk2"))
            options(guiToolkit = "RGtk2")
        else {
            if (require("gWidgetsrJava"))
                options(guiToolkit = "rJava")
            else {
                if (require("gWidgetstcltk"))
                  options(guiToolkit = "tcltk")
                else {
                  packageStartupMessage("You need to install one of packages 'gWidgetsRGtk2', 'gWidgetsrJava' \n",
                    "    and 'gWidgetstcltk' to create the GUI.\n",
                    "See ?formatR for more infomation.")
                }
            }
        }
    }
    if (interactive())
        formatR()
}
