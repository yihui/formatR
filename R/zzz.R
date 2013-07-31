.onLoad = function(lib, pkg) {
  if (interactive() && !is.null(getOption('guiToolkit')))
    try(tidy.gui(getOption('guiToolkit')))
}
