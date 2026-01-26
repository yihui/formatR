# formatR

[![R-CMD-check](https://github.com/yihui/formatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/formatR/actions/workflows/R-CMD-check.yaml)
[![CRAN release](https://www.r-pkg.org/badges/version/formatR)](https://cran.r-project.org/package=formatR)

Format R code automatically.

See the package homepage <https://yihui.org/formatR/> for more information. You can also try a live demo at <https://yihui.shinyapps.io/formatR/>. It can reformat your code from

[![R source code before
reformatting](https://db.yihui.org/imgur/lUgtEAb.png)](https://yihui.shinyapps.io/formatR/)

to

[![R source code after
reformatting](https://db.yihui.org/imgur/TBZm0B8.png)](https://yihui.shinyapps.io/formatR/)

## Pre-commit Hook

formatR is available as a pre-commit hook. Note that, to use this, `formatR` and `docopt` must be installed in the system R library; the hook does not install anything itself.

The following arguments are available:

```
Options:    
  --no-comments         Remove comments. 
  --no-blanks           Remove blank lines. 
  --arrow               If given, arrows will not be substituted for = signs. 
  --pipe                If given, pipes will not be changed. 
  --brace-newline       If given, braces will not be put on new lines. 
  --indent=INDENT       Number of indents. [default: 4]    
  --no-wrap             Skip wrapping the text. 
  --width-cutoff=WIDTH  Text width cutoff. [default: 80]    
  --args-newline        If TRUE, arguments are started on a new line after a function call. [default: FALSE]    
```
