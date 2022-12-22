# CHANGES IN formatR VERSION 1.13

- `tidy_source()` supports anonymous functions of the form `\(args) expr` for R >= 4.1.0 now (thanks, @notPlancha, #98).

# CHANGES IN formatR VERSION 1.12

- Added a new argument `pipe` to `tidy_source()`. If `pipe = TRUE`, it will convert the **magrittr** pipe `%>%` to the base R pipe operator `|>`.

- Added a function `tidy_rstudio()` to reformat R code in the RStudio editor.

- Added a function `tidy_pipe()` to substitute `%>%` with `|>` (currently works only in RStudio).

# CHANGES IN formatR VERSION 1.11

## NEW FEATURES

- The right arrow assignment operator `->` is supported now.

- Added a new argument `args.newline` to `formatR::tidy_source()`. When set to `TRUE`, function arguments can start on a new line, e.g.,

  ```r
  shiny::updateSelectizeInput(session, "foo", label = "New Label", selected = c("A",
    "B"), choices = LETTERS, server = TRUE)
  ```
  
  can be reformatted to:
  
  ```r
  shiny::updateSelectizeInput(
    session, "foo", label = "New Label", selected = c("A", "B"),
    choices = LETTERS, server = TRUE
  )
  ```

# CHANGES IN formatR VERSION 1.10

## NEW FEATURES

- Support the new pipe operator `|>` in R 4.1.0.

- The `width.cutoff` argument works more accurately when comments are indented (thanks, @iqis, #92). Previously, `width.cutoff` did not take the indentation or the number of `#` characters in a comment when wrapping it, which may lead to wrapped comment lines that are wider than `width.cutoff`.

# CHANGES IN formatR VERSION 1.9

## NEW FEATURES

- Lines will be wrapped after operators `%>%`, `%T%`, `%$%`, and `%<>%` now (thanks, @g4challenge #54, @jzelner #62, @edlee123 #68).

- The argument `width.cutoff` of `tidy_source()` used to be the lower bound of line widths. Now if you pass a number wrapped in `I()`, it will be treated as the uppper bound, e.g., `tidy_source(width.cutoff = I(60))`. However, please note that the upper bound cannot always be respected, e.g., when the code contains an extremely long string, there is no way to break it into shorter lines automatically (thanks, @krivit @pablo14, #71).

- The value of the argument `width.cutoff` can be specified in the global option `formatR.width` now. By default, the value is still taken from the global option `width` like before.

## BUG FIXES

- When the text in the clipboard on macOS does not have a final EOL, `tidy_source()` fails to read the last line (thanks, @edlee123, #54).

# CHANGES IN formatR VERSION 1.8

## MAJOR CHANGES

- White spaces on blank lines are removed now (thanks, @nylander, #88).

- This package requires R >= 3.2.3 now.

# CHANGES IN formatR VERSION 1.7

## MAJOR CHANGES

- `tidy_source()` will try to preserve the character encoding of the `text` argument in the output.

# CHANGES IN formatR VERSION 1.6

## NEW FEATURES

- Added a new argument `wrap` to `tidy_source()` so that users can choose not to wrap comments via `tidy_source(..., wrap = FALSE)`, or set the global option `options(formatR.wrap = FALSE)` (thanks, @YongLiedu, #73).

# CHANGES IN formatR VERSION 1.5

## NEW FEATURES

- added a new function `tidy_file()` to format specified R scripts (thanks, @edlee123, #61)

- usage() was re-implemented by @egnha in #66; the major difference with the previous version is that `width` means the maximum width (if possible) instead of the minimum; it also gained two new arguments, `indent.by.FUN` and `fail`; see ?formatR::usage for details

# CHANGES IN formatR VERSION 1.4

## NEW FEATURES

- `tidy_source()` can preserve line breaks in character strings in source code

## MAJOR CHANGES

- the deprecated functions tidy.source(), tidy.dir(), and tidy.eval() have been removed; use tidy_source(), tidy_dir() and tidy_eval() instead

- comments that begin with `#+` or `#-` are no longer wrapped; such comments are treated as knitr chunk options in `knitr::spin()` (#52)

## BUG FIXES

- `tidy_source()` should not write an extra space to the last line of code (thanks, @mr-karan, #49)

- long strings (> 1000 characters) in source code can be preserved now (thanks, @jholtman, #50)

- `tidy_source()` might move any lines of code starting with `else` back to the previous lines (thanks, @Auburngrads, #51)

# CHANGES IN formatR VERSION 1.3

## NEW FEATURES

- `tidy_source()` can deal with multibyte characters that cannot represented in the system native encoding now (on Windows)

- `usage()` works for functions obtained from `::` or `:::` now, e.g. `usage(formatR::tidy_source)`

# CHANGES IN formatR VERSION 1.2

## MAJOR CHANGES

- the minimal required R version is 3.0.2 now

# CHANGES IN formatR VERSION 1.1

## NEW FEATURES

- added a new argument `output` to usage()

## BUG FIXES

- fixed yihui/knitr#918: when code is NULL, parse() will hang (with a question mark waiting for input)

# CHANGES IN formatR VERSION 1.0

## NEW FEATURES

- added a function tidy_app() to replace tidy.gui() in previous versions: tidy_app() launches a Shiny app in the browser to reformat R code. The gWidgets interface (e.g. GTK+) is no longer supported. See https://yihui.shinyapps.io/formatR/ for a live demo.

## BUG FIXES

- the shebang #! is no longer treated as an R comment (thanks, Mirko Ebert, #36)

## MAJOR CHANGES

- three functions were renamed (from the `foo.bar` style to `foo_bar`): `tidy.source()` (`tidy_source()`), `tidy.dir()` (`tidy_dir()`), and `tidy.eval()` (`tidy_eval()`)

- the arguments of tidy_source() were renamed: `keep.comment` was renamed to `comment`, `keep.blank.line` -> `blank`, `replace.assign` -> `arrow`, `left.brace.newline` -> `brace.newline`, and `reindent.spaces` -> `indent`; similarly, the corresponding global options were also renamed: now you should use `options(formatR.comment)` instead of `options(keep.comment)`, `keep.blank.line` -> `formatR.blank`, and so on; see `?tidy_source` and https://yihui.org/formatR/ for details

## MINOR CHANGES

- the usage() function returns the source code of the usage of a function now; in previous versions, it only returns NULL

- added a new argument 'tidy' in usage() to make it possible not to reformat the usage code

- tidy_source() may not work for R 3.0.0 or 3.0.1 if the code only contains comments due to a bug in base R, which has been fixed; if you use R 3.0, please upgrade to at least R 3.0.2 (R 2.15.x is not affected)

# CHANGES IN formatR VERSION 0.10

## MINOR CHANGES

- the argument 'replace.assign' in tidy.source() will be treated as FALSE if there is no = in the code, because there is no need to replace = in such cases (this slighly improves the performance)

- the PDF vignette was removed, and only the Markdown vignette was kept in this package, which uses the vignette engine knitr::docco_linear; see vignette('formatR', package = 'formatR')

# CHANGES IN formatR VERSION 0.9

## MAJOR CHANGES

- tidy.source() uses utils::getParseData() to identify comments in R code under R 3.0.x, which is much more accurate than the regular expressions in previous versions; users are strongly recommended to try R 3.0.x (#25, #26)

- changed the meaning of the argument 'width' in usage(); see documentation

# CHANGES IN formatR VERSION 0.8

## MAJOR CHANGES

- tidy.source(text = character(0)) returns character(0) instead of ''

- removed the (dark voodoo) functions parse.tidy() and deparse.tidy() as well as the operator "%InLiNe_IdEnTiFiEr%"; they were designed for the pgfSweave package, which has been archived on CRAN for a long time

- the function unmask.source() is no longer exported

# CHANGES IN formatR VERSION 0.7

## BUG FIXES

- backslashes in whole lines of comments can be correctly retained now (e.g. #' \code{1+1}) (thanks, KAPLAN Bernard)

- the font button in tidy.gui() works again (#23) (thanks, Dason Kurkiewicz)

- the option left.brace.newline was buggy; it did not work for empty lines

## MAJOR CHANGES

- the option keep.space in tidy.source() was removed; the spaces before comments will not be faithfully kept

## NEW FEATURES

- the number of spaces for indentation can be specified in tidy.gui()

# CHANGES IN formatR VERSION 0.6

## NEW FEATURES

- the replace.assign argument is much more reliable now; it is based on the codetools package (code analysis) instead of regular expressions (big thanks to Kohske Takahashi)

- replace.assign also works when keep.comment=FALSE; in previous versions, replace.assign=TRUE only applies to keep.comment=TRUE

- tidy.source() gained a new argument 'left.brace.newline'; when set to TRUE, the left curly brace { will be moved to a new line (#18) (thanks, Jared Lander)

## MAJOR CHANGES

- the 'text.tidy' component in the results of tidy.source() is a character vector of code blocks instead of code lines now, e.g. in previous versions, the result may be c('if (TRUE) {', '1', '}') (vector of length 3), but now it becomes 'if (TRUE) {\n1\n}'; each element of 'text.tidy' contains a minimal complete code block

- potential dependency on the parser package has been removed (replaced by the codetools package); this also makes it more robust to use Unicode characters in R code now, see issue #13 for example

- roxygen comments (#') will not be reflowed; this gives us control over which comments to be reflowed (sometimes we do not want comments to be wrapped and we can write them in the special roxygen comments)

## MINOR CHANGES

- the results of tidy.source() (a list) only contain text.tidy and text.mask now; begin.comment and end.comment were removed since they were not used anywhere

# CHANGES IN formatR VERSION 0.5

## MAJOR CHANGES

- the dependency on the parser package was removed because it was orphaned on CRAN; this affects two features: replace = with <- (the 'replace.assign' option in tidy.source()) and the identification of inline comments; tidy.source() will still work in most cases, but please keep in mind that (1) 'replace.assign=TRUE' will not be entirely reliable without parser (so use with extreme caution if you do not have parser installed) (2) if you want to write # in a character string, you must use double quotes, e.g. "here is a #" will be fine whereas 'here is a #' is not; if you want to use quotes in comments, please always use single quotes, e.g. # 'single quotes' (inline comments that contain double quotes will be dropped); if the parser package is available in your system (e.g. you installed it from the archived source on CRAN), everything will be the same as before

- the default value for 'envir' in tidy.eval() was changed from globalenv() to parent.frame()

## MINOR CHANGES

- \\t will no longer be replaced with \t when keep.space=TRUE because it is dangerous to do so; see #17 for an example

# CHANGES IN formatR VERSION 0.4

## NEW FEATURES

- a new argument 'reindent.spaces' for tidy.source() to reindent the code with a specified number of spaces (e.g. 2)

- comments will be reflowed as a whole block when possible (instead of being wrapped line by line); thanks, Paul Johnson

## MAJOR CHANGES

- when a comment block is reflowed, the second and following lines will not be indented

- the default value of the 'width.cutoff' argument in tidy.source() is getOption('width') now; in the past it was 75% of that width which was less intuitive

- part of the documentation of tidy.source() has been moved to https://github.com/yihui/formatR/wiki/

- internally the comments are preserved by putting them in an expression invisible("# comments"); in past versions comments were retained in assignments; this change should not affect end users

## BUG FIXES

- fixed #16: \\ in comments are preserved correctly now

# CHANGES IN formatR VERSION 0.3-4

## MINOR CHANGES

- slight tweaks to the vignette (stopped Sweave from adding \usepackage{Sweave} which introduces ae by default)

- fixed the error message in tidy.source(), pointing users to the wiki page on GitHub (thanks, Gabor Grothendieck)

# CHANGES IN formatR VERSION 0.3-3

## MINOR CHANGES

- functions unmask.source(), parse.tidy(), deparse.tidy() and the operator %InLiNe_IdEnTiFiEr% were marked as `internal' in documentation

- the vignette is processed by the knitr package

- fixed a buglet in usage() so it can process functions with dots correctly

# CHANGES IN formatR VERSION 0.3-2

## MINOR CHANGES

- the parser package is imported (in previous versions formatR depends on parser); thanks, Romain Francois

# CHANGES IN formatR VERSION 0.3-1

## SIGNIFICANT CHANGES

- the function formatR() was renamed to tidy.gui() which is a more meaningful name since it is used to create a GUI

## NEW FEATURES

- usage() will tell if the function is S3

- a wiki is set up as the manual for formatR: https://github.com/yihui/formatR/wiki

- tidy.eval() can evaluate the code in a specified environment now; see the 'envir' argument

## MINOR CHANGES

- keep.blank.line is TRUE by default now (was FALSE in previous versions), i.e. blank lines are preserved by default

# CHANGES IN formatR VERSION 0.2-4

## NEW FEATURES

- a new function tidy.eval(): evaluate R code and insert the output masked in comments (following ##)

- the empty lines before 'else' will be removed even if keep.blank.line = TRUE; it is ill-advised to use blank lines among incomplete code chunks, e.g. if (TRUE) {'this is a BAD style of R programming'}

- tidy.source() reports the line number when errors occur, which can help users detect the problem in the R code more quickly (thanks, Hadley Wickham)

# CHANGES IN formatR VERSION 0.2-3

## NEW FEATURES

- 'else ...' will be moved back to the last line so that we will no longer see an 'else' statement in a new line

# CHANGES IN formatR VERSION 0.2-2

## NEW FEATURES

- formatR now uses the parser package to parse inline comments, which can guarantee that these comments will be correctly parsed (no longer uses the 'unsafe' regular expressions to parse comments, so forget about the previous rules of writing comments -- just write comments with an arbitrary number of spaces before # as you wish)

- the use of parser also enabled a new feature: '=' can be replaced with '<-' wherever appropriate (for example, '=' in function arguments will not be replaced; only thoese equal signs which are used to assigning purposes can be replaced)

- long roxygen comments will not be wrapped (i.e. comments begin with #' or ##')

## MINOR CHANGES

- fixed a minor problem in the function usage() (out --> output)

- comments after { will be moved to the next line (in previous versions, these comments will cause errors)

# CHANGES IN formatR VERSION 0.2-1

## MINOR CHANGES

- the escape character '\' in comments of complete lines will be successfully preserved, which is especially useful for tidy.source() to format the roxygen comments since we usually write comments like "##' @author Someone \email{}" but "\e" is not a legal character in R (this will lead to errors in earlier versions of this package)

# CHANGES IN formatR VERSION 0.2-0

## NEW FEATURES

- a new function usage() to print the formatted usage of a function

# CHANGES IN formatR VERSION 0.1-9

## NEW FEATURES

- tidy.source() can wrap long comments into shorter ones now (this only applies to the whole lines of comments; the inline comments will not be wrapped since it is tricky to do so)

## MINOR CHANGES

- '\t' will be parsed to ' ' when 'keep.space' is TRUE in tidy.source() (this might be undesirable, though)

# CHANGES IN formatR VERSION 0.1-8

## NEW FEATURES

- new functions parse.tidy() and deparse.tidy() for the package pgfSweave to help tidy the source code in Sweave

- a new function tidy.dir() to format all the R scripts under a directory

- added a package vignette

# CHANGES IN formatR VERSION 0.1-7

## NEW FEATURES

- full support to multi-byte characters in the formatR() GUI

- a new function unmask.source() to obtain the real source code from the masked source

- a new operator '%InLiNe_IdEnTiFiEr%' designed mainly for pgfSweave (mask the inline comments)

# CHANGES IN formatR VERSION 0.1-6

## NEW FEATURES

- the inline comments will also be preserved in most cases (in earlier versions, only single lines of comments are preserved)

- tidy.source() gained a new argument 'text' to accept a character vector as the source code

- multi-byte characters are partially supported in the formatR() GUI now (full support will come in 0.1-7)

