#' ---
#' title: "Development"
#' author: "Corrado Lanera"
#' date: "`r date()`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     keep_md: true
#' ---
#'
#' ```{r setup, include=FALSE}
#' knitr::opts_chunk$set(
#'   echo = TRUE,
#'   eval = FALSE
#' )
#' ```
#'








#'
#' Development history
#' ====================================================================
#' List here all the command executed during development of the package
#'



# usethis::use_test("<function_name>")
# usethis::use_r("<function_name>")


# ...
# ...
# ...








#'
#' Check cycles
#' ====================================================================
#'
#' Before pushes
#' --------------------------------------------------------------------
#'
usethis::use_tidy_description()
devtools::check_man()
spelling::spell_check_package()
spelling::update_wordlist()




#'
#' Before pull requests
#' --------------------------------------------------------------------
#'
lintr::lint_package()

# The following calls run into your (interactive) session
# Use the corresponding RStudio button under the "Build"
# tab to execute them mantaining free your console
# (and running them in a non-interactive session)
devtools::test()
devtools::check()

#'
#' > Update the `NEWS.md` file
#'
usethis::use_version()
