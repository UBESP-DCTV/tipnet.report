fix_codpat <- function(x) {
  x <- stringr::str_replace_all(x, "[^[:alnum:]]+", "-")

  are_strange <- stringr::str_detect(x, "^[0-9]{6}$")

  if (any(are_strange)) {
    ui_warn("
      Some {ui_field('codpat')} have no separation: {ui_value(x[are_strange])}."
    )

    x[are_strange] <- x[are_strange] %>%
      stringr::str_replace("(^[0-9]{4})([0-9]{2}$)", "\\1-\\2")

    ui_done("They have been fixed as {ui_value(x[are_strange])}")
  }

  x
}
