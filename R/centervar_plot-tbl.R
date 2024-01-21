#' Barplot stratified by center
#'
#' @return a ggplot
#' @export
centervar_plot <- function(.db, what, reported_name) {

  geom_centervar <- if (any(what == c("redcap_repeat_instance","durata_degenza","niv_it_tot"))) {
    function(p) {
      p +
      geom_boxplot(aes(y = .data[[what]]))
    }
  } else if (any(what == "pim")) {
    function(p) {
      p +
        geom_boxplot(
          aes(
            y = .data[["pim_val"]],
            colour = .data[["pim_type"]]
          )
        ) +
        coord_flip()
    }
  } else if (any(what == "smr")) {
      function(p) {
        p +
          geom_text(
            aes(
              x = .data[["smr_val"]],
              y = .data[["smr_type"]],
              label = .data$center,colour=.data[["smr_type"]]
            ),
            position = "jitter"
          )
        }
  } else if (any(what == c("tipo_chir", "altro_osp","motivo_post_oper",
                           "motivo_ricovero2","motivo_ric_trauma2","vent_iniz2","niv_it"))){
    function(p) {
      p +
        geom_bar(
          aes(fill = .data[[what]]),
          position =  position_dodge2(reverse=TRUE)
        )
    }
  }
  else {
    function(p) {
      p +
        geom_bar(
          aes(fill = .data[[what]]),
          position =  position_dodge2(reverse=TRUE)
        )
    }
  }

  .db |>
    transform_centervar(what = what) |>
    ggplot(aes(x = .data$center)) |>
    geom_centervar() +
    coord_flip() +
    labs(
      x = "Center",
      y = "Counts",
      fill = stringr::str_to_sentence(reported_name),
      colour = stringr::str_to_sentence(reported_name)
    ) +
    theme(legend.position = "top")
}




#' Table stratified by center
#'
#' @return a data frame with levels' counts for the selected variable,
#'   each center.
#' @export
centervar_tbl <- function(.db, what) {

  if (any(what == c( "redcap_repeat_instance","durata_degenza","niv_it_tot"))) {
    checkmate::assert_string(what)

    .db |>
      dplyr::group_by(.data$center, .add = TRUE) |>
      dplyr::summarise(
        N = n(),
        Mediana = median(.data[[what]], na.rm = TRUE),
        IQR = IQR(.data[[what]], na.rm = TRUE)
      )
  } else if (any(what == "pim")) {
    checkmate::assert_string(what)

    .db |>
      transform_centervar(what = what) |>
      dplyr::group_by(.data$center, .data$pim_type, .add = TRUE) |>
      dplyr::summarise(
        Mediana = median(.data[["pim_val"]], na.rm = TRUE),
        IQR = IQR(.data[["pim_val"]], na.rm = TRUE)
      ) |>
      dplyr::relocate(dplyr::all_of(c("center", "pim_type"))) |>
      dplyr::arrange(.data[["center"]], .data[["pim_type"]])
  } else if (any(what == "smr")) {
    .db |>
      transform_centervar(what = what)
    #    dplyr::relocate(dplyr::all_of(c("center", "smr_type"))) |>
    #   dplyr::arrange(.data[["center"]], .data[["smr_type"]])
  } else if (any(what == c("tipo_chir", "altro_osp","motivo_post_oper",
                           "motivo_ricovero2","motivo_ric_trauma2","vent_iniz2","niv_it"))) {
    .db |>
      transform_centervar(what = what) |>
      dplyr::group_by(
        across(c(.data$center, dplyr::all_of(what))),
        .add = TRUE
      ) |>
      dplyr::summarise(Somma = dplyr::n())
  }
  else {
    .db |>
      transform_centervar(what = what) |>
      dplyr::group_by(
        across(c(.data$center, dplyr::all_of(what))),
        .add = TRUE
      ) |>
      dplyr::summarise(Somma = dplyr::n())
  }

}





transform_centervar <- function(x, what) {

  func_name <- glue::glue("transform_centervar_{what[[1]]}")

  tryCatch(
    do.call(func_name, list(x = x, what = what)),
    error = function(e) transform_centervar_default(x, what)
  ) |>
    dplyr::mutate(
      center = forcats::fct_inseq(.data[["center"]]) |>
        forcats::fct_rev()
    )
}



transform_centervar_pim <- function(x, what) {
  checkmate::assert_string(what)

  x |>
    dplyr::select(
      "center", "age_class", "gender", "tipologia", "ingresso_dt",
      dplyr::matches("pim")
    ) |>
    pivot_longer(
      dplyr::all_of(c("pim3")),
      names_to = "pim_type",
      values_to = "pim_val"
    ) |>
    ggplot2::remove_missing(na.rm = TRUE, vars = "pim_val")
}

transform_centervar_smr <- function(x, what) {
  checkmate::assert_string(what)

  x |>
    dplyr::select(any_of(c(
      "center", "age_class", "gender", "tipologia", "ingresso_dt",
      "esito_tip", "pim3"
    ))) |>
    # dplyr::mutate(
    #   year = as.integer(lubridate::year(.data[["ingresso_dt"]]))
    # ) |>
    dplyr::group_by(.data$center, .add = TRUE) |>
    dplyr::summarise(
      #smr_pim2 = sum(.data$esito_tip=="morto", na.rm = TRUE) /
      # (sum(.data$pim2, na.rm = TRUE)/100),
      smr_pim3 = sum(.data$esito_tip=="morto", na.rm = TRUE) /
        (sum(.data$pim3, na.rm = TRUE)/100)) |>
    pivot_longer(
      dplyr::all_of(c("smr_pim3")),
      names_to = "smr_type",
      values_to = "smr_val"
    )
}

transform_centervar_redcap_repeat_instance <- function(x, what) {
  checkmate::assert_string(what)

  dplyr::filter(x, .data[[what]] != 1)
}

transform_centervar_durata_degenza <- function(x, what) {
  checkmate::assert_string(what)

  dplyr::filter(x, .data[[what]] != 1)
}

transform_centervar_niv_it_tot <- function(x, what) {
    checkmate::assert_string(what)
  checkmate::assert_string(what)

  dplyr::filter(x,!is.na(.data[["niv_it"]]))
}


transform_centervar_tipo_chir <- function(x, what) {
  checkmate::assert_string(what)
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    ) |>
  dplyr::filter(.data[["tipologia"]]== "chirurgico")

}

transform_centervar_altro_osp <- function(x, what) {
  checkmate::assert_string(what)
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    ) |>
    dplyr::filter(.data[["provenienza"]]== "altro ospedale")
}
transform_centervar_motivo_ric_trauma2 <- function(x, what) {
  checkmate::assert_string(what)
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    ) |>
  dplyr::filter(.data[["tipologia2"]]== "Trauma")

}

transform_centervar_motivo_ricovero2 <- function(x, what) {
  checkmate::assert_string(what)
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    ) |>
    dplyr::filter(.data[["tipologia"]]== "medico")

}
transform_centervar_motivo_post_oper <- function(x, what) {
  checkmate::assert_string(what)
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    ) |>
    dplyr::filter(.data[["tipologia"]]== "chirurgico")

}

transform_centervar_default <- function(x, what)
  {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    )
}


