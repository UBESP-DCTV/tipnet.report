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

  if (any(what == c(
    "redcap_repeat_instance",
    "durata_degenza",
    "niv_it_tot"
  ))) {
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

  message(func_name)
  tryCatch(
    do.call(func_name, list(x = x, what = what)),
    error = function(e) {
      transform_centervar_default(x, what)
    }
  ) |>
    dplyr::mutate(
      center = forcats::fct_inseq(.data[["center"]]) |>
        forcats::fct_rev()
    )
}

transform_centervar_default <- function(x, what) {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    )
}


transform_centervar_tipo_chir  <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_motivo_post_oper <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_motivo_ric_trauma2 <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_mod_decesso <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_deceduto <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}


transform_centervar_prelievo_organi <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_sede_inf2 <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_diagnosi_inf <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_tipo_inf <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_sepsi <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )
}

transform_centervar_altro_osp  <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )

}

transform_centervar_motivo_ricovero2  <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )

}

transform_centervar_sede_inf2  <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )

}

transform_centervar_diagnosi_inf  <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )

}

transform_centervar_tipo_inf  <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )

}

transform_centervar_sepsi <- function(x, what) {
  checkmate::assert_string(what)

  transform_centervar_branched(
    x,
    what,
    get_branch(what, "var"),
    get_branch(what, "val")
  )

}
transform_centervar_branched <- function(
    x,
    what ,
    branchvar,
    branchvalue
)   {
  checkmate::assert_string(what)

  res <- x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
    )

  if (sum(c(branchvar, branchvar) == "<null>") == 1) {
    stop(stringr::str_c(
      "Only one between branchval and branchvar is NULL.",
      "Both or none of them must be NULL.",
      sep = "\n"
    ))
  }

  if (branchvar != "<null>") {
    res <- res |>
      dplyr::filter(.data[[branchvar]] == branchvalue)
  }

  res
}


get_branch <- function(what, type = c("var", "val")) {
  type <- match.arg(type)

  switch(type,
    "var" = get_branch_var(what),
    "val" = get_branch_val(what),
    stop("Strange error, this should never happend!!")
  )
}

get_branch_var <- function(what) {
  dplyr::case_when(
    what == "tipo_chir" ~ "tipologia",
    what == "altro_osp" ~ "provenienza",
    what == "motivo_ricovero2" ~ "tipologia",
    what == "motivo_post_oper" ~ "tipologia",
    what == "motivo_ric_trauma2" ~ "tipologia2",
    what == "mod_decesso" ~ "esito_tip",
    what == "deceduto" ~ "esito_tip",
    what == "prelievo_organi" ~ "esito_tip",
    what == "sede_inf2" ~ "inf_ingresso_tip",
    what == "diagnosi_inf" ~ "inf_ingresso_tip",
    what == "tipo_inf" ~ "inf_ingresso_tip",
    what == "sepsi" ~ "inf_ingresso_tip",
    TRUE ~ "<null>"
  )
}

get_branch_val <- function(what) {
  dplyr::case_when(
    what == "tipo_chir" ~ "chirurgico",
    what == "altro_osp" ~ "altro ospedale",
    what == "motivo_ricovero2" ~ "medico",
    what == "motivo_post_oper" ~ "chirurgico",
    what == "motivo_ric_trauma2" ~ "Trauma",
    what == "mod_decesso" ~ "morto",
    what == "deceduto" ~ "morto",
    what == "prelievo_organi" ~ "morto",
    what == "sede_inf2" ~ "si",
    what == "diagnosi_inf" ~ "si",
    what == "tipo_inf" ~ "si",
    what == "sepsi" ~ "si",
    TRUE ~ "<null>"
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
      smr_pim3 = sum(.data$esito_tip == "morto", na.rm = TRUE) /
        (sum(.data$pim3, na.rm = TRUE)/100)) |>
    pivot_longer(
      dplyr::all_of(c("smr_pim3")),
      names_to = "smr_type",
      values_to = "smr_val"
    )
}

transform_centervar_what_not_one <- function(x, what) {
  checkmate::assert_string(what)

  dplyr::filter(x, .data[[what]] != 1)

}

transform_centervar_redcap_repeat_instance <- function(x, what) {
  transform_centervar_what_not_one(x,what)

}

transform_centervar_durata_degenza <- function(x, what) {
  transform_centervar_what_not_one(x,what)
}

transform_centervar_nonmissing_var <- function(x, what,var) {
  checkmate::assert_string(what)

  dplyr::filter(x,!is.na(.data[[var]]))
}



transform_centervar_niv_it_tot <- function(x, what) {
  transform_centervar_nonmissing_var(x,what,"niv_it")

}

transform_centervar_niv_it<- function(x, what) {
  transform_centervar_nonmissing_var_niv(x,what,"niv_it")

}

transform_centervar_vent_iniz2<- function(x, what) {
  transform_centervar_nonmissing_var_niv(x,what,"niv_it")

}

transform_centervar_nonmissing_var_niv <- function(x, what,var) {
  checkmate::assert_string(what)
  x |> dplyr::filter(!is.na(.data[[var]])) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na))

}


#transform_centervar_motivo_ricovero2 <- function(x, what) {
#  checkmate::assert_string(what)
#  x |>
#    dplyr::mutate(
#      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
#    ) |>
#    dplyr::filter(.data[["tipologia"]] == "medico")

#}



#transform_centervar_default <- function(x, what) {
#  x |>
#    dplyr::mutate(
#      dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
#    )
#  }

