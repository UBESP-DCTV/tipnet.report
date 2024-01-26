lowering_character_columns <- function(db) {
    dplyr::mutate_if(db, is.character, stringr::str_to_lower)
}
