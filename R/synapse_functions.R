#' Create Synapse Object
#'
#' Returns a synapse object that has been loged in with the token
#'
#' @param synapse_auth_token A personal access token created from Synapse
create_syn_obj <- function(synapse_auth_token) {
  synapseclient <- reticulate::import("synapseclient")
  syn <- synapseclient$Synapse()
  syn$login(authToken = synapse_auth_token)
  return(syn)
}

#' Get Synapse Table
#'
#' Creates a SQL query for a Synapse table, runs it, reads the result file,
#' and returns a tibble.
#'
#' @param synapse_id An id for a Synapse table
#' @param syn An object returned by create_synapse_login().
#' @param columns A list of columns name in the table
#' @param filters A list of SQL filters such as "id = 'syn2'"
get_syn_tbl <- function(synapse_id, syn, columns = NULL, filters = NULL) {
  table <-
    create_syn_tbl_query(synapse_id, columns, filters) %>%
    syn$tableQuery(includeRowIdAndRowVersion = FALSE) %>%
    purrr::pluck("filepath") %>%
    readr::read_csv(., col_types = readr::cols())
  return(table)
}

#' Create Synapse Table Query
#'
#' Crates a SQL query for a synapse table
#'
#' @param synapse_id An id for a Synapse table
#' @param columns A list of columns name in the table
#' @param filters A list of SQL filters such as "id = 'syn2'"
create_syn_tbl_query <- function(synapse_id, columns = NULL, filters = NULL) {
  if (is.null(columns)) {
    column_string <- "*"
  } else {
    column_string <- stringr::str_c(columns, collapse = ", ")
  }

  if (is.null(filters)) {
    filter_string <- ""
  } else {
    filter_string <- filters %>%
      stringr::str_c(collapse = " AND ") %>%
      stringr::str_c("WHERE ", .)
  }

  glue::glue("SELECT {column_string} FROM {synapse_id} {filter_string}")
}

#' Map Synapse Get Entities
#'
#' @param synapse_ids A list of synapse file ids
#' @param synapse_object An object returned by create_synapse_login().
map_synapse_get_entities <- function(synapse_ids, synapse_object) {
  purrr::map(synapse_ids, synapse_get, synapse_object)
}

#' Synapse Get
#'
#' @param synapse_id A synapse id
#' @param synapse_object An object returned by create_synapse_login().
synapse_get <- function(synapse_id, synapse_object) {
  synapse_object$get(synapse_id)
}

#' Title
#'
#' @param syn An object returned by create_synapse_login().
#' @param file The path to the file to be be strored
#' @param parent_id The synapse id of the folder where the file will be stored
#' @param remove_file T/F
store_file_in_synapse <- function(syn, file, parent_id, remove_file = TRUE) {
  file <- reticulate::import("synapseclient")$File(file, parent_id)
  syn$store(file)
  if (remove_file) rm(file)
}
