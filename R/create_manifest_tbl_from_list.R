#' Create Manifest Table From List
#'
#' @param project_manifest_list A nested list in this form:
#'list(
#'  "syn10" =  list(
#'    list(
#'       list("syn1", "dataset1"),
#'       list("syn2", "synapse_storage_manifest.csv"),
#'       list("Component1", "Component1")
#'     ),
#'     list(
#'       list("syn3", "dataset1"),
#'       list("syn4", "synapse_storage_manifest.csv"),
#'       list("Component2", "Component2")
#'     )
#'   ),
#'   "syn10" = list(
#'     list(
#'       list("syn5", "dataset1"),
#'       list("syn6", "synapse_storage_manifest.csv"),
#'       list("Component3", "Component3")
#'     ),
#'     list(
#'       list("syn7", "dataset1"),
#'       list("syn8", "synapse_storage_manifest.csv"),
#'       list("Component4", "Component4")
#'     )
#'   )
#' )
#' @param synapse_object An object created by create_syn_obj()
create_manifest_table_from_list <- function(project_manifest_list, synapse_object){
  table <- create_intermediate_manifest_table(
    project_manifest_list, synapse_object
  )
  return(list(
    "table" = create_manifest_table(table),
    "errors" = create_synapse_error_list(table)
  ))
}

#' Create Intermediate Manifest Table
#'
#' @param project_manifest_list A nested list in this form:
#'list(
#'  "syn10" =  list(
#'    list(
#'       list("syn1", "dataset1"),
#'       list("syn2", "synapse_storage_manifest.csv"),
#'       list("Component1", "Component1")
#'     ),
#'     list(
#'       list("syn3", "dataset1"),
#'       list("syn4", "synapse_storage_manifest.csv"),
#'       list("Component2", "Component2")
#'     )
#'   ),
#'   "syn10" = list(
#'     list(
#'       list("syn5", "dataset1"),
#'       list("syn6", "synapse_storage_manifest.csv"),
#'       list("Component3", "Component3")
#'     ),
#'     list(
#'       list("syn7", "dataset1"),
#'       list("syn8", "synapse_storage_manifest.csv"),
#'       list("Component4", "Component4")
#'     )
#'   )
#' )
#' @param synapse_object An object created by create_syn_obj()
create_intermediate_manifest_table <- function(
    project_manifest_list,
    synapse_object
){
  project_manifest_list %>%
    purrr::flatten() %>%
    purrr::map(create_manifest_table_row) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(.data$manifest_id)) %>%
    add_manifest_entity_column(synapse_object) %>%
    add_manifest_path_column()
}


#' Create Manifest Table Row
#'
#' Creates a one row tibble with the data from the manifest as
#' columns, any empty strings ('') are replaced with NAs.
#'
#' @param manifest A nested list in this form:
#' list(
#'   list("syn1", "dataset1"),
#'   list("syn2", "synapse_storage_manifest.csv"),
#'   list("Component1", "Component1")
#' )
#'
create_manifest_table_row <- function(manifest){
  row <-
    dplyr::tibble(
      "dataset_name"    = manifest[[1]][[2]],
      "dataset_id"      = manifest[[1]][[1]],
      "manifest_name"   = manifest[[2]][[2]],
      "manifest_id"     = manifest[[2]][[1]],
      "component_label" = manifest[[3]][[1]]
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~dplyr::if_else(.x == "", NA_character_, .x)
    ))
}

#' Add Manifest Entity Column
#'
#' Maps synapse GET on the "manifest_id" and add the result entity on as
#' "manifest_entity" column.
#'
#' @param table A dataframe with column "manifest_id" that contains synapse
#' file ids for manifests.
#' @param synapse_object object returned by create_synapse_login().
add_manifest_entity_column <- function(table, synapse_object){
  dplyr::mutate(
    table,
    "manifest_entity" = map_synapse_get_entities(
      .data$manifest_id,
      synapse_object
    )
  )
}

#' Add Manifest Path Column
#'
#' Pulls the manifest path from the entities in the "manifest_entity" column,
#' and adds it as the "manifest_path" column.
#'
#' @param table A dataframe with column "manifest_entity" that contains manifest
#' synapse file entities.
add_manifest_path_column <- function(table){
  dplyr::mutate(
    table,
    "manifest_path" = purrr::map_chr(
      .data$manifest_entity,
      get_path_from_synapse_entity
    )
  )
}

#' Get Path From Synapse Entity
#'
#' Gets the path form a Synpase File entity, or returns NA.
#'
#' @param entity A Synsape File entity.
get_path_from_synapse_entity <- function(entity){
  if(is.null(entity$path)) return(NA_character_)
  else return(entity$path)
}

#' Create Manifest Table
#'
#' Returns a table with the manifest table as a column
#'
#' @param table A dataframe with columns "manifest_path", "manifest_id" and
#' "manifest_entity"
create_manifest_table <- function(table){
  table %>%
    dplyr::filter(!is.na(.data$manifest_path)) %>%
    dplyr::mutate("manifest_tbl" = purrr::map(
      .data$manifest_path,
      readr::read_csv,
      col_types = readr::cols()
    )) %>%
    dplyr::select(-c("manifest_entity", "manifest_path"))
}

#' Create Synapse Error List
#'
#' Returns a list of manifetss in the table without a file path
#'
#' @param table A dataframe with columns "manifest_path", "manifest_id" and
#' "manifest_entity"
create_synapse_error_list <- function(table){
  lst <- table %>%
    dplyr::filter(is.na(.data$manifest_path)) %>%
    dplyr::select("manifest_id", "manifest_entity") %>%
    tibble::deframe()
  if(length(lst) == 0) return(NULL)
  else return(lst)
}
