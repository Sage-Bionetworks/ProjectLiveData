#' Create Data Object From Table
#'
#' Creates a named list with an item per component in the table. For each
#' component, all manifests are formated, combined together, and joined to the
#' fileview. Finally one last item named "All Files" is added to the list that
#' is the filewview with only files that are found in the manifests.
#'
#' @param manifests_table A dataframe, with columns "manifest_tbl", and
#' "component_label
#' @param fileview A dataframe with column "id"
create_data_object_from_table <- function(manifests_table, fileview){
  manifest_list <- create_manifest_list(manifests_table, fileview)
  all_files_tbl <- create_all_files_table(fileview, manifest_list)
  c(list("All Files" = all_files_tbl), manifest_list)
}

#' Create Manifest List
#'
#' Creates a named list of manifest tables per component from a manifest tables.
#'
#' @param manifests_table A dataframe, with columns "manifest_tbl", and
#' "component_label
#' @param fileview A dataframe with column "id"
create_manifest_list <- function(manifests_table, fileview){
  manifests_table %>%
    dplyr::group_by(.data$component_label) %>%
    dplyr::summarise("manifest_tbl" = list(.data$manifest_tbl)) %>%
    tibble::deframe() %>%
    purrr::map(combine_component_tables, fileview)
}

#' Create All Files Table
#'
#' Filters the fileview table for ids that in any of the manifests
#'
#' @param fileview A dataframe with column "id"
#' @param manifest_list A list of manifests, each with a column "id"
create_all_files_table <- function(fileview, manifest_list){
  file_ids <- manifest_list %>%
    purrr::map(dplyr::pull, "id") %>%
    unlist() %>%
    unique()
  dplyr::filter(fileview, .data$id %in% file_ids)
}


#' Combine Component Tables
#'
#' Formats each manifgest in list, combines them, and joins them to the fileview
#'
#' @param manifest_table_list A list of dataframes, with column "entityId"
#' @param fileview A dataframe with column "id"
combine_component_tables <- function(manifest_table_list, fileview){
  manifest_table_list %>%
    purrr::map(format_manifest) %>%
    dplyr::bind_rows() %>%
    dplyr::right_join(fileview, ., by = c("id" = "entityId"))
}


# TODO: use schematic API to find annotation type, and use that to format
# each column

#' Format Manifest
#'
#' Converts all columsn to strings
#'
#' @param manifest A dataframe
format_manifest <- function(manifest){
  dplyr::mutate(manifest, dplyr::across(dplyr::everything(), as.character))
}
