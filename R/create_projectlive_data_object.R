#' Create Projectlive Data Object
#'
#' The main function for this package. This function will:
#' - download the projects associated fileview
#' - get the associated manifests for each project
#' - download and open each manifest
#' - group up the manifests into one table for each component type
#' - save this object as a .RDS file and upload to synapse
#'
#' @param fileview_id The fileview used as the asset store to get the manifests
#' from
#' @param auth_token A synapse auth token
#' @param project_ids_to_keep A list of project ids to include
#' @param project_ids_to_remove A list of project ids to not include,
#' only used if project_ids_to_keep is NULL
#' @param fileview_columns  A list of columns to include("id", "name"
#' "createdOn" and "projectId", always included.)
#' @param verbose T/F
#' @param upload_to_synapse T/F
#' @param synapse_destination_id A synapse ID, where the file will be stored at
create_projectlive_data_object <- function(
    fileview_id,
    auth_token,
    project_ids_to_keep = NULL,
    project_ids_to_remove = NULL,
    fileview_columns = c(),
    verbose = F,
    upload_to_synapse = F,
    synapse_destination_id = NULL
  ){

  synapse_object <- create_syn_obj(auth_token)
  fileview <- get_fileview_tbl(
    fileview_id,
    synapse_object,
    project_ids_to_keep,
    project_ids_to_remove,
    fileview_columns
  )

  if(is.null(project_ids_to_keep)) {
    project_ids <- get_project_ids_from_fileview(fileview)
  } else {
    project_ids <- project_ids_to_keep
  }

  if(verbose){
    print("Getting manifests for projects:")
    print(project_ids)
  }

  list_object <- get_project_manifest_list_from_project_ids(
    project_ids,
    fileview_id,
    auth_token
  )

  table_object <- create_manifest_table_from_list(list_object$list, synapse_object)
  data_object  <- create_data_object_from_table(table_object$table, fileview)

  if(upload_to_synapse){
    upload_object_to_synapse(
      data_object,
      synapse_destination_id,
      synapse_object
    )
  }

  return(list(
    "projectlive_data_object"     = data_object,
    "get_project_manifest_errors" = list_object$errors,
    "synapse_get_manifest_errors" = table_object$errors
  ))
}

#' Get Fileview Table
#'
#' Returns a dataframe of the Synspae fileview given.
#'
#' @param fileview_id The synapse ID of the fileview
#' @param synapse_object An object returned by create_synapse_login()
#' @param project_ids_to_keep A list of project ids to include
#' @param project_ids_to_remove A list of project ids to not include,
#' only used if project_ids_to_keep is NULL
#' @param columns A list of columns to include("id", "name" and
#' are "createdOn" always included.)
get_fileview_tbl <- function(
    fileview_id,
    synapse_object,
    project_ids_to_keep = NULL,
    project_ids_to_remove = NULL,
    columns = c()
  ){

  columns <- create_fileview_column_list(columns)
  filter <- create_fileview_project_id_filter(
    project_ids_to_keep, project_ids_to_remove
  )
  get_syn_tbl(
    synapse_id = fileview_id,
    syn = synapse_object,
    columns = columns,
    filters = filter
  )
}

#' Create Fileview Column List
#'
#' Creates a list of columns that will be used in the SQL query to get the
#' filewview
#'
#' @param columns A list of columns to include("id", "name" "createdOn" and
#' "projectId", always included.)
create_fileview_column_list <- function(columns  = c()){
  unique(c("id", "name", "createdOn", "projectId", unlist(columns)))
}

#' Create Fileview Project ID Filter
#'
#' Creates a SQL filter for removing projects that are unwanted in the
#' filewview such as test projects.
#'
#' @param project_ids_to_keep A list of project ids to include
#' @param project_ids_to_remove A list of project ids to not include,
#' only used if project_ids_to_keep is NULL
create_fileview_project_id_filter <- function(
    project_ids_to_keep = NULL,
    project_ids_to_remove = NULL
){
  filter <- NULL
  if(!is.null(project_ids_to_keep) || !is.null(project_ids_to_remove)){
    if(!is.null(project_ids_to_keep)){
      ids <- project_ids_to_keep
      glue_string <- "projectId IN ({id_string})"
    } else {
      ids <- project_ids_to_remove
      glue_string <- "projectId NOT IN ({id_string})"
    }
    id_string <- ids %>%
      unique() %>%
      stringr::str_c("'", ., "'") %>%
      stringr::str_c(collapse = ", ")
    filter <-  glue::glue(glue_string)
  }
  return(filter)
}

#' Get Project IDs From Fileview
#'
#' Gets a list of project ids from the fileview, sorted and unique.
#'
#' @param fileview A tibble of the Synapse fileview
get_project_ids_from_fileview <- function(fileview){
  fileview %>%
    dplyr::pull("projectId") %>%
    unique() %>%
    sort()
}

upload_object_to_synapse <- function(
    obj,
    synapse_id,
    synapse_object,
    file_name = "data.RDS"
){
  saveRDS(obj, file = file_name)
  store_file_in_synapse(synapse_object, file_name, synapse_id)
}
