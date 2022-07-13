#' Get Project Manifest Lists
#'
#' Gets the manifest synapse ids associated with a list of projects, and
#' separates the results into successes and errors.
#'
#' @param project_ids A list of synapse ID for the project to get the manifests
#'  from.
#' @param fileview_id A synapse ID for the fileview to use.
#' @param synapse_token A synapse PAT with download priveledges.
get_project_manifest_lists <- function(
    project_ids,
    fileview_id,
    synapse_token
) {
  manifest_attempts <- map_get_project_manifest_list(
    project_ids,
    fileview_id,
    synapse_token
  )

  manifest_successes <- manifest_attempts %>%
    purrr::discard(., purrr::map_lgl(., ~class(.x) == "try-error"))

  manifest_failures <- manifest_attempts %>%
    purrr::keep(., purrr::map_lgl(., ~class(.x) == "try-error"))

  if (length(manifest_failures) == 0) manifest_failures <- NULL

  return(list(
    "list" = manifest_successes,
    "errors"  = manifest_failures
  ))
}


#' Map Get Project Manifest List
#'
#' Attempts to get the manifest synapse ids associated with a list of projects.
#'
#' @param project_ids A list of synapse ID for the project to get the manifests
#'  from.
#' @param fileview_id A synapse ID for the fileview to use.
#' @param synapse_token A synapse PAT with download priveledges.
map_get_project_manifest_list <- function(
    project_ids,
    fileview_id,
    synapse_token
) {
  project_ids %>%
    unlist() %>%
    unique() %>%
    purrr::set_names(.) %>%
    purrr::map(~try(get_project_manifest_list(
      project_id = .x,
      fileview_id = fileview_id,
      synapse_token = synapse_token
    )))
}

#' Get Project Manifest List
#'
#' Attempts to get the manifest synapse ids associated with a project, and
#' handle the various error cases.
#'
#' @param project_id A synapse ID for the project to get the manifests from.
#' @param fileview_id A synapse ID for the fileview to use.
#' @param synapse_token A synapse PAT with download priveledges.
get_project_manifest_list <- function(
    project_id,
    fileview_id,
    synapse_token
) {
  manifest_attempt <- try(get_project_manifests(
    project_id,
    fileview_id,
    synapse_token
  ))

  error_occured <- any(
    class(manifest_attempt) == "try-error",
    typeof(manifest_attempt[[1]]) == "externalptr"
  )

  if (error_occured) {
    stop(
      "API error: project_id: ",
      project_id,
      ", fileview_id: ",
      fileview_id
    )
  }

  return(manifest_attempt)
}


#' Get Project Manifests
#'
#' A call to the schamtic API, getting all manifest synapse ids for a project,
#' using thefileview(asset_view) for that project.
#'
#' @param project_id A synapse ID for the project to get the manifests from.
#' @param fileview_id A synapse ID for the fileview to use.
#' @param synapse_token A synapse PAT with download priveledges.
#' @param url A URL to the schematic API.
get_project_manifests <- function(
    project_id,
    fileview_id,
    synapse_token,
    url = "http://localhost:3001/v1/storage/project/manifests"
) {
  request <- httr::GET(
    url,
    query = list(
      project_id  = project_id,
      asset_view  = fileview_id,
      input_token = synapse_token
    )
  )

  httr::content(request, as = "parsed")
}
