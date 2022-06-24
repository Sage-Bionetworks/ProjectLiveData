test_that("create_projectlive_data_object", {
  obj <- create_projectlive_data_object(
    fileview_id = "syn20446927",
    project_ids_to_keep = "syn20834712",
    upload_to_synapse = T,
    synapse_destination_id = "syn32204059",
    auth_token = AUTH_TOKEN
  )
  expect_named(
    obj,
    c("projectlive_data_object", "get_project_manifest_errors", "synapse_get_manifest_errors")
  )
  expect_named(obj$projectlive_data_object)
  expect_null(obj$get_project_manifest_errors)
  expect_null(obj$synapse_get_manifest_errors)
})
