auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")

if (auth_token  == "") {
  config <- yaml::read_yaml("configs/config.yaml")
  auth_token = config$auth_token
}

expected_object_names <- c(
  "projectlive_data_object",
  "get_project_manifest_errors",
  "synapse_get_manifest_errors"
)

test_that("create_projectlive_data_object", {
  # obj <- create_projectlive_data_object(
  #   fileview_id = "syn20446927",
  #   project_ids_to_keep = "syn20834712",
  #   upload_to_synapse = TRUE,
  #   synapse_destination_id = "syn32204059",
  #   auth_token = auth_token
  # )
  # expect_named(obj, expected_object_names)
  # expect_named(obj$projectlive_data_object)
  # expect_true(length(obj$projectlive_data_object) > 0)
  # expect_null(obj$get_project_manifest_errors)
  # expect_null(obj$synapse_get_manifest_errors)
  # file.remove("data.RDS")
})
