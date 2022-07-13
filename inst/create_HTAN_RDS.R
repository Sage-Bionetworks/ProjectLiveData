config <- yaml::read_yaml(
  system.file("config.yaml", package = "ProjectLiveData")
)

obj <- create_projectlive_data_object(
  fileview_id = "syn20446927",
  auth_token = config$auth_token,
  upload_to_synapse = TRUE,
  synapse_destination_id = "syn32206991"
)
print(obj$get_project_manifest_errors)
print(obj$synapse_get_manifest_errors)
