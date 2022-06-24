obj <- create_projectlive_data_object(
  fileview_id = "syn20446927",
  auth_token = .GlobalEnv$AUTH_TOKEN,
  upload_to_synapse = T,
  synapse_destination_id = "syn32204059"
)
print(obj$get_project_manifest_errors)
print(obj$synapse_get_manifest_errors)
