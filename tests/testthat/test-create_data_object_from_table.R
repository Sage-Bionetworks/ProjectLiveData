fileview <- dplyr::tibble(
  "id" = c(
    "syn1001",
    "syn1002",
    "syn1003",
    "syn1004",
    "syn1005",
    "syn1006",
    "syn1007",
    "syn1008"
  ),
  "name" = c(
    "f1001",
    "f1002",
    "f1003",
    "f1004",
    "f1005",
    "f1006",
    "f1007",
    "f1008"
  ),
  "createdOn" = lubridate::dmy(01012021)
)

manifest_table1 <- dplyr::tibble(
  "entityId" = c("syn1001", "syn1002", "syn1003"),
  "annotation1" = c("value1", "value2", "value3"),
  "annotation2" = c(1, 2, 3)
)

formatted_manifest_table1 <- dplyr::tibble(
  "entityId" = c("syn1001", "syn1002", "syn1003"),
  "annotation1" = c("value1", "value2", "value3"),
  "annotation2" = c("1", "2", "3")
)

manifest_table2 <- dplyr::tibble(
  "entityId" = c("syn1004", "syn1005", "syn1006"),
  "annotation1" = c("value1", "value2", "value3"),
  "annotation3" = c(4, 5, 6)
)

manifests_table1 <- dplyr::tibble(
  "dataset_name" = c("dataset1", "dataset2"),
  "dataset_id" = c("syn101", "syn102"),
  "manifest_name" = c(
    "synapse_storage_manifest.csv", "synapse_storage_manifest.csv"
  ),
  "manifest_id" = c("syn1", "syn2"),
  "component_label" = c("Component1", "Component1"),
  "manifest_tbl" = list(manifest_table1, manifest_table2)
)

combined_manifest_table1 <- dplyr::tibble(
  "id" = c("syn1001", "syn1002", "syn1003", "syn1004", "syn1005", "syn1006"),
  "name" = c("f1001", "f1002", "f1003", "f1004", "f1005", "f1006"),
  "createdOn" = lubridate::dmy(01012021),
  "annotation1" = c("value1", "value2", "value3", "value1", "value2", "value3"),
  "annotation2" = c("1", "2", "3", NA, NA, NA),
  "annotation3" = c(NA, NA, NA, "4", "5", "6")
)

manifests_table2 <- dplyr::tibble(
  "dataset_name" = c("dataset1", "dataset2"),
  "dataset_id" = c("syn101", "syn102"),
  "manifest_name" = c(
    "synapse_storage_manifest.csv", "synapse_storage_manifest.csv"
  ),
  "manifest_id" = c("syn1", "syn2"),
  "component_label" = c("Component1", "Component2"),
  "manifest_tbl" = list(manifest_table1, manifest_table2)
)

combined_manifest_table2 <- dplyr::tibble(
  "id" = c("syn1001", "syn1002", "syn1003"),
  "name" = c("f1001", "f1002", "f1003"),
  "createdOn" = lubridate::dmy(01012021),
  "annotation1" = c("value1", "value2", "value3"),
  "annotation2" = c("1", "2", "3")
)

combined_manifest_table3 <- dplyr::tibble(
  "id" = c("syn1004", "syn1005", "syn1006"),
  "name" = c("f1004", "f1005", "f1006"),
  "createdOn" = lubridate::dmy(01012021),
  "annotation1" = c("value1", "value2", "value3"),
  "annotation3" = c("4", "5", "6")
)

all_files_table1 <- dplyr::tibble(
  "id" = c("syn1001", "syn1002", "syn1003", "syn1004", "syn1005", "syn1006"),
  "name" = c("f1001", "f1002", "f1003", "f1004", "f1005", "f1006"),
  "createdOn" = lubridate::dmy(01012021),
)

manifest_list1 <- list("Component1" = combined_manifest_table1)
manifest_list2 <- list(
  "Component1" = combined_manifest_table2,
  "Component2" = combined_manifest_table3
)
manifest_list3 <- list(
  "All Files" = dplyr::tibble(
    "id" = c("syn1001", "syn1002", "syn1003", "syn1004", "syn1005", "syn1006"),
    "name" = c("f1001", "f1002", "f1003", "f1004", "f1005", "f1006"),
    "createdOn" = lubridate::dmy(01012021),
  ),
  "Component1" = combined_manifest_table1
)

manifest_list4 <- list(
  "All Files" = dplyr::tibble(
    "id" = c("syn1001", "syn1002", "syn1003", "syn1004", "syn1005", "syn1006"),
    "name" = c("f1001", "f1002", "f1003", "f1004", "f1005", "f1006"),
    "createdOn" = lubridate::dmy(01012021),
  ),
  "Component1" = combined_manifest_table2,
  "Component2" = combined_manifest_table3
)


test_that("create_data_object_from_table", {
  expect_equal(
    create_data_object_from_table(manifests_table1, fileview),
    manifest_list3
  )

  expect_equal(
    create_data_object_from_table(manifests_table2, fileview),
    manifest_list4
  )
})

test_that("create_manifest_list", {
  expect_equal(
    create_manifest_list(manifests_table1, fileview),
    manifest_list1
  )

  expect_equal(
    create_manifest_list(manifests_table2, fileview),
    list(
      "Component1" = combined_manifest_table2,
      "Component2" = combined_manifest_table3
    )
  )
})

test_that("create_all_files_table", {
  expect_equal(
    create_all_files_table(fileview, manifest_list1),
    all_files_table1
  )
})

test_that("combine_component_tables", {
  expect_equal(
    combine_component_tables(list(manifest_table1, manifest_table2), fileview),
    combined_manifest_table1
  )
})

test_that("format_manifest", {
  expect_equal(format_manifest(manifest_table1), formatted_manifest_table1)
})
