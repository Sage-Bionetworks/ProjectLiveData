manifest1 <- list(
  list("syn101", "dataset1"),
  list("syn1", "synapse_storage_manifest.csv"),
  list("Component1", "Component1")
)

manifest2 <- list(
  list("syn102", "dataset2"),
  list("", "synapse_storage_manifest.csv"),
  list("Component2", "Component2")
)

manifest3 <- list(
  list("syn103", "dataset3"),
  list("", ""),
  list("", "")
)

project_manifest_list1 <- list(manifest1, manifest2, manifest3)
project_manifest_list2 <- list(manifest1)

project_manifest_lists <- list(project_manifest_list1, project_manifest_list2)



intermediate_manifest_table1 <- dplyr::tibble(
  "dataset_name" = c("dataset1", "dataset1"),
  "dataset_id" = c("syn101", "syn101"),
  "manifest_name" = c(
    "synapse_storage_manifest.csv", "synapse_storage_manifest.csv"
  ),
  "manifest_id" = c("syn1", "syn1"),
  "component_label" = c("Component1", "Component1"),
  "manifest_entity" = list(
    list("path" = "file.csv"), list("path" = "file.csv")
  ),
  "manifest_path" = c("file.csv", "file.csv")
)

intermediate_manifest_table2 <- dplyr::tibble(
  "dataset_name" = c("dataset1", "dataset1"),
  "dataset_id" = c("syn101", "syn101"),
  "manifest_name" = c(
    "synapse_storage_manifest.csv", "synapse_storage_manifest.csv"
  ),
  "manifest_id" = c("syn1", "syn1"),
  "component_label" = c("Component1", "Component1"),
  "manifest_entity" = list(list(), list()),
  "manifest_path" = c(NA_character_, NA_character_)
)

manifest_table <- dplyr::tibble(
  "dataset_name" = c("dataset1", "dataset1"),
  "dataset_id" = c("syn101", "syn101"),
  "manifest_name" = c(
    "synapse_storage_manifest.csv", "synapse_storage_manifest.csv"
  ),
  "manifest_id" = c("syn1", "syn1"),
  "component_label" = c("Component1", "Component1"),
  "manifest_tbl" = list(dplyr::tibble(), dplyr::tibble())
)

test_that("create_manifest_table", {
  mockery::stub(
    where = create_manifest_table,
    what = "create_int_manifest_table",
    how = intermediate_manifest_table1
  )

  mockery::stub(
    where = create_manifest_table,
    what = "add_manifest_tbl_column",
    how = manifest_table
  )

  expect_equal(
    create_manifest_table(project_manifest_list1, list()),
    list(
      "table" = manifest_table,
      "errors" = NULL
    )
  )

})

test_that("create_int_manifest_table", {
  mockery::stub(
    where = create_int_manifest_table,
    what = "add_manifest_entity_column",
    how = function(table, synapse_object) {
      dplyr::mutate(
        table,
        "manifest_entity" = list(
          list("path" = "file.csv"), list("path" = "file.csv")
        ),
      )
    }
  )

  expect_equal(
    create_int_manifest_table(project_manifest_lists, list()),
    intermediate_manifest_table1
  )

  mockery::stub(
    where = create_int_manifest_table,
    what = "add_manifest_entity_column",
    how = function(table, synapse_object) {
      dplyr::mutate(
        table,
        "manifest_entity" = list(list(), list()),
      )
    }
  )

  expect_equal(
    create_int_manifest_table(project_manifest_lists, list()),
    intermediate_manifest_table2
  )

})

test_that("create_manifest_table_row", {
  expect_equal(
    create_manifest_table_row(manifest1),
    dplyr::tibble(
      "dataset_name"    = "dataset1",
      "dataset_id"      = "syn101",
      "manifest_name"   = "synapse_storage_manifest.csv",
      "manifest_id"     = "syn1",
      "component_label" = "Component1"
    )
  )

  expect_equal(
    create_manifest_table_row(manifest2),
    dplyr::tibble(
      "dataset_name"    = "dataset2",
      "dataset_id"      = "syn102",
      "manifest_name"   = "synapse_storage_manifest.csv",
      "manifest_id"     = NA_character_,
      "component_label" = "Component2"
    )
  )

  expect_equal(
    create_manifest_table_row(manifest3),
    dplyr::tibble(
      "dataset_name"    = "dataset3",
      "dataset_id"      = "syn103",
      "manifest_name"   = NA_character_,
      "manifest_id"     = NA_character_,
      "component_label" = NA_character_
    )
  )
})

test_that("add_manifest_entity_column", {
  mockery::stub(
    where = add_manifest_entity_column,
    what = "map_synapse_get_entities",
    how = list(list("path" = "manifest.csv"))
  )

  expect_equal(
    add_manifest_entity_column(
      dplyr::tibble(
        "dataset_name"    = "dataset3",
        "dataset_id"      = "syn103",
        "manifest_name"   = NA_character_,
        "manifest_id"     = NA_character_,
        "component_label" = NA_character_
      )
    ),
    dplyr::tibble(
      "dataset_name"    = "dataset3",
      "dataset_id"      = "syn103",
      "manifest_name"   = NA_character_,
      "manifest_id"     = NA_character_,
      "component_label" = NA_character_,
      "manifest_entity" = list(list("path" = "manifest.csv"))
    )

  )
})

test_that("add_manifest_path_column", {
  expect_equal(
    add_manifest_path_column(
      dplyr::tibble(
        "dataset_name"    = "dataset3",
        "dataset_id"      = "syn103",
        "manifest_name"   = NA_character_,
        "manifest_id"     = NA_character_,
        "component_label" = NA_character_,
        "manifest_entity" = list(list("path" = "manifest.csv"))
      )
    ),
    dplyr::tibble(
      "dataset_name"    = "dataset3",
      "dataset_id"      = "syn103",
      "manifest_name"   = NA_character_,
      "manifest_id"     = NA_character_,
      "component_label" = NA_character_,
      "manifest_entity" = list(list("path" = "manifest.csv")),
      "manifest_path"   = "manifest.csv"
    )
  )

  expect_equal(
    add_manifest_path_column(
      dplyr::tibble(
        "dataset_name"    = "dataset3",
        "dataset_id"      = "syn103",
        "manifest_name"   = NA_character_,
        "manifest_id"     = NA_character_,
        "component_label" = NA_character_,
        "manifest_entity" = list(list())
      )
    ),
    dplyr::tibble(
      "dataset_name"    = "dataset3",
      "dataset_id"      = "syn103",
      "manifest_name"   = NA_character_,
      "manifest_id"     = NA_character_,
      "component_label" = NA_character_,
      "manifest_entity" = list(list()),
      "manifest_path"   = NA_character_
    )
  )
})

test_that("add_manifest_tbl_column", {
  mockery::stub(
    where = add_manifest_tbl_column,
    what = "readr::read_csv",
    how = dplyr::tibble()
  )
  expect_equal(
    add_manifest_tbl_column(intermediate_manifest_table1),
    manifest_table
  )
  expect_equal(
    add_manifest_tbl_column(intermediate_manifest_table2),
    dplyr::slice(manifest_table, 0)
  )
})

test_that("create_synapse_error_list", {
  expect_null(
    create_synapse_error_list(intermediate_manifest_table1)
  )
  expect_equal(
    create_synapse_error_list(intermediate_manifest_table2),
    list("syn1" = list(), "syn1" = list())
  )
})
