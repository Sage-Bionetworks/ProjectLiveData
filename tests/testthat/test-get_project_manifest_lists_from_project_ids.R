manifest1 <- list(
  list("syn101", "dataset1"),
  list("syn2", "synapse_storage_manifest.csv"),
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
project_manifest_list2 <- list(manifest1, manifest2)

test_that("get_project_manifest_list_from_project_ids", {
  mockery::stub(
    where = get_project_manifest_list_from_project_ids,
    what = "map_get_project_manifest_list_from_project_ids",
    how = list(
      "syn10" = project_manifest_list1,
      "syn11" = project_manifest_list1,
      "syn12" = try(stop(), silent = T),
      "syn13" = try(stop(), silent = T)
    )
  )

  expect_equal(
    get_project_manifest_list_from_project_ids(
      c("syn10", "syn11", "syn12", "syn13"), "syn2", "token"
    ),
    list(
      "list" = list(
        "syn10" = project_manifest_list1,
        "syn11" = project_manifest_list1
      ),
      "errors" = list(
        "syn12" = try(stop(), silent = T),
        "syn13" = try(stop(), silent = T)
      )
    )
  )

  mockery::stub(
    where = get_project_manifest_list_from_project_ids,
    what = "map_get_project_manifest_list_from_project_ids",
    how = list(
      "syn10" = project_manifest_list1,
      "syn11" = project_manifest_list1
    )
  )

  expect_equal(
    get_project_manifest_list_from_project_ids(
      c("syn10", "syn11"), "syn2", "token"
    ),
    list(
      "list" = list(
        "syn10" = project_manifest_list1,
        "syn11" = project_manifest_list1
      ),
      "errors" = NULL
    )
  )
})


test_that("map_get_project_manifest_list_from_project_ids", {
  mockery::stub(
    where = map_get_project_manifest_list_from_project_ids,
    what = "get_project_manifest_list_from_project_id",
    how = project_manifest_list1
  )

  expect_equal(
    map_get_project_manifest_list_from_project_ids(
      c("syn10", "syn11"), "syn2", "token"
    ),
    list(
      "syn10" = project_manifest_list1,
      "syn11" = project_manifest_list1
    )
  )

  mockery::stub(
    where = map_get_project_manifest_list_from_project_ids,
    what = "get_project_manifest_list_from_project_id",
    how = try(stop(), silent = T)
  )

  expect_equal(
    map_get_project_manifest_list_from_project_ids(
      c("syn10", "syn11"), "syn2", "token"
    ),
    list(
      "syn10" = try(stop(), silent = T),
      "syn11" = try(stop(), silent = T)
    )
  )
})



test_that("get_project_manifest_list_from_project_id", {
  mockery::stub(
    where = get_project_manifest_list_from_project_id,
    what = "get_project_manifests",
    how = project_manifest_list1
  )

  expect_equal(
    get_project_manifest_list_from_project_id("syn1", "syn2", "token"),
    project_manifest_list1
  )

  mockery::stub(
    where = get_project_manifest_list_from_project_id,
    what = "get_project_manifests",
    how = try(stop(), silent = T)
  )

  expect_error(
    get_project_manifest_list_from_project_id("syn1", "syn2", "token"),
    "API error: project_id: syn1, fileview_id: syn2"
  )
})
