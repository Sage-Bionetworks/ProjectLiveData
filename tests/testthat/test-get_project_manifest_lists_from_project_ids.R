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

test_that("get_project_manifest_lists", {
  mockery::stub(
    where = get_project_manifest_lists,
    what = "map_get_project_manifest_list",
    how = list(
      "syn10" = project_manifest_list1,
      "syn11" = project_manifest_list1,
      "syn12" = try(stop(), silent = TRUE),
      "syn13" = try(stop(), silent = TRUE)
    )
  )

  expect_equal(
    get_project_manifest_lists(
      c("syn10", "syn11", "syn12", "syn13"), "syn2", "token"
    ),
    list(
      "list" = list(
        "syn10" = project_manifest_list1,
        "syn11" = project_manifest_list1
      ),
      "errors" = list(
        "syn12" = try(stop(), silent = TRUE),
        "syn13" = try(stop(), silent = TRUE)
      )
    )
  )

  mockery::stub(
    where = get_project_manifest_lists,
    what = "map_get_project_manifest_list",
    how = list(
      "syn10" = project_manifest_list1,
      "syn11" = project_manifest_list1
    )
  )

  expect_equal(
    get_project_manifest_lists(
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


test_that("map_get_project_manifest_list", {
  mockery::stub(
    where = map_get_project_manifest_list,
    what = "get_project_manifest_list",
    how = project_manifest_list1
  )

  expect_equal(
    map_get_project_manifest_list(
      c("syn10", "syn11"), "syn2", "token"
    ),
    list(
      "syn10" = project_manifest_list1,
      "syn11" = project_manifest_list1
    )
  )

  mockery::stub(
    where = map_get_project_manifest_list,
    what = "get_project_manifest_list",
    how = try(stop(), silent = TRUE)
  )

  expect_equal(
    map_get_project_manifest_list(
      c("syn10", "syn11"), "syn2", "token"
    ),
    list(
      "syn10" = try(stop(), silent = TRUE),
      "syn11" = try(stop(), silent = TRUE)
    )
  )
})



test_that("get_project_manifest_list", {
  mockery::stub(
    where = get_project_manifest_list,
    what = "get_project_manifests",
    how = project_manifest_list1
  )

  expect_equal(
    get_project_manifest_list("syn1", "syn2", "token"),
    project_manifest_list1
  )

  mockery::stub(
    where = get_project_manifest_list,
    what = "get_project_manifests",
    how = try(stop(), silent = TRUE)
  )

  expect_error(
    get_project_manifest_list("syn1", "syn2", "token"),
    "API error: project_id: syn1, fileview_id: syn2"
  )
})
