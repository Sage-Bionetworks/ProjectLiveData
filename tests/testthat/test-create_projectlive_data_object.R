fileview1 <- dplyr::tibble(
  "id" = c("syn1", "syn2", "syn3", "syn4"),
  "name" = c("file1", "file2", "file3", "file4"),
  "projectId" = c("syn10", "syn10", "syn11", "syn12"),
  "createdOn" = c(
    lubridate::dmy(01012022),
    lubridate::dmy(01012021),
    lubridate::dmy(01012020),
    lubridate::dmy(01012019)
  )
)


test_that("create_fileview_proj_filter", {

  expect_null(create_fileview_proj_filter())

  expect_equal(
    create_fileview_proj_filter(project_ids_to_keep = "syn12"),
    "projectId IN ('syn12')"
  )

  expect_equal(
    create_fileview_proj_filter(project_ids_to_remove = "syn12"),
    "projectId NOT IN ('syn12')"
  )

  expect_equal(
    create_fileview_proj_filter(project_ids_to_remove = c("syn11", "syn12")),
    "projectId NOT IN ('syn11', 'syn12')"
  )

  expect_equal(
    create_fileview_proj_filter(project_ids_to_remove = list("syn11", "syn12")),
    "projectId NOT IN ('syn11', 'syn12')"
  )
})

test_that("create_fileview_column_list", {
  expect_equal(
    create_fileview_column_list(),
    c("id", "name", "createdOn", "projectId")
  )

  expect_equal(
    create_fileview_column_list(c("id")),
    c("id", "name", "createdOn", "projectId")
  )

  expect_equal(
    create_fileview_column_list("projectId"),
    c("id", "name", "createdOn", "projectId")
  )

  expect_equal(
    create_fileview_column_list(c("projectId")),
    c("id", "name", "createdOn", "projectId")
  )

  expect_equal(
    create_fileview_column_list(list("projectId")),
    c("id", "name", "createdOn", "projectId")
  )

  expect_equal(
    create_fileview_column_list(list("projectId", "projectId")),
    c("id", "name", "createdOn", "projectId")
  )
})


test_that("get_project_ids_from_fileview", {
  expect_equal(
    get_project_ids_from_fileview(fileview1),
    c("syn10", "syn11", "syn12")
  )
})
