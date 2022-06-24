test_that("create_syn_tbl_query", {

  expect_equal(
    create_syn_tbl_query("syn1"),
    "SELECT * FROM syn1 "
  )

  expect_equal(
    create_syn_tbl_query("syn1", c("col1", "col2")),
    "SELECT col1, col2 FROM syn1 "
  )

  expect_equal(
    create_syn_tbl_query(
      "syn1",
      filters = c("id = 'syn2'", "name <> 'file.txt'")
    ),
    "SELECT * FROM syn1 WHERE id = 'syn2' AND name <> 'file.txt'"
  )
})
