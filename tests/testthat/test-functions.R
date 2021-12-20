test_that("File opening", {
  data <- open_irga_file("data-tests/example_data.txt")
  expect_equal(dim(data), c(73, 89))
  expect_equal(sum(duplicated(colnames(data))), 0)
  expect_output(open_irga_file("data-tests/example_data.txt", acao = TRUE))
})
