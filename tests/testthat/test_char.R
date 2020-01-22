test_that("test as.character equality", {
  expect_equal(char(1), as.character(1))
  expect_equal(char(list(a = 1, b = 'home', c = 1.23)),
               as.character(list(a = 1, b = 'home', c = 1.23)))
})