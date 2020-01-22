test_that("test summaries data.frame", {
  expected_df <- data.frame(stage = as.factor(c('Planning', 'Development', 'Deployment')),
                            start = as.Date(c('01/01/2020', '20/01/2020', '20/04/2020'), format = '%d/%m/%Y'),
                            end = as.Date(c('20/01/2020', '10/04/2020', '01/05/2020'), format = '%d/%m/%Y'),
                            value = c(6.3, 4.3, 1.3),
                            type = rep('summary', 3), stringsAsFactors = FALSE)
  file_path <- system.file("extdata", "example_task_1.csv", package = "gantt")
  task <- read_task(file_path)
  summary <- create_summaries(task)
  expect_equal(summary, expected_df)
})