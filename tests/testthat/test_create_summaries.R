test_that("test summaries not cascade", {
  expected_df <- data.frame(stage = as.factor(c('Planning', 'Development', 'Deployment')),
                            start = as.Date(c('01/01/2020', '20/01/2020', '20/04/2020'), format = '%d/%m/%Y'),
                            end = as.Date(c('20/01/2020', '11/04/2020', '01/05/2020'), format = '%d/%m/%Y'),
                            value = c(7.35, 5.35, 1.35),
                            type = rep('summary', 3), stringsAsFactors = FALSE)
  file_path <- system.file("extdata", "example_task_3.csv", package = "gantt")
  task <- read_task(file_path)
  summary <- create_summaries(task, cascade_summaries = 'None')
  expect_equal(summary, expected_df)
})