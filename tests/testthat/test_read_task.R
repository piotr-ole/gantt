test_that("test column types", {
    proper_types <- list(start = 'Date', end = 'Date', task = 'factor', type = 'factor',
                         stage = 'factor', control = 'integer', value = 'integer')
    file_path <- system.file("extdata", "example_task_1.csv", package = "gantt")
    task <- read_task(file_path)
    expect_equal(lapply(task, class), proper_types)
})