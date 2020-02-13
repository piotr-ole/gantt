test_that("test column types", {
    proper_types <- list(start = 'Date', end = 'Date', task = 'factor', type = 'factor',
                         stage = 'factor', control = 'integer', people = 'factor', value = 'integer')
    file_path <- system.file("extdata", "example_task_3.csv", package = "gantt")
    task <- read_task(file_path)
    expect_equal(lapply(task, class), proper_types)
})