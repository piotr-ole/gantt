test_that("test column types", {
    proper_types <- list(point = 'Date', task = 'factor', type = 'factor',
                         value = 'integer', stage = 'factor', control = 'integer')
    file_path <- system.file("extdata", "example_task_1.csv", package = "gantt")
    task <- read_task(file_path)
    task <- mutate_task(task)
    expect_equal(lapply(task, class), proper_types)
})