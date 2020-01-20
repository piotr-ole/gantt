test_that("calculate middle date", {
    expect_equal(middle_date(as.Date('2020-01-01'), as.Date('2020-01-03')), as.Date('2020-01-02'))
    expect_equal(middle_date(as.Date('2020-01-01'), as.Date('2020-01-04')), as.Date('2020-01-03'))
})
