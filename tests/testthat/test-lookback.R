box::use(
  app / view / wrangle / lookback[lookback],
)

test_that("lookback fn works", {
  expect_equal(2 * 2, 4)
})
