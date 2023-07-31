box::use(
  tt = testthat,
)

box::use(
  app / logic / swissknife / sklang[...],
  app / logic / swissknife / skwrangle[...],
)

tt$test_that("%//% returns RHS if LHS is <type>(0)", {
  x <- NULL %in% "x"
  tt$expect_equal(x %//% "OK", "OK")
})

tt$test_that("%na?% returns RHS if LHS is NA", {
  x <- NA
  tt$expect_equal(x %na?% "OK", "OK")
})

tt$test_that("%nan?% returns RHS if LHS is NA", {
  x <- NaN
  tt$expect_equal(x %nan?% "OK", "OK")
})

tt$test_that("%p100k% returns ratio of RHS and LHS per 100 000", {
  x <- 1
  y <- 100
  tt$expect_equal(x %per100k% y, 1000)
})
