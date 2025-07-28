test_that("numeric input causes error", {
  expect_error(xtractr::get_media_calls(10))
})
