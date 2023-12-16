test_that("numeric input causes error", {
  expect_error(xtract::get_media_calls(10))
})
