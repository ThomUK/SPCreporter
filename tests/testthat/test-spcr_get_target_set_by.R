test_that("it returns dash if there is no target set", {

  expect_equal(
    spcr_get_target_set_by(NA, "Bobby Target Setter"),
    "-"
  )

})

test_that("it the target setter if there is a target", {

  expect_equal(
    spcr_get_target_set_by(100, "Bobby Target Setter"),
    "Bobby Target Setter"
  )

})
