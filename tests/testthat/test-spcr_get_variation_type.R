
test_that("it returns correct string when last point is common cause", {
  spc <- data.frame(
    point_type = c("special_cause_improvement", "common_cause"),
    relative_to_mean = 0
  )

  expect_equal(
    spcr_get_variation_type(spc, "neutral"),
    "CC"
  )
})

test_that("it returns correct strings when improvement direction is neutral", {
  spc <- data.frame(
    point_type = c("common_cause", "special_cause_neutral"),
    relative_to_mean = -1
  )

  expect_equal(
    spcr_get_variation_type(spc, "neutral"),
    "SC_LO_NEUTRAL"
  )

  spc <- data.frame(
    point_type = c("common_cause", "special_cause_neutral"),
    relative_to_mean = 1
  )

  expect_equal(
    spcr_get_variation_type(spc, "neutral"),
    "SC_HI_NEUTRAL"
  )
})

test_that("it returns correct strings when improvement direction is increase", {
  spc <- data.frame(
    point_type = c("common_cause", "special_cause_concern")
  )

  expect_equal(
    spcr_get_variation_type(spc, "increase"),
    "SC_LO_CON"
  )

  spc <- data.frame(
    point_type = c("common_cause", "special_cause_improvement")
  )

  expect_equal(
    spcr_get_variation_type(spc, "increase"),
    "SC_HI_IMP"
  )
})

test_that("it returns correct strings when improvement direction is decrease", {
  spc <- data.frame(
    point_type = c("common_cause", "special_cause_concern")
  )

  expect_equal(
    spcr_get_variation_type(spc, "decrease"),
    "SC_HI_CON"
  )

  spc <- data.frame(
    point_type = c("common_cause", "special_cause_improvement")
  )

  expect_equal(
    spcr_get_variation_type(spc, "decrease"),
    "SC_LO_IMP"
  )
})
