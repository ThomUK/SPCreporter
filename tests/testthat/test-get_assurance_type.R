
test_that("it returns correct string when improvement direction is neutral", {
  spc <- data.frame(
    upl = 5,
    lpl = 1,
    target = NA
  )

  expect_equal(
    get_assurance_type(spc, "neutral"),
    "Neutral"
  )
})

test_that("it returns correct string when no target is set", {
  spc <- data.frame(
    upl = 5,
    lpl = 1,
    target = NA
  )

  expect_equal(
    get_assurance_type(spc, "increase"),
    "No target"
  )
})

test_that("it returns correct string in pass/fail conditions", {
  # improvement direction = increase
  spc <- data.frame(
    upl = 3,
    target = 2, # the target is between process limits
    lpl = 1
  )

  expect_equal(
    get_assurance_type(spc, "increase"),
    "RND_TARG"
  )

  # improvement direction = decrease
  spc <- data.frame(
    upl = 3,
    target = 2, # the target is between process limits
    lpl = 1
  )

  expect_equal(
    get_assurance_type(spc, "decrease"),
    "RND_TARG"
  )
})

test_that("it returns correct string in failing conditions", {
  # improvement direction = increase
  spc <- data.frame(
    target = 4, # the target is above process limits
    upl = 3,
    lpl = 1
  )

  expect_equal(
    get_assurance_type(spc, "increase"),
    "FAIL_TARG"
  )


  # improvement direction = decrease
  spc <- data.frame(
    upl = 3,
    lpl = 1,
    target = 0 # the target is below process limits
  )

  expect_equal(
    get_assurance_type(spc, "decrease"),
    "FAIL_TARG"
  )
})

test_that("it returns correct string in passing conditions", {
  # improvement direction = increase
  spc <- data.frame(
    upl = 3,
    lpl = 1,
    target = 0.5 # the target is below process limits
  )

  expect_equal(
    get_assurance_type(spc, "increase"),
    "PASS_TARG"
  )

  # improvement direction = decrease
  spc <- data.frame(
    target = 4, # the target is above process limits
    upl = 3,
    lpl = 1
  )

  expect_equal(
    get_assurance_type(spc, "decrease"),
    "PASS_TARG"
  )
})
