
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

test_that("it uses the most recent row, not the first, for upl/lpl", {
  # Simulates a real ptd_spc output where a single-point first rebase phase
  # produces NA limits for that row, while the current (last) rows have valid
  # limits. The target column is constant (as ptd_spc stores it) — only
  # upl/lpl are NA early on.  get_assurance_type must use tail(), not [1],
  # matching get_variation_type.
  spc <- data.frame(
    upl    = c(NA,  NA,  3),
    lpl    = c(NA,  NA,  1),
    target = c(0.5, 0.5, 0.5) # constant target; 0.5 < lpl → PASS for "increase"
  )

  expect_equal(
    get_assurance_type(spc, "increase"),
    "PASS_TARG"
  )
})
