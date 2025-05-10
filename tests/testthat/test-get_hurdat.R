test_that("basin checks", {
  expect_error(
    get_hurdat2(),
    "A single `basin` must be specified if no `storm_id` is.",
    fixed = TRUE
  )

  expect_error(
    get_hurdat2(basin = "hi"),
    r"[`basin` must be one of "AL" or "EP", not "hi".]",
    fixed = FALSE
  )
  expect_error(
    get_hurdat2(basin = c("AL", "EP", "AL")),
    "A single `basin` must be specified if no `storm_id` is.",
    fixed = TRUE
  )

  expect_no_error(get_hurdat2(basin = "AL"))
  expect_no_error(get_hurdat2(basin = "EP"))
})

test_that("storm_id checks", {
  expect_error(
    get_hurdat2(storm_id = "hi"),
    "`storm_id` must be specified using a valid ATCF ID or the storm's NAME-YEAR.",
    fixed = TRUE
  )

  expect_error(
    get_hurdat2(storm_id = "HARVEY-2017"),
    "`basin` must be specified if using NAME-YEAR `storm_id`.",
    fixed = TRUE
  )

  expect_no_error(get_hurdat2(storm_id = "AL092017"))

  expect_error(
    get_hurdat2(storm_id = "EP090000"),
    r"[Cannot find `storm_id` "EP090000" within the "EP" basin dataset.]",
    fixed = TRUE
  )
})

test_that("storm_id and basin checks", {
  expect_error(
    get_hurdat2("EP", "AL092017"),
    "`basin` specified must match that of the `storm_id`",
    fixed = TRUE
  )

  expect_error(
    get_hurdat2("EP", "EP090000"),
    r"[Cannot find `storm_id` "EP090000" within the "EP" basin dataset.]",
    fixed = TRUE
  )

  expect_error(
    get_hurdat2("EP", "HARVEY-2017"),
    r"[Cannot find `storm_id` "HARVEY-2017" within the "EP" basin dataset.]",
    fixed = TRUE
  )

  expect_no_error(get_hurdat2("AL", "AL092007"))
  expect_no_error(get_hurdat2("AL", "HARVEY-2017"))
})
