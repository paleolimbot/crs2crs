
test_that("crs_trans_explicit works", {
  expect_identical(
    wk::wk_transform(rep(wk::xy(0, 0), 5), crs_trans_explicit(wk::xy(1:5, 1:5))),
    wk::xy(1:5, 1:5)
  )

  # check with ZM values
  expect_identical(
    wk::wk_transform(
      rep(wk::xyzm(0, 0, 0, 0), 5),
      crs_trans_explicit(wk::xy(1:5, 1:5))
    ),
    wk::xyzm(1:5, 1:5, 0, 0)
  )

  expect_identical(
    wk::wk_transform(
      rep(wk::xyzm(0, 0, 0, 0), 5),
      crs_trans_explicit(wk::xyzm(1:5, 1:5, 1:5, 1:5))
    ),
    wk::xyzm(1:5, 1:5, 1:5, 1:5)
  )
})
