test_that("necklace gives correct answer", {
  expect_that(necklace(2, 2), equals(3))
  expect_that(necklace(2, 3), equals(4))
})
