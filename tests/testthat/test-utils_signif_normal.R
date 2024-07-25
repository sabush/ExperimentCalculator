test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("correct confidence intervals - absolute difference", {
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$UCI,
               5 - 4 +
                 qt(1 - 0.05/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$LCI,
               5 - 4 -
                 qt(1 - 0.05/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(2, 5, 2, 2, 500, 500, 0.05)$UCI,
               5 - 2 +
                 qt(1 - 0.05/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(2, 5, 2, 2, 500, 500, 0.05)$LCI,
               5 - 2 -
                 qt(1 - 0.05/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 1, 2, 500, 500, 0.05)$UCI,
               5 - 4 +
                 qt(1 - 0.05/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 1, 2, 500, 500, 0.05)$LCI,
               5 - 4 -
                 qt(1 - 0.05/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (1 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$UCI,
               5 - 4 +
                 qt(1 - 0.1/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$LCI,
               5 - 4 -
                 qt(1 - 0.1/2, 500 + 500 - 2) *
                 sqrt(((500 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (500 + 500 - 2)) * sqrt(1 / 500 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 750, 500, 0.05)$UCI,
               5 - 4 +
                 qt(1 - 0.05/2, 750 + 500 - 2) *
                 sqrt(((750 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (750 + 500 - 2)) * sqrt(1 / 750 + 1 / 500),
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 750, 500, 0.05)$LCI,
               5 - 4 -
                 qt(1 - 0.05/2, 750 + 500 - 2) *
                 sqrt(((750 - 1) * (2 ** 2) + (500 - 1) * (2 ** 2)) / (750 + 500 - 2)) * sqrt(1 / 750 + 1 / 500),
               tolerance = '3e')
})

test_that("correct point estimate - absolute difference", {
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$point_est,
               5 - 4,
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(2, 5, 2, 2, 500, 500, 0.05)$point_est,
               5 - 2,
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 1, 2, 500, 500, 0.05)$point_est,
               5 - 4,
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$point_est,
               5 - 4,
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 750, 500, 0.05)$point_est,
               5 - 4,
               tolerance = '3e')
})

test_that("correct conclusion - absolute difference", {
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$point_est,
               "significant",
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$point_est,
               "significant",
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 4.1, 2, 2, 200, 210, 0.1)$point_est,
               "not significant",
               tolerance = '3e')
  expect_equal(pair_abs_diff_summary_normal(4, 4.1, 2, 2, 200, 210, 0.05)$point_est,
               "not significant",
               tolerance = '3e')
})


test_that("correct confidence intervals - relative difference", {
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$UCI,
               (5 - 4) / 4 +
                 qt(1 - 0.05/2, 1000 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((2 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$LCI,
               (5 - 4) / 4 -
                 qt(1 - 0.05/2, 1000 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((2 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(2, 5, 2, 2, 500, 500, 0.05)$UCI,
               (5 - 2) / 2 +
                 qt(1 - 0.05/2, 1000 - 2) *
                 sqrt(1/(2 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (2 ** 4) * ((2 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(2, 5, 2, 2, 500, 500, 0.05)$LCI,
               (5 - 2) / 2 -
                 qt(1 - 0.05/2, 1000 - 2) *
                 sqrt(1/(2 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (2 ** 4) * ((2 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 1, 2, 500, 500, 0.05)$UCI,
               (5 - 4) / 4 +
                 qt(1 - 0.05/2, 1000 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((1 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 1, 2, 500, 500, 0.05)$LCI,
               (5 - 4) / 4 -
                 qt(1 - 0.05/2, 1000 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((1 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$UCI,
               (5 - 4) / 4 +
                 qt(1 - 0.1/2, 1000 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((2 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$LCI,
               (5 - 4) / 4 -
                 qt(1 - 0.1/2, 1000 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((2 /sqrt(500)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 750, 500, 0.05)$UCI,
               (5 - 4) / 4 +
                 qt(1 - 0.05/2, 1250 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((2 /sqrt(750)) ** 2)),
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 750, 500, 0.05)$LCI,
               (5 - 4) / 4 -
                 qt(1 - 0.05/2, 1250 - 2) *
                 sqrt(1/(4 ** 2) * ((2 /sqrt(500)) ** 2) + (5 ** 2) / (4 ** 4) * ((2 /sqrt(750)) ** 2)),
               tolerance = '3e')
})

test_that("correct point estimate - relative difference", {
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$point_est,
               (5 - 4) / 4,
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(2, 5, 2, 2, 500, 500, 0.05)$point_est,
               (5 - 2) / 2,
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 1, 2, 500, 500, 0.05)$point_est,
               (5 - 4) / 4,
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$point_est,
               (5 - 4) / 4,
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 750, 500, 0.05)$point_est,
               (5 - 4) / 4,
               tolerance = '3e')
})

test_that("correct conclusion - relative difference", {
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.05)$point_est,
               "significant",
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 5, 2, 2, 500, 500, 0.1)$point_est,
               "significant",
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 4.1, 2, 2, 200, 210, 0.1)$point_est,
               "not significant",
               tolerance = '3e')
  expect_equal(pair_rel_diff_summary_normal(4, 4.1, 2, 2, 200, 210, 0.05)$point_est,
               "not significant",
               tolerance = '3e')
})
