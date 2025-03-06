

test_that("correct confidence intervals - absolute difference", {
  expect_equal(pair_abs_diff_summary_binary(42, 46, 1000, 1200, 0.05)$UCI,
               prop.test(c(46, 42), c(1200, 1000), correct = FALSE)$conf.int[2])
  expect_equal(pair_abs_diff_summary_binary(42, 46, 1000, 1200, 0.05)$LCI,
               prop.test(c(46, 42), c(1200, 1000), correct = FALSE)$conf.int[1])
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.05)$UCI,
               prop.test(c(460, 340), c(1200, 1000), correct = FALSE)$conf.int[2])
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.05)$LCI,
               prop.test(c(460, 340), c(1200, 1000), correct = FALSE)$conf.int[1])
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.1)$UCI,
               prop.test(c(460, 340), c(1200, 1000), conf.level = 0.9, correct = FALSE)$conf.int[2])
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.1)$LCI,
               prop.test(c(460, 340), c(1200, 1000), conf.level = 0.9, correct = FALSE)$conf.int[1])
})

test_that("correct point estimate - absolute difference", {
  expect_equal(pair_abs_diff_summary_binary(42, 46, 1000, 1200, 0.05)$point_est,
               46/1200 - 42/1000)
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.05)$point_est,
               460/12000 - 340/10000)
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.1)$point_est,
               460/12000 - 340/10000)
})

test_that("correct conclusion - absolute difference", {
  expect_equal(pair_abs_diff_summary_binary(42, 46, 1000, 1200, 0.05)$signif,
               "not significant")
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.05)$signif,
               "not significant")
  expect_equal(pair_abs_diff_summary_binary(340, 460, 10000, 12000, 0.1)$signif,
               "significant")
})




test_that("correct confidence intervals - relative difference", {
  expect_equal(pair_rel_diff_summary_binary(42, 46, 1000, 1200, 0.05)$UCI,
               ((46 / 1200) - (42 / 1000)) / (42 / 1000) +
                 qnorm(1 - 0.05/2) *
                 sqrt(1/((42 / 1000) ** 2) * ((46 / 1200) * (1 - (46 / 1200)) / 1200) +
                        ((46 / 1200) ** 2) / ((42 / 1000) ** 4) * ((42 / 1000) * (1 - (42 / 1000)) / 1000)))
  expect_equal(pair_rel_diff_summary_binary(42, 46, 1000, 1200, 0.05)$LCI,
               ((46 / 1200) - (42 / 1000)) / (42 / 1000) -
                 qnorm(1 - 0.05/2) *
                 sqrt(1/((42 / 1000) ** 2) * ((46 / 1200) * (1 - (46 / 1200)) / 1200) +
                        ((46 / 1200) ** 2) / ((42 / 1000) ** 4) * ((42 / 1000) * (1 - (42 / 1000)) / 1000)))
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.05)$UCI,
               ((460 / 12000) - (340 / 10000)) / (340 / 10000) +
                 qnorm(1 - 0.05/2) *
                 sqrt(1/((340 / 10000) ** 2) * ((460 / 12000) * (1 - (460 / 12000)) / 12000) +
                        ((460 / 12000) ** 2) / ((340 / 10000) ** 4) * ((340 / 10000) * (1 - (340 / 10000)) / 10000)))
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.05)$LCI,
               ((460 / 12000) - (340 / 10000)) / (340 / 10000) -
                 qnorm(1 - 0.05/2) *
                 sqrt(1/((340 / 10000) ** 2) * ((460 / 12000) * (1 - (460 / 12000)) / 12000) +
                        ((460 / 12000) ** 2) / ((340 / 10000) ** 4) * ((340 / 10000) * (1 - (340 / 10000)) / 10000)))
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.1)$UCI,
               ((460 / 12000) - (340 / 10000)) / (340 / 10000) +
                 qnorm(1 - 0.1/2) *
                 sqrt(1/((340 / 10000) ** 2) * ((460 / 12000) * (1 - (460 / 12000)) / 12000) +
                        ((460 / 12000) ** 2) / ((340 / 10000) ** 4) * ((340 / 10000) * (1 - (340 / 10000)) / 10000)))
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.1)$LCI,
               ((460 / 12000) - (340 / 10000)) / (340 / 10000) -
                 qnorm(1 - 0.1/2) *
                 sqrt(1/((340 / 10000) ** 2) * ((460 / 12000) * (1 - (460 / 12000)) / 12000) +
                        ((460 / 12000) ** 2) / ((340 / 10000) ** 4) * ((340 / 10000) * (1 - (340 / 10000)) / 10000)))
})

test_that("correct point estimate - relative difference", {
  expect_equal(pair_rel_diff_summary_binary(42, 46, 1000, 1200, 0.05)$point_est,
               ((46 / 1200) - (42 / 1000)) / (42 / 1000))
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.05)$point_est,
               ((460 / 12000) - (340 / 10000)) / (340 / 10000))
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.1)$point_est,
               ((460 / 12000) - (340 / 10000)) / (340 / 10000))
})

test_that("correct conclusion - relative difference", {
  expect_equal(pair_rel_diff_summary_binary(42, 46, 1000, 1200, 0.05)$signif,
               "not significant")
  expect_equal(pair_rel_diff_summary_binary(340, 460, 10000, 12000, 0.05)$signif,
               "not significant")
  expect_equal(pair_rel_diff_summary_binary(325, 460, 10000, 12000, 0.1)$signif,
               "significant")
})
