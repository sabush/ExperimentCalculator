test_that("power difference is correct - binary", {
  expect_equal(power_diff_bin(1000, 0.5, 0.3, 0.32, 0.05, 0.8),
               pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32), 1000 * 0.5, 1000 * 0.5, 0.05)$power - 0.8)
  expect_equal(power_diff_bin(1000, 0.4, 0.3, 0.32, 0.05, 0.8),
               pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32), 1000 * 0.4, 1000 * 0.6, 0.05)$power - 0.8)
  expect_equal(power_diff_bin(1000, 0.5, 0.3, 0.34, 0.05, 0.8),
               pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.34), 1000 * 0.5, 1000 * 0.5, 0.05)$power - 0.8)
  expect_equal(power_diff_bin(1000, 0.5, 0.28, 0.32, 0.05, 0.8),
               pwr::pwr.2p2n.test(pwr::ES.h(0.28, 0.32), 1000 * 0.5, 1000 * 0.5, 0.05)$power - 0.8)
  expect_equal(power_diff_bin(1000, 0.5, 0.3, 0.32, 0.1, 0.8),
               pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32), 1000 * 0.5, 1000 * 0.5, 0.1)$power - 0.8)
  expect_equal(power_diff_bin(1000, 0.5, 0.3, 0.32, 0.05, 0.7),
               pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32), 1000 * 0.5, 1000 * 0.5, 0.05)$power - 0.7)
})


# test_that("power solver is correct - binary, sample size calculation", {
#   expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32),
#                                   solve_power_bin_n(0.5, 0.3, 0.02, 'abs', 0.05, 0.8) * 0.5,
#                                   solve_power_bin_n(0.5, 0.3, 0.02, 'abs', 0.05, 0.8) * 0.5, 0.05)$power,
#                0.8,
#                tolerance = '2e')
# })
test_that("power solver is correct - binary, sample size calculation", {
  expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32),
                                  solve_power_bin_n(0.4, 0.3, 0.02, 'abs', 0.05, 0.8) * 0.4,
                                  solve_power_bin_n(0.4, 0.3, 0.02, 'abs', 0.05, 0.8) * 0.6, 0.05)$power,
               0.8,
               tolerance = '2e')
})
# test_that("power solver is correct - binary, sample size calculation", {
#   expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.34),
#                                   solve_power_bin_n(0.5, 0.3, 0.04, 'abs', 0.05, 0.8) * 0.5,
#                                   solve_power_bin_n(0.5, 0.3, 0.04, 'abs', 0.05, 0.8) * 0.5, 0.05)$power,
#                0.8,
#                tolerance = '2e')
# })
# test_that("power solver is correct - binary, sample size calculation", {
#   expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32),
#                                   solve_power_bin_n(0.5, 0.3, 0.02, 'abs', 0.1, 0.8) * 0.5,
#                                   solve_power_bin_n(0.5, 0.3, 0.02, 'abs', 0.1, 0.8) * 0.5, 0.1)$power,
#                0.8,
#                tolerance = '2e')
# })
# test_that("power solver is correct - binary, sample size calculation", {
#   expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.3, 0.32),
#                                   solve_power_bin_n(0.5, 0.3, 0.02, 'abs', 0.05, 0.7) * 0.5,
#                                   solve_power_bin_n(0.5, 0.3, 0.02, 'abs', 0.05, 0.7) * 0.5, 0.05)$power,
#                0.7,
#                tolerance = '2e')
# })
# relative differences
test_that("power solver is correct - binary, sample size calculation", {
  expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.2, 0.22),
                                  solve_power_bin_n(0.5, 0.2, 0.1, 'rel', 0.05, 0.8) * 0.5,
                                  solve_power_bin_n(0.5, 0.2, 0.1, 'rel', 0.05, 0.8) * 0.5, 0.05)$power,
               0.8,
               tolerance = '2e')
})
test_that("power solver is correct - binary, sample size calculation", {
  expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.2, 0.22),
                                  solve_power_bin_n(0.4, 0.2, 0.1, 'rel', 0.05, 0.8) * 0.4,
                                  solve_power_bin_n(0.4, 0.2, 0.1, 'rel', 0.05, 0.8) * 0.6, 0.05)$power,
               0.8,
               tolerance = '2e')
})
# test_that("power solver is correct - binary, sample size calculation", {
#   expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.2, 0.24),
#                                   solve_power_bin_n(0.5, 0.2, 0.2, 'rel', 0.05, 0.8) * 0.5,
#                                   solve_power_bin_n(0.5, 0.2, 0.2, 'rel', 0.05, 0.8) * 0.5, 0.05)$power,
#                0.8,
#                tolerance = '2e')
# })
test_that("power solver is correct - binary, sample size calculation", {
  expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.2, 0.22),
                                  solve_power_bin_n(0.5, 0.2, 0.1, 'rel', 0.1, 0.8) * 0.5,
                                  solve_power_bin_n(0.5, 0.2, 0.1, 'rel', 0.1, 0.8) * 0.5, 0.1)$power,
               0.8,
               tolerance = '2e')
})
# test_that("power solver is correct - binary, sample size calculation", {
#   expect_equal(pwr::pwr.2p2n.test(pwr::ES.h(0.2, 0.22),
#                                   solve_power_bin_n(0.5, 0.2, 0.1, 'rel', 0.05, 0.7) * 0.5,
#                                   solve_power_bin_n(0.5, 0.2, 0.1, 'rel', 0.05, 0.7) * 0.5, 0.05)$power,
#                0.7,
#                tolerance = '2e')
# })


test_that("power solver is correct - binary, min detectible effect calculation", {
  # Absolute differences
  expect_equal(abs(pwr::ES.h(0.2, 0.2 + solve_power_bin_mde(0.5, 0.2, 10000, 'abs', 0.05, 0.8))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 + solve_power_bin_mde(0.3, 0.2, 10000, 'abs', 0.05, 0.8))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.3, 10000 * 0.7, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.3, 0.3 + solve_power_bin_mde(0.5, 0.3, 10000, 'abs', 0.05, 0.8))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 + solve_power_bin_mde(0.5, 0.2, 20000, 'abs', 0.05, 0.8))),
               pwr::pwr.2p2n.test(h = NULL, 20000 * 0.5, 20000 * 0.5, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 + solve_power_bin_mde(0.5, 0.2, 10000, 'abs', 0.1, 0.8))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.1, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 + solve_power_bin_mde(0.5, 0.2, 10000, 'abs', 0.05, 0.7))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.05, 0.7)$h,
               tolerance = '2e')

  # relative differences
  expect_equal(abs(pwr::ES.h(0.2, 0.2 * (1 + solve_power_bin_mde(0.5, 0.2, 10000, 'rel', 0.05, 0.8)))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 * (1 + solve_power_bin_mde(0.3, 0.2, 10000, 'rel', 0.05, 0.8)))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.3, 10000 * 0.7, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.3, 0.3 * (1 + solve_power_bin_mde(0.5, 0.3, 10000, 'rel', 0.05, 0.8)))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 * (1 + solve_power_bin_mde(0.5, 0.2, 20000, 'rel', 0.05, 0.8)))),
               pwr::pwr.2p2n.test(h = NULL, 20000 * 0.5, 20000 * 0.5, 0.05, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 * (1 + solve_power_bin_mde(0.5, 0.2, 10000, 'rel', 0.1, 0.8)))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.1, 0.8)$h,
               tolerance = '2e')

  expect_equal(abs(pwr::ES.h(0.2, 0.2 * (1 + solve_power_bin_mde(0.5, 0.2, 10000, 'rel', 0.05, 0.7)))),
               pwr::pwr.2p2n.test(h = NULL, 10000 * 0.5, 10000 * 0.5, 0.05, 0.7)$h,
               tolerance = '2e')
})
