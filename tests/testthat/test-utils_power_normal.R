test_that("power difference is correct - normal", {
  expect_equal(power_diff_norm(1000, 0.5, 4, 5, 2, 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, (5 - 4) / 2, 0.05)$power - 0.8,
               tolerance = '3e')
  expect_equal(power_diff_norm(2000, 0.5, 4, 5, 2, 0.05, 0.8),
               pwr.t2n.test(2000 * 0.5, 2000 * 0.5, (5 - 4) / 2, 0.05)$power - 0.8,
               tolerance = '3e')
  expect_equal(power_diff_norm(1000, 0.3, 4, 5, 2, 0.05, 0.8),
               pwr.t2n.test(1000 * 0.3, 1000 * 0.7, (5 - 4) / 2, 0.05)$power - 0.8,
               tolerance = '3e')
  expect_equal(power_diff_norm(1000, 0.5, 3, 5, 2, 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, (5 - 3) / 2, 0.05)$power - 0.8,
               tolerance = '3e')
  expect_equal(power_diff_norm(1000, 0.5, 4, 6, 2, 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, (6 - 4) / 2, 0.05)$power - 0.8,
               tolerance = '3e')
  expect_equal(power_diff_norm(1000, 0.5, 4, 5, 2, 0.1, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, (5 - 4) / 2, 0.1)$power - 0.8,
               tolerance = '3e')
  expect_equal(power_diff_norm(1000, 0.5, 4, 5, 2, 0.05, 0.7),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, (5 - 4) / 2, 0.05)$power - 0.7,
               tolerance = '3e')
})


test_that("power solver is correct - normal, sample size calculation", {
  # Absolute differences
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 1, 'abs', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 1, 'abs', 0.05, 0.8) * 0.5,
                             1/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.3, 4, 2, 1, 'abs', 0.05, 0.8) * 0.3,
                             solve_power_norm_n(0.3, 4, 2, 1, 'abs', 0.05, 0.8) * 0.7,
                             1/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 5, 2, 1, 'abs', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 5, 2, 1, 'abs', 0.05, 0.8) * 0.5,
                             1/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 3, 1, 'abs', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 3, 1, 'abs', 0.05, 0.8) * 0.5,
                             1/3, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 0.5, 'abs', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 0.5, 'abs', 0.05, 0.8) * 0.5,
                             0.5/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 1, 'abs', 0.1, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 1, 'abs', 0.1, 0.8) * 0.5,
                             1/2, 0.1)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 1, 'abs', 0.05, 0.9) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 1, 'abs', 0.05, 0.9) * 0.5,
                             1/2, 0.05)$power,
               0.9,
               tolerance = '3e')

  # relative differences
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 0.25, 'rel', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 0.25, 'rel', 0.05, 0.8) * 0.5,
                             1/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.3, 4, 2, 0.25, 'rel', 0.05, 0.8) * 0.3,
                             solve_power_norm_n(0.3, 4, 2, 0.25, 'rel', 0.05, 0.8) * 0.7,
                             1/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 5, 2, 0.25, 'rel', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 5, 2, 0.25, 'rel', 0.05, 0.8) * 0.5,
                             1.25/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 3, 0.25, 'rel', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 3, 0.25, 'rel', 0.05, 0.8) * 0.5,
                             1/3, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 0.125, 'rel', 0.05, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 0.125, 'rel', 0.05, 0.8) * 0.5,
                             0.5/2, 0.05)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 0.25, 'rel', 0.1, 0.8) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 0.25, 'rel', 0.1, 0.8) * 0.5,
                             1/2, 0.1)$power,
               0.8,
               tolerance = '3e')
  expect_equal(pwr.t2n.test(solve_power_norm_n(0.5, 4, 2, 0.25, 'rel', 0.05, 0.9) * 0.5,
                             solve_power_norm_n(0.5, 4, 2, 0.25, 'rel', 0.05, 0.9) * 0.5,
                             1/2, 0.05)$power,
               0.9,
               tolerance = '3e')
})

test_that("power solver is correct - normal, min detectible effect calculation", {
  # Absolute differences
  expect_equal(solve_power_norm_mde(0.5, 4, 2, 1000, 'abs', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.8)$d * 2,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.3, 4, 2, 1000, 'abs', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.3, 1000 * 0.3, d = NULL, 0.05, 0.8)$d * 2,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 5, 2, 1000, 'abs', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.8)$d * 2,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 3, 1000, 'abs', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.8)$d * 3,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 2, 2000, 'abs', 0.05, 0.8),
               pwr.t2n.test(2000 * 0.5, 2000 * 0.5, d = NULL, 0.05, 0.8)$d * 2,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 2, 1000, 'abs', 0.1, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.1, 0.8)$d * 2,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 2, 1000, 'abs', 0.05, 0.9),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.9)$d * 2,
               tolerance = '3e')

  # relative differences
  expect_equal(solve_power_norm_mde(0.5, 4, 2, 1000, 'rel', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.8)$d * 2 / 4,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.3, 4, 2, 1000, 'rel', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.3, 1000 * 0.3, d = NULL, 0.05, 0.8)$d * 2 / 4,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 5, 2, 1000, 'rel', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.8)$d * 2 / 5,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 3, 1000, 'rel', 0.05, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.8)$d * 3 / 4,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 2, 2000, 'rel', 0.05, 0.8),
               pwr.t2n.test(2000 * 0.5, 2000 * 0.5, d = NULL, 0.05, 0.8)$d * 2 / 4,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 2, 1000, 'rel', 0.1, 0.8),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.1, 0.8)$d * 2 / 4,
               tolerance = '3e')

  expect_equal(solve_power_norm_mde(0.5, 4, 2, 1000, 'rel', 0.05, 0.9),
               pwr.t2n.test(1000 * 0.5, 1000 * 0.5, d = NULL, 0.05, 0.9)$d * 2 / 4,
               tolerance = '3e')
})
