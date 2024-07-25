test_that("sample ratio mismatch test is correct", {
  expect_equal(create_srm_text(data.frame(sample_size = c(1000, 1000),
                                          expected_proportion = c(0.5, 0.5)), 0.0001),
               HTML('<font color = "#1b680b"><b>There is no sample ratio mismatch error</b></font> (p-value = 1)'))
  expect_equal(create_srm_text(data.frame(sample_size = c(1000, 1200),
                                          expected_proportion = c(0.5, 0.5)), 0.0001),
               HTML('<font color = "#c30000"><b>There is a sample ratio mismatch error</b></font> (p-value = 0.00002) <br>Note that this means that experiment results are more likely to be untrustworthy, and the cause for the mismatch should be investigated.'))
  expect_equal(create_srm_text(data.frame(sample_size = c(1000, 1150),
                                          expected_proportion = c(0.5, 0.5)), 0.0001),
               HTML('<font color = "#1b680b"><b>There is no sample ratio mismatch error</b></font> (p-value = 0.0012)'))
  expect_equal(create_srm_text(data.frame(sample_size = c(1000, 1150),
                                          expected_proportion = c(0.5, 0.5)), 0.01),
               HTML('<font color = "#c30000"><b>There is a sample ratio mismatch error</b></font> (p-value = 0.0012) <br>Note that this means that experiment results are more likely to be untrustworthy, and the cause for the mismatch should be investigated.'))
  expect_equal(create_srm_text(data.frame(sample_size = c(751, 249),
                                          expected_proportion = c(0.75, 0.25)), 0.0001),
               HTML('<font color = "#1b680b"><b>There is no sample ratio mismatch error</b></font> (p-value = 0.94)'))
})

