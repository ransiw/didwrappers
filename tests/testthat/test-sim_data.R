test_that("sim_data() errors if the length of basetreat is different from the timetreat", {
  expect_error(sim_data(basetreat = c(3,4,5), timetreat = c(10,20), dosage=c(1,1,2), tef=c(2,2,2), basecontrol = NULL, cohort = NULL))
})
