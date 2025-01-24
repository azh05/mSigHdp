
test_that("ChainBurnin", {

  load("misc.test.data/1000044.activated.hdp.state.Rdata")
  reg <- new.env()
  load("RunhdpInternal.testdata/test.ChainBurnin.Rdata",
       envir = reg)

  retvalx1 <- Burnin(hdp.state          = hdp.state,
                     seedNumber          = (44 + 3e6),
                     burnin              = 100,
                     cpiter              = 3,
                     burnin.verbosity    = 0,
                     burnin.multiplier   = 2,
                     checkpoint          = FALSE)

  retvalx2 <- Burnin(hdp.state          = hdp.state,
                     seedNumber          = (44 + 3e6),
                     burnin              = 200,
                     cpiter              = 3,
                     burnin.verbosity    = 0,
                     burnin.multiplier   = 1,
                     checkpoint          = FALSE)

  # If we need to regenerate the baseline data:
  # save(retvalx1, file = "RunhdpInternal.testdata/test.ChainBurnin.Rdata")
  # save(retvalx, file = "RunhdpInternal.testdata/test.ChainBurnin.Rdata")

  expect_equal(retvalx1, reg$retvalx1)
  expect_equal(retvalx2, reg$retvalx1)


})
