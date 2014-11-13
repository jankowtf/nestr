##------------------------------------------------------------------------------
context("getNested/basics")
##------------------------------------------------------------------------------

test_that("getNested/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setNested(id = "test", value = TRUE))
  expect_equal(res <- getNested(id = "test"), TRUE)
  
  expect_true(res <- setNested(id = "a/b/c", value = 10, gap = TRUE))
  expect_is(res <- getNested(id = "a"), "environment")
  expect_is(res <- getNested(id = "a/b"), "environment")
  expect_equal(res <- getNested(id = "a/b/c"), 10)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getNested/default")
##------------------------------------------------------------------------------

test_that("getNested/default", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_equal(getNested(id = "test", default = NA), NA)
  expect_equal(res <- getNested(id = "a/b/c", default = NA), NA)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getNested/where")
##------------------------------------------------------------------------------

test_that("getNested/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setNested(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getNested(id = "a/b/c", where = where), 10)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setNested(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getNested(id = "a/b/c", where = where), 10)
  
  on.exit(setwd(wd_0))
})

