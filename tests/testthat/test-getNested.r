##------------------------------------------------------------------------------
context("getNested/basics")
##------------------------------------------------------------------------------

test_that("getNested/basics", {
  
  expect_true(setNested(id = "test", value = TRUE))
  expect_equal(getNested(id = "test"), TRUE)
  
  expect_true(setNested(id = "a/b/c", value = 10))
  expect_is(res <- getNested(id = "a"), "environment")
  expect_is(res <- getNested(id = "a/b"), "environment")
  expect_equal(res <- getNested(id = "a/b/c"), 10)
  
  expect_equal(getNested(id = "a/b/c/d"), NULL)
  
})

##------------------------------------------------------------------------------
context("getNested/default")
##------------------------------------------------------------------------------

test_that("getNested/default", {
  
  expect_true(setNested(id = "test", value = TRUE))
  expect_equal(getNested(id = "test", default = NA), TRUE)
  expect_equal(getNested(id = "z", default = NA), NA)
  expect_equal(getNested(id = "z/a/b", default = NA), NA)
  
  expect_equal(getNested(id = "test", default = "nope"), TRUE)
  expect_equal(getNested(id = "z", default = "nope"), "nope")
  expect_equal(getNested(id = "z/a/b", default = "nope"), "nope")
  
})

##------------------------------------------------------------------------------
context("getNested/where")
##------------------------------------------------------------------------------

test_that("getNested/where", {

  where <- new.env()
  expect_true(res <- setNested(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getNested(id = "a/b/c", where = where), 10)
  
})

