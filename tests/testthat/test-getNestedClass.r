##------------------------------------------------------------------------------
context("getNestedClass/basics")
##------------------------------------------------------------------------------

test_that("getNestedClass/basics", {
  
  expect_true(setNested(id = "test", value = TRUE))
  expect_equal(getNestedClass(id = "test"), "logical")
  
  expect_true(setNested(id = "a/b/c", value = 10))
  expect_equal(getNestedClass(id = "a"), "environment")
  expect_equal(getNestedClass(id = "a/b"), "environment")
  expect_equal(getNestedClass(id = "a/b/c"), "numeric")
  expect_equal(getNestedClass(id = "a/b/c/d"), character())
  
  expect_equal(getNestedClass(id = "c"), character())
  expect_equal(getNestedClass(id = "c/d/e"), character())
  
})

##------------------------------------------------------------------------------
context("getNestedClass/strict")
##------------------------------------------------------------------------------

test_that("getNestedClass/strict/empty ID", {
  
  expect_equal(getNestedClass(id = character()), character())
  expect_warning(expect_equal(
    getNestedClass(id = character(), strict = 1), character()))
  expect_error(getNestedClass(id = character(), strict = 2))

})

test_that("getNestedClass/strict/not-existing", {
  
  expect_equal(getNestedClass(id = "c/d/e"), character())
  expect_warning(expect_equal(getNestedClass(id = "c/d/e", strict = 1), character()))
  expect_error(getNestedClass(id = "c/d/e", strict = 2))

})

##------------------------------------------------------------------------------
context("getNestedClass/where")
##------------------------------------------------------------------------------

test_that("getNestedClass/where", {

  where <- new.env()
  expect_true(setNested(id = "a/b/c", value = 10, where = where))
  expect_equal(getNestedClass(id = "a/b/c", where = where), "numeric")
  
  expect_equal(getNestedClass(id = "c/d/e", where = where), character())
  
})

