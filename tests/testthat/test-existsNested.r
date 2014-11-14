##------------------------------------------------------------------------------
context("existsNested/basics")
##------------------------------------------------------------------------------

test_that("existsNested/basics", {
  
  expect_true(setNested(id = "test", value = TRUE))
  expect_true(existsNested(id = "test"))
  
  expect_true(setNested(id = "a/b/c", value = 10))
  expect_true(existsNested(id = "a"))
  expect_true(existsNested(id = "a/b"))
  expect_true(existsNested(id = "a/b/c"))
  expect_false(existsNested(id = "a/b/c/d"))
  
  expect_false(existsNested(id = "c"))
  expect_false(existsNested(id = "c/d/e"))
  
})

##------------------------------------------------------------------------------
context("existsNested/strict")
##------------------------------------------------------------------------------

test_that("existsNested/strict/empty ID", {
  
  expect_false(existsNested(id = character()))
  expect_warning(expect_false(existsNested(id = character(), strict = 1)))
  expect_error(existsNested(id = character(), strict = 2))

})

test_that("existsNested/strict/not-existing", {
  
  expect_false(existsNested(id = "c/d/e"))
  expect_warning(expect_false(existsNested(id = "c/d/e", strict = 1)))
  expect_error(existsNested(id = "c/d/e", strict = 2))

})

##------------------------------------------------------------------------------
context("existsNested/where")
##------------------------------------------------------------------------------

test_that("existsNested/where", {

  where <- new.env()
  expect_true(setNested(id = "a/b/c", value = 10, where = where))
  expect_true(existsNested(id = "a/b/c", where = where))
  
  expect_false(existsNested(id = "c/d/e", where = where))
  
})

