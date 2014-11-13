##------------------------------------------------------------------------------
context("fromJson/data frame/simple/from file/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/data frame/simple/from file/parent frame", {
  
  rm(list = ls(all.names = TRUE), envir = environment())
  jsoncars <- jsonlite::toJSON(mtcars, pretty=TRUE)
#   cat(jsoncars)
  input <- jsoncars

  expect_is(res <- fromJson(input = input), "environment")
  expect_true(all(colnames(input) %in% ls(res)))

  expect_is(am, "integer")
  expect_true(length(am) > 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/data frame/simple/from URL/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/data frame/simple/from URL/parent frame", {
  
  rm(list = ls(all.names = TRUE), envir = environment())
  input <- "https://api.github.com/users/hadley/orgs"
  expect_is(res <- fromJson(input = input), "environment")
  expect_true(all(c("avatar_url", "events_url", "id", "login", 
    "members_url", "public_members_url", "repos_url", "url") %in% ls(res)))

  expect_is(avatar_url, "character")
  expect_true(length(avatar_url) > 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/data frame/simple/from URL/custom envir")
##------------------------------------------------------------------------------

test_that(desc="fromJson/data frame/simple/from URL/custom envir", {
  
  input <- "https://api.github.com/users/hadley/orgs"
  expect_is(where <- fromJson(input = input, where = new.env()), "environment")
  expect_true(all(c("avatar_url", "events_url", "id", "login", 
    "members_url", "public_members_url", "repos_url", "url") %in% ls(where)))

  expect_is(where$avatar_url, "character")
  expect_true(length(where$avatar_url) > 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/data frame/nested/from URL/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/data frame/nested/from URL/parent frame", {
  
  rm(list = ls(all.names = TRUE), envir = environment())
  input <- "https://api.github.com/users/hadley/repos"
  tmp <- jsonlite::fromJSON(input)
#   colnames(tmp)
  expect_is(res <- fromJson(input = input), "environment")
  expect_true(all(colnames(tmp) %in% ls(res)))

  expect_is(id, "integer")
  expect_true(length(id) > 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/data frame/nested but flattened/from URL/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/data frame/nested but flattened/from URL/parent frame", {
  
  rm(list = ls(all.names = TRUE), envir = environment())
  input <- "https://api.github.com/users/hadley/repos"
  tmp <- jsonlite::fromJSON(input, flatten = TRUE)
#   colnames(tmp)
  expect_is(res <- fromJson(input = input, flatten = TRUE), "environment")
  expect_true(all(colnames(tmp) %in% ls(res)))

  expect_is(id, "integer")
  expect_true(length(id) > 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/list/simple nesting/from URL/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/list/simple nesting/from URL/parent frame", {
  
  rm(list = ls(all.names = TRUE), envir = environment())
  input <- "https://api.github.com/users/hadley/orgs"
  expect_is(res <- fromJson(input = input, simplifyDataFrame = FALSE), "environment")
  expect_true(all(paste0("[", 1:6, "]") %in% ls(res)))

  expect_is(environment()[["[1]"]]$avatar_url, "character")
  expect_true(length(environment()[["[1]"]]$avatar_url) == 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/list/complex nesting/from URL/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/list/complex nesting/from URL/parent frame", {
  
  rm(list = ls(all.names = TRUE), envir = environment())
  input <- "https://api.github.com/users/hadley/repos"
  tmp <- jsonlite::fromJSON(input)
#   colnames(tmp)
  expect_is(res <- fromJson(input = input, simplifyDataFrame = FALSE), "environment")
  expect_true(all(paste0("[", 1:30, "]") %in% ls(res)))

# ls(environment()[["[1]"]]$archive_url)
# tmp <- jsonlite::fromJSON(input, simplifyDataFrame = FALSE)
# tmp[[1]]$owner
  expect_is(environment()[["[1]"]]$owner, "environment")
  expect_is(environment()[["[1]"]]$owner$type, "character")
  
})

##------------------------------------------------------------------------------
context("fromJson/list/complex nesting but flattened/from URL/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromJson/list/complex nesting but flattened/from URL/parent frame", {
  
  skip("this combination is actually not possible")
  rm(list = ls(all.names = TRUE), envir = environment())
  input <- "https://api.github.com/users/hadley/repos"
  tmp_1 <- jsonlite::fromJSON(input, flatten = TRUE, simplifyDataFrame = FALSE)
  tmp_2 <- jsonlite::fromJSON(input, simplifyDataFrame = FALSE)
  identical(tmp_1, tmp_2)
  ## --> identical
  
})

##------------------------------------------------------------------------------
context("fromJson/list/simple nesting/from URL/custom envir")
##------------------------------------------------------------------------------

test_that(desc="fromJson/list/simple nesting/from URL/custom envir", {
  
  input <- "https://api.github.com/users/hadley/orgs"
  expect_is(where <- fromJson(input = input, where = new.env(), 
    simplifyDataFrame = FALSE), "environment")
  expect_true(all(paste0("[", 1:3, "]") %in% ls(where)))

  expect_is(where[["[1]"]]$avatar_url, "character")
  expect_true(length(where[["[1]"]]$avatar_url) == 1)
  
})

##------------------------------------------------------------------------------
context("fromJson/list/complex nesting/from URL/custom envir")
##------------------------------------------------------------------------------

test_that(desc="fromJson/list/complex nesting/from URL/custom envir", {
  
  input <- "https://api.github.com/users/hadley/repos"
  expect_is(where <- fromJson(input = input, where = new.env(), 
    simplifyDataFrame = FALSE), "environment")
  expect_true(all(paste0("[", 1:30, "]") %in% ls(where)))

  expect_is(where[["[1]"]]$owner, "environment")
  expect_is(where[["[1]"]]$owner$type, "character")
  
})
