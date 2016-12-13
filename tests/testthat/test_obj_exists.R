library(rstore)

context("obj.exists")

teardown = function(){
  forget.obj("obj0")
  forget.obj("obj1")
}

test_that("obj.exists return TRUE if object exists in the storage", {
  expect_equal(length(list.obj()), 0)

  expect_equal(obj.exists("obj0"), FALSE)
  expect_equal(obj.exists("obj1"), FALSE)
  expect_equal(obj.exists("obj1", "any"), FALSE)

  ret = save.obj("abc", "obj0")
  expect_equal(obj.exists(ret[1], ret[2]), TRUE)
  expect_equal(obj.exists("obj0", "not_exist_rev"), FALSE)

  ret = save.obj("def", "obj1", "rev1")
  expect_equal(obj.exists("obj1", "rev1"), TRUE)
  expect_equal(obj.exists("obj1", "not_exist_rev"), FALSE)
})

teardown()
