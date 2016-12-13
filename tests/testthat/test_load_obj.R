library(rstore)
context("load.obj")

teardown = function(){
  forget.obj("obj0")
  forget.obj("obj1")
}

test_that("load.obj loads saved objects", {
  expect_equal(length(list.obj()), 0)

  save.obj(123, "obj0", "rev0")
  expect_equal(load.obj("obj0", "rev0"), 123)

  save.obj(list(456), "obj0", "rev1")
  expect_equal(load.obj("obj0", "rev1"), list(456))

  save.obj(data.frame(x=456), "obj1", "rev0")
  expect_equal(load.obj("obj1", "rev0"), data.frame(x=456))
})

teardown()

test_that("load.obj loads newest object if rev is NULL", {
  expect_equal(length(list.obj()), 0)

  save.obj(1, "obj0", "rev0")
  save.obj(2, "obj0", "rev1")
  save.obj(3, "obj0", "rev2")
  expect_equal(load.obj("obj0"), 3)

  save.obj(4, "obj1", "rev2")
  save.obj(5, "obj1", "rev1")
  save.obj(6, "obj1", "rev0")
  expect_equal(load.obj("obj1"), 6)

  forget.obj("obj0")
  forget.obj("obj1")
})

teardown()
