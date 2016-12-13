library(rstore)
context("save.obj")

teardown = function(){
  forget.obj("obj0")
  forget.obj("obj1")
}

test_that("save.obj saves objects", {
  expect_equal(length(list.obj()), 0)

  save.obj(123, "obj0", "rev0")
  expect_equal(load.obj("obj0", "rev0"), 123)

  save.obj(list(456), "obj0", "rev1")
  expect_equal(load.obj("obj0", "rev1"), list(456))

  save.obj(data.frame(x=456), "obj1", "rev0")
  expect_equal(load.obj("obj1", "rev0"), data.frame(x=456))

  forget.obj("obj0")
  forget.obj("obj1")
})

test_that("value of save.obj is character vector of object name and revision name", {
  expect_equal(save.obj(123, "obj0", "rev0"), c("obj0", "rev0"))
  expect_equal(save.obj(list(456), "obj0", "rev1"), c("obj0", "rev1"))
  expect_equal(save.obj(data.frame(x=456), "obj1", "rev0"), c("obj1", "rev0"))

  ret = save.obj(123, "obj0", "rev2")
  expect_equal(load.obj(ret[1], ret[2]), 123)
  ret = save.obj(list(456), "obj0", "rev3")
  expect_equal(load.obj(ret[1], ret[2]), list(456))
  ret = save.obj(data.frame(x=456), "obj1", "rev2")
  expect_equal(load.obj(ret[1], ret[2]), data.frame(x=456))
})

teardown()

test_that("revision of saved object automatically assign if rev is NULL", {
  ret = save.obj(1, "obj0")
  expect_equal(load.obj(ret[1], ret[2]), 1)
  ret = save.obj(2, "obj0")
  expect_equal(load.obj(ret[1], ret[2]), 2)
  ret = save.obj(3, "obj0")
  expect_equal(load.obj(ret[1], ret[2]), 3)
  ret = save.obj(4, "obj1")
  expect_equal(load.obj(ret[1], ret[2]), 4)
})

teardown()

test_that("can't overwrite same name with same revision", {
  expect_equal(length(list.obj()), 0)

  expect_equal(obj.exists("obj0", "rev0"), FALSE)
  save.obj(123, "obj0", "rev0")
  expect_equal(obj.exists("obj0", "rev0"), TRUE)

  expect_error(save.obj(list(456), "obj0", "rev0"), "already exist")
  expect_equal(obj.exists("obj0", "rev0"), TRUE)
  expect_equal(load.obj("obj0", "rev0"), 123)

  forget.obj("obj0")
})
