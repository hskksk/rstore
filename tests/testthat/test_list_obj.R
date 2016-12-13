library(rstore)
context("list.obj")

teardown = function(){
  forget.obj("obj0")
  forget.obj("obj1")
}

teardown()

test_that("list.obj lists objects", {
  expect_equal(length(list.obj()), 0)

  save.obj(123, "obj0", "rev0")
  save.obj(456, "obj0", "rev1")
  save.obj(789, "obj1", "rev0")
  save.obj(012, "obj1", "rev1")

  lst = list.obj()
  expect_equal(length(lst), 4)
  expect_true(stringr::str_detect(lst[1], "obj0") & stringr::str_detect(lst[1], "rev0"))
  expect_true(stringr::str_detect(lst[2], "obj0") & stringr::str_detect(lst[2], "rev1"))
  expect_true(stringr::str_detect(lst[3], "obj1") & stringr::str_detect(lst[3], "rev0"))
  expect_true(stringr::str_detect(lst[4], "obj1") & stringr::str_detect(lst[4], "rev1"))
})

teardown()

test_that("list.obj with name argument lists objects with specific name", {
  expect_equal(length(list.obj()), 0)

  save.obj(123, "obj0", "rev0")
  save.obj(456, "obj0", "rev1")
  save.obj(789, "obj1", "rev0")
  save.obj(012, "obj1", "rev1")

  lst = list.obj("obj0")
  expect_equal(length(lst), 2)
  expect_true(stringr::str_detect(lst[1], "obj0") & stringr::str_detect(lst[1], "rev0"))
  expect_true(stringr::str_detect(lst[2], "obj0") & stringr::str_detect(lst[2], "rev1"))
})

teardown()

test_that("list.obj with rev argument lists objects with specific revision", {
  expect_equal(length(list.obj()), 0)

  save.obj(123, "obj0", "rev0")
  save.obj(456, "obj0", "rev1")
  save.obj(789, "obj1", "rev0")
  save.obj(012, "obj1", "rev1")

  lst = list.obj(rev="rev1")
  expect_equal(length(lst), 2)
  expect_true(stringr::str_detect(lst[1], "obj0") & stringr::str_detect(lst[1], "rev1"))
  expect_true(stringr::str_detect(lst[2], "obj1") & stringr::str_detect(lst[2], "rev1"))
})

teardown()
