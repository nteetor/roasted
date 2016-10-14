context('String object')

test_that('charAt method', {
  rope <- String('twine')
  expect_equal(rope$charAt(1), 't')
  expect_error(
    object = rope$charAt(nchar(rope) + 1),
    regexp = "`index` out of bounds"
  )
})
