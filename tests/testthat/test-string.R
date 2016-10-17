context('String object')

test_that('string constructor', {
  shuttle <- string('flying')
  expect_equal(class(shuttle), c('string', 'R6'))
  expect_true(is.string(shuttle))
  expect_equal(class(string(shuttle)), c('string', 'R6'))
  expect_true(is.string(string(shuttle)))
})

test_that('toString method', {
  cotton <- string('threads')
  expect_equal(cotton$toString(), 'threads')
  scarf <- string(cotton)
  expect_equal(scarf$toString(), 'threads')
})

test_that('charAt method', {
  rope <- string('twine')
  expect_equal(rope$charAt(1), 't')
  expect_error(rope$charAt(nchar('twine') + 1), '`index` out of bounds')
})

test_that('getChars method', {
  garage <- string('toyota')
  expect_equal(garage$getChars(1, 2), c('t', 'o'))
  expect_equal(garage$getChars(5, 6), c('t', 'a'))
  expect_error(garage$getChars(-1, 1), '`begin` and `end`')
  expect_error(garage$getChars(2, -2), '`begin` and `end`')
  expect_error(garage$getChars(3, 2), '`begin` must be less')
})

test_that('compareTo method', {
  quality <- string('threadcount')
  expect_equal(quality$compareTo('dhreadcount'), utf8ToInt('t') - utf8ToInt('d'))
  expect_equal(quality$compareTo('threadcount'), 0)
  expect_error(quality$compareTo(NULL), 'Argument `another` must be of class string')
})
