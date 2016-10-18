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
  expect_error(rope$charAt(-5), '`index` must be a positive integer')
})

test_that('getChars method', {
  garage <- string('toyota')
  expect_equal(garage$getChars(1, 2), c('t', 'o'))
  expect_equal(garage$getChars(5, 6), c('t', 'a'))
  expect_error(garage$getChars(-1, 1), '`begin` and `end`')
  expect_error(garage$getChars(2, -2), '`begin` and `end`')
  expect_error(garage$getChars(3, 2), '`begin` must be less')
  expect_error(garage$getChars(1, 10), '`end` out of bounds')
})

test_that('getBytes method', {
  snack <- string('cheese')
})

test_that('compareTo(ignoreCase = FALSE) method', {
  quality <- string('threadcount')
  expect_equal(quality$compareTo('dhreadcount'), utf8ToInt('t') - utf8ToInt('d'))
  expect_equal(quality$compareTo('threadcount'), 0)
  turtles <- string('shreadcount')
  expect_equal(quality$compareTo(turtles), 1)
  expect_error(quality$compareTo(NULL), 'Argument `another` must be of class string')
})

test_that('compareTo(ignoreCase = TRUE) method', {
  detective <- string('conan')
  expect_equal(detective$compareTo('cONaN', ignoreCase = TRUE), 0)
  expect_lt(detective$compareTo('fffff', ignoreCase = TRUE), 0)
})

test_that('equals method', {
  one <- string('one')

  expect_false(one$equals(string('ONE')))
  expect_false(one$equals('one'))
  expect_false(one$equals(NULL))
  expect_false(one$equals(1))

  expect_true(one$equals(string('one')))
})

test_that('contentEquals method', {
  two <- string('two')

  expect_false(two$contentEquals('TWO'))

  expect_true(two$contentEquals('two'))

  expect_error(two$contentEquals(2), 'Could not coerce `object` to string')
})

test_that('equalsIgnoreCase', {
  igor <- string('case')

  expect_true(igor$equalsIgnoreCase(string('CASE')))
})

test_that('substring method', {
  below <- string('par')
  expect_equal(below$substring(1, 2)$toString(), 'pa')
})

test_that('regionMatches(ignoreCase = FALSE) method', {
  area51 <- string('aliens')

  expect_error(area51$regionMatches(3, "ALIENS", 30, 0), '`other` must be of class string')

  expect_false(area51$regionMatches(-3030, string('ens'), 0, 3))
  expect_false(area51$regionMatches(4, string('ens'), -3030, 3))
  expect_false(area51$regionMatches(5, string('ali'), 0, 3))
  expect_false(area51$regionMatches(2, string('ali'), 0, 4))

  expect_true(area51$regionMatches(2, string('li'), 1, 2))
  expect_true(area51$regionMatches(4, string('mittens'), 5, 3))
})

test_that('regionMatches(ignoreCase = TRUE) method', {
  fire <- string('kindling')
  expect_true(fire$regionMatches(3, string('xxNDLxx'), 3, 3, ignoreCase = TRUE))
})

test_that('startsWith method', {
  bronze <- string('age')

  expect_error(bronze$startsWith(10000), '`prefix` must be of class string or character')

  expect_true(bronze$startsWith(''))
  expect_true(bronze$startsWith(string('')))
  expect_true(bronze$startsWith('age'))
  expect_true(bronze$startsWith(string('age')))

  expect_true(bronze$startsWith('ag'))
  expect_true(bronze$startsWith('ge', 2))
})

test_that('endsWith method', {
  total <- string('war')

  expect_error(total$endsWith(3030))

  expect_true(total$endsWith(''))
  expect_true(total$endsWith(string('')))
  expect_true(total$endsWith('war'))
  expect_true(total$endsWith(string('war')))

  expect_true(total$endsWith('r'))
  expect_true(total$endsWith('ar'))
})
