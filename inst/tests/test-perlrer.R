#TODO non ascii tests
string = c('this is Text', 'chr-12', '12343 66544456')
multiline_string = 'This\nis\na\nstring'

alpha = '[A-Za-z]'
alphas = paste0(alpha, '+')

diget = '[0-9]'

digets = paste0(diget, '+')

space = ' '

any = '.*'

test_that('m fails if given other than character vector', {

          expect_that(m(1:10, diget), throws_error())
          expect_that(m(string, 1), throws_error())
          expect_that(m(string, diget, 1), throws_error())
          expect_that(m(factor(string), diget), throws_error())

})

test_that('m with no capture returns a logical', {

          expect_that(m(string, diget), equals(c(FALSE, TRUE, TRUE)))
          expect_that(m(string, digets), equals(c(FALSE, TRUE, TRUE)))
          expect_that(m(string, alpha), equals(c(TRUE, TRUE, FALSE)))

})

test_that('m works with perl style capture variables', {

  expect_that(m(string, paste0(capture(4), any1(not(4)), capture('$1+'))), equals(list(`1`=c('', '',4), `2`=c('', '', 444))))
  # named backreferences not supported in r expect_that(m(string, paste0(named_capture(4, 'four'), '[^4]+', capture('$four+'))), equals(list(four=c('', '',4), `2`=c('', '', 444))))

})

test_that('m with capture returns numbered list', {

          expect_that(m(string, capture(digets)), equals(list(`1`=c('', '12', '12343'))))
          expect_that(m(string, capture(alphas)), equals(list(`1`=c('this', 'chr', ''))))
          expect_that(m(string, paste0(capture(alphas), space, capture(alphas))), equals(list(`1`=c('this', '', ''), `2`=c('is', '', ''))))

})

test_that('m with named capture returns named list', {

          expect_that(m(string, named_capture(digets, 'digets')), equals(list(digets=c('', '12', '12343'))))
          expect_that(m(string, named_capture(alphas, 'alphas')), equals(list(alphas=c('this', 'chr', ''))))

})

test_that('m options other than g', {
          expect_that(m(string, 'text', options='i'), equals(c(TRUE, FALSE, FALSE)))
          expect_that(m(multiline_string, 'This.is.a', options='s'), equals(TRUE))
          expect_that(m(multiline_string, 'this.is.a', options='si'), equals(TRUE))

          expect_that(m(multiline_string, 'this\n#comment\n . is . a', options='six'), equals(TRUE))
          expect_that(m(multiline_string, '^is'), equals(FALSE))
          expect_that(m(multiline_string, '^is', options='m'), equals(TRUE))
          expect_that(m(multiline_string, 'is$', options='m'), equals(TRUE))
})

test_that('m with g', {
          expect_that(m(string, capture(alpha), options='g'), equals(list(list(`1`=c('t', 'h', 'i', 's', 'i', 's', 'T', 'e', 'x', 't')), list(`1`=c('c', 'h', 'r')), list(`1`=''))))
})

test_that('s fails if given other than character vector', {
          expect_that(s(1:10, diget), throws_error())
          expect_that(s(string, 1), throws_error())
          expect_that(s(string, diget, 1), throws_error())
          expect_that(s(factor(string), diget), throws_error())
})

test_that('s substitutes properly, with and without options', {
          expect_that(s(string, 'Text', 'test'), equals(c('this is test', 'chr-12', '12343 66544456')))
          expect_that(s(string, 'text', 'test', options='i'), equals(c('this is test', 'chr-12', '12343 66544456')))
          expect_that(s(string, 'is [ ] text', 'is test', options='ix'), equals(c('this is test', 'chr-12', '12343 66544456')))
          expect_that(s(string, 'i', 'x', options='g'), equals(c('thxs xs Text', 'chr-12', '12343 66544456')))
})

test_that("split_regex splits various regex\'s properly", {
  expect_that(split_regex('/test/i'), c(pattern='test', options='i'))
  expect_that(split_regex('{test}i'), c(pattern='test', options='i'))
  expect_that(split_regex('[test]i'), c(pattern='test', options='i'))
  expect_that(split_regex('<test>i'), c(pattern='test', options='i'))
  expect_that(split_regex('!test!i'), c(pattern='test', options='i'))
  expect_that(split_regex('_test_i'), c(pattern='test', options='i'))
  expect_that(split_regex(' test i'), c(pattern='test', options='i'))
  expect_that(split_regex('/test/i'), c(pattern='test', options='i'))
  expect_that(split_regex('{test}i'), c(pattern='test', options='i'))
  expect_that(split_regex('/test/replace/i'), c(pattern='test', replacement='replace', options='i'))
  expect_that(split_regex('{test}{replace}i'), c(pattern='test', replacement='replace', options='i'))
})

test_that("infix functions work properly", {
  expect_that(string %m% '/this/', equals(m(string, 'this')))
  expect_that(string %m% '/this//', throws_error())
  expect_that(string %s% '/this/that/', equals(s(string, 'this', 'that')))
})
