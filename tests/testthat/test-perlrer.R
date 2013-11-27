#TODO non ascii tests
string = c('this is Text', 'chr-12', '12343 66544456')
multiline_string = 'This\nis\na\nstring'

alpha = '[A-Za-z]'
alphas = paste0(alpha, '+')

diget = '[0-9]'
digets = paste0(diget, '+')

test_that('m fails if given other than character vector', {

          expect_error(m(1:10, diget))
          expect_error(m(string, 1))
          expect_error(m(string, diget, 1))
          expect_error(m(factor(string), diget))

})

test_that('m with no capture returns a logical', {

          expect_equal(m(string, diget), c(FALSE, TRUE, TRUE))
          expect_equal(m(string, digets), c(FALSE, TRUE, TRUE))
          expect_equal(m(string, alpha), c(TRUE, TRUE, FALSE))

})

test_that('m works with perl style capture variables', {

          expect_equal(m(string, paste0(capture(4), any1(not(4)), capture('$1+'))), list(`1`=c('FALSE', 'FALSE',4), `2`=c('FALSE', 'FALSE', 444)))
          # named backreferences not supported in r expect_equal(m(string, paste0(named_capture(4, 'four'), '[^4]+', capture('$four+'))), list(four=c('', '',4), `2`=c('', '', 444)))

})

test_that('m with capture returns numbered list', {

          expect_equal(m(string, capture(digets)), list(`1`=c('FALSE', '12', '12343')))
          expect_equal(m(string, capture(alphas)), list(`1`=c('this', 'chr', 'FALSE')))
          expect_equal(m(string, paste0(capture(alphas), ' ', capture(alphas))), list(`1`=c('this', 'FALSE', 'FALSE'), `2`=c('is', 'FALSE', 'FALSE')))

})

test_that('m with named capture returns named list', {

          expect_equal(m(string, named_capture('digets', digets)), list(digets=c('FALSE', '12', '12343')))
          expect_equal(m(string, named_capture('alphas', alphas)), list(alphas=c('this', 'chr', 'FALSE')))

})

test_that('m options other than g', {
          expect_equal(m(string, 'text', options='i'), c(TRUE, FALSE, FALSE))
          expect_equal(m(multiline_string, 'This.is.a', options='s'), TRUE)
          expect_equal(m(multiline_string, 'this.is.a', options='si'), TRUE)

          expect_equal(m(multiline_string, 'this\n#comment\n . is . a', options='six'), TRUE)
          expect_equal(m(multiline_string, '^is'), FALSE)
          expect_equal(m(multiline_string, '^is', options='m'), TRUE)
          expect_equal(m(multiline_string, 'is$', options='m'), TRUE)
})

test_that('m with g', {
          expect_equal(m(string, capture(alpha), options='g'),
                       list(`1`=c('t', 'h', 'i', 's', 'i', 's', 'T', 'e', 'x', 't'),
                            `1`=c('c', 'h', 'r'), `1`='FALSE'))
          #TODO more m g tests
})

test_that('s fails if given other than character vector', {
          expect_that(s(1:10, diget), throws_error())
          expect_that(s(string, 1), throws_error())
          expect_that(s(string, diget, 1), throws_error())
          expect_that(s(factor(string), diget), throws_error())
})

test_that('s substitutes properly, with and without options', {
          expect_equal(s(string, 'Text', 'test'), c('this is test', 'chr-12', '12343 66544456'))
          expect_equal(s(string, 'text', 'test', options='i'), c('this is test', 'chr-12', '12343 66544456'))
          expect_equal(s(string, 'is [ ] text', 'is test', options='ix'), c('this is test', 'chr-12', '12343 66544456'))
          expect_equal(s(string, 'i', 'x', options='g'), c('thxs xs Text', 'chr-12', '12343 66544456'))
})

test_that("split_regex_m splits various regex\'s properly", {
          expect_equal(split_regex_m('/test/i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('{test}i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('[test]i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('<test>i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('!test!i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('_test_i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m(' test i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('/test/i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('{test}i'), list(pattern='test', options='i'))
          expect_equal(split_regex_m('{test}i'), list(pattern='test', options='i'))
          expect_error(split_regex_m('{test}I'))
          expect_error(split_regex_m('/test/blah/'))
          expect_error(split_regex_m('test/blah/'))
          expect_error(split_regex_m('/test//'))
          expect_error(split_regex_m(''))
          expect_error(split_regex_m('/'))
          expect_error(split_regex_m('//'))
          expect_error(split_regex_m('///'))
})

test_that("split_regex_s splits various regex\'s properly", {
          expect_equal(split_regex_s('/test/replace/i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('{test}{replace}i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('[test][replace]i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('<test><replace>i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('!test!replace!i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('_test_replace_i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s(' test replace i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('/test/replace/i'), list(pattern='test', replacement='replace', options='i'))
          expect_equal(split_regex_s('{test}{replace}i'), list(pattern='test', replacement='replace', options='i'))
          expect_error(split_regex_s('/test/blah/aoirestn/'))
          expect_error(split_regex_s('test/blah/ar/'))
          expect_error(split_regex_s(''))
          expect_error(split_regex_s('/'))
          expect_error(split_regex_s('//'))
          expect_error(split_regex_s('///'))
})

test_that("infix functions work properly", {
          expect_equal(string %m% '/this/', m(string, 'this'))
          expect_that(string %m% '/this//', throws_error())
          expect_equal(string %s% '/this/that/', s(string, 'this', 'that'))
})

test_that("pjoin works properly", {
          expect_error(pjoin(1, string))
          expect_equal(pjoin(' ', string), 'this is Text chr-12 12343 66544456')
          expect_equal(pjoin(' ', string, list('testing', 'a', 'list')), 'this is Text chr-12 12343 66544456 testing a list')
          expect_equal(pjoin(':', string, list('testing', 'a', 'list')), 'this is Text:chr-12:12343 66544456:testing:a:list')
})
test_that("psplit works properly", {
          expect_error(psplit(1, string))
          expect_error(psplit(string, 1))
          expect_error(psplit(' ', string, 1))
          expect_equal(psplit(' ', string), list(c('this', 'is', 'Text'), 'chr-12', c('12343', '66544456')))
          expect_equal(psplit(' ', string[[1]]), psplit(' ', string)[[1]])
})
