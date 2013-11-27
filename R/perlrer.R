#' \pkg{perlrer} Use perl regular expressions in R!
#'
#' This package aims to bring common perl regular expression functonality to R
#' There are two major functions, m for regular expression matching and s for substitution
#' @examples
#' library(perlrer)
#' string = 'This is a test'
#'
#' #using the functional style
#' m(string, 'this', 'i')
#' #using the perl infix function style
#' string %m% '/this/i'
#'
#' #substitutions
#' string = 'This is a test'
#' s(string, 'this', 'that', 'i')
#' string %s% '/this/that/i'
#'
#' #perl join and split for strings
#' pjoin(' ', string)
#' psplit(' ', string)
#' @import assertthat
#' @name perlrer
#' @docType package
NULL

#' Match function using perl compatible regular expressions
#'
#' @param data character vector to match against
#' @param pattern regular expression to use for matching
#' @param options regular expression options to use
#' @return if no captures, returns a logical vector the same length as the
#' input character vector specifying if the relevant value matched or not.  If
#' there are captures in the regular expression, returns a list of named
#' character vectors with the captured text.  If the g option is used with
#' capturing, the output is a list of lists.
#' @seealso \code{\link{regex}} Section 'Perl-like Regular Expressions' for a
#' discussion of the supported options
#' @export
m = function(data, pattern, options=""){
  #check arguments
  assert_that(is.character(data))
  assert_that(is.character(pattern))
  assert_that(is.character(options))

  process_matches = function(res, data){
    starts = attr(res, 'capture.start')
    if(is.null(starts)){
      return(res != -1)
    }
    lengths = attr(res, 'capture.length')
    names = attr(res, 'capture.names')
    ret = list()
    for(itr in seq_len(ncol(starts))){
      ret[[itr]] = unname(ifelse(starts[,itr] == -1, "FALSE",
                          substring(data, starts[,itr], starts[,itr] + lengths[,itr] - 1)))
    }
    names(ret) = ifelse(names == "", 1:nrow(starts), names)
    ret
  }

  if(grepl('g', options)){
    options = gsub('g', '', options)
    pattern = reformat_pattern(pattern, options)
    mapply(process_matches, gregexpr(pattern=pattern, data, perl=T), data)
  }
  else{
    pattern = reformat_pattern(pattern, options)
    process_matches(regexpr(pattern=pattern, data, perl=T), data)
  }
}

#' Perl style infix match function
#'
#' This function allows the match syntax to mimic perl's very closely.  As in
#' perl the first character is used as a delimiter to separate the regular
#' expression and options.  Paired delimiters are supported as in perl, so both
#' '/this/' and '{this}' are both supported for instance.
#' @inheritParams m
#' @param pattern a character delimited regular expression pattern like those in perl
#' @examples
#' string = c('this is a Test', 'string')
#' string %m% '/test/i'
#' #paired delimiters
#' string %m% '{test}i'
#' #captures return numbered results
#' string %m% '!(string)!'
#' #named captures
#' string %m% '/(?<type>string)/'
#' # g option also
#' string %m% '{(\\w+)}g'
#' @export
"%m%" = function(data, pattern){
  pattern = split_regex_m(pattern)
  m(data, pattern$pattern, pattern$options)
}

#' Substitution function using perl compatible regular expressions
#'
#' @param data character vector to substitute
#' @param pattern regular expression to match
#' @param replacement replacement text to use
#' @param options option flags
#' @seealso \code{\link{regex}} Section 'Perl-like Regular Expressions' for a
#' discussion of the supported options
#' @examples
#' string = c('this is a Test', 'string')
#' s(string, 'test', 'not a test', 'i')
#' s(string, 'i', 'x', 'g')
#' @export
#cannot get number of replacements without modifying C code in grep.c
s = function(data, pattern, replacement, options='') {
  #check arguments
  assert_that(is.character(data))
  assert_that(is.character(pattern))
  assert_that(is.character(replacement))
  assert_that(is.character(options))

  replacement = perl_capture(replacement)

  res = if(grepl(options, 'g') == TRUE){
    options = gsub('g', '', options)
    pattern = reformat_pattern(pattern, options)
    gsub(x=data, pattern=pattern, replacement=replacement, perl=T)
  }
  else{
    pattern = reformat_pattern(pattern, options)
    sub(x=data, pattern=pattern, replacement=replacement, perl=T)
  }
  res
}

#' Perl style infix substitution function
#'
#' This function allows the substitution syntax to mimic perl's very closely.  As in
#' perl the first character is used as a delimiter to separate the regular
#' expression and options.  Paired delimiters are supported as in perl, so both
#' '/this/' and '{this}' are both supported for instance.
#'
#' @param pattern a character delimited regular expression pattern like those in perl
#' @inheritParams s
#' @examples
#' string = c('this is a Test', 'string')
#' string %s% '/this/that/'
#' string %s% '{is}{at}g'
#' @export
"%s%" = function(data, pattern){
  pattern = split_regex_s(pattern)
  s(data, pattern$pattern, pattern$replacement, pattern$options)
}

#' split a character vector by using a regular expression, like perl's split
#'
#' @param pattern the pattern to split by
#' @param data the data to split on
#' @param options options to include in the regular expression
#' @param names to give the output
#' @return a (named) character vector with the split result
#' @export
psplit = function(pattern, data, options='', names=NULL){
  split_matches = function(res, data){
    lengths = attr(res, 'match.length')
    if(base::any(lengths == -1))
      return(data)
    prev = c(1, res)
    ret = substring(data, prev+c(0, lengths), c(res-lengths, nchar(data)))
    if(!is.null(names))
      names(ret) = names[seq_along(names)] #TODO short circuit like perl
    ret
  }
  if(length(data) < 2) split_matches(gregexpr(pattern=reformat_pattern(pattern, options), data, perl=T)[[1]], data)
  else mapply(split_matches, gregexpr(pattern=reformat_pattern(pattern, options), data, perl=T), data)
}

#' join a character vector and/or a list by a delimiter, like perl's join it
#' will append all objects and list elements
#'
#' @param delim delimiter to join by,  defaults to space
#' @param ... objects to join
#' @return a joined character
#' @export
pjoin = function(delim = ' ', ...) paste(unlist(list(...)), collapse=delim)

escape_special = function(x){
  s(x, '([\\[\\](){}])', '\\\\$1', 'g')
}

split_regex_m = function(regex){
  first = escape_special(substring(regex, 1, 1))
  #from http://stackoverflow.com/questions/7901978/regex-and-escaped-and-unescaped-delimiter
  pattern = if(first %in% names(pairs)){
    escaped_text = paste0('(?:\\\\.|[^\\\\', pairs[first], ']++)')
    paste0('^', first, named_capture('pattern', any1(escaped_text)),
      pairs[first], named_capture('options', '[a-z]*'), '$')
  }
  else {
    escaped_text = paste0('(?:\\\\.|[^\\\\', first, ']++)')
    paste0('^', first, named_capture('pattern', any1(escaped_text)),
      first, named_capture('options', '[a-z]*'), '$')
  }
  res = m(regex, pattern)
  if(res$pattern == FALSE)
    stop('regex: ', regex, ' is not a valid regular expression')
  res
}

split_regex_s = function(regex){
  first = escape_special(substring(regex, 1, 1))
  #from http://stackoverflow.com/questions/7901978/regex-and-escaped-and-unescaped-delimiter
  escaped_text = paste0('(?:\\\\.|[^\\\\', first, ']++)')
  pattern = if(first %in% names(pairs)){
    escaped_text = paste0('(?:\\\\.|[^\\\\', pairs[first], ']++)')
    paste0('^', first, named_capture('pattern', any1(escaped_text)),
      pairs[first], first, named_capture('replacement', any(escaped_text)),
      pairs[first], named_capture('options', '[a-z]*'), '$')
  }
  else {
    escaped_text = paste0('(?:\\\\.|[^\\\\', first, ']++)')
    paste0('^', first, named_capture('pattern', any1(escaped_text)),
      first, named_capture('replacement', any(escaped_text)),
      first, named_capture('options', '[a-z]*'), '$')
  }
  res = m(regex, pattern)
  if(res$pattern == FALSE)
    stop('regex: ', regex, ' is not a valid regular expression')
  res
}
perl_capture = function(x){
  gsub('\\$([0-9]+)', '\\\\1', x)
}

reformat_pattern = function(pattern, options){
  #replace any perl style captures ($1) with \\1
  pattern = perl_capture(pattern)

  #add any given options to the pattern
  if(options != "")
    pattern = paste0("(?", paste0(options, collapse=""), ")", pattern)
  pattern
}

pairs = c('<' = '>', '\\{' = '\\}', '\\[' = '\\]', '\\(' = '\\)')

#helper functions used in testing
capture = function(...) paste0('(', ..., ')', collapse='')
named_capture = function(name, ...) paste0('(?<', name, '>', ..., ')', collapse='')
group = function(...) paste0('(?:', ..., ')', collpase='')
not = function(...) paste0('[^', ..., ']', collape='')
any1 = function(x) paste0(x, '+')
any = function(x) paste0(x, '*')
not_escaped = function(x) paste0('(?<!\\\\)', x)
cap_class = function(x) paste('[', x, ']')
