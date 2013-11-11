#' \pkg{perlrer} Use perl regular expressions in R!
#'
#' @import assertthat
#' @name perlrer
#' @docType package
NULL

pairs = c('<' = '>', '\\{' = '\\}', '\\[' = '\\]', '\\(' = '\\)')

#' @export m
m = function(data, pattern, options=""){
  #check arguments
  assert_that(is.character(data))
  assert_that(is.character(pattern))
  assert_that(is.character(options))

  process_matches = function(data, res){
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
    res = gregexpr(pattern=pattern, data, perl=T)
    ret = list()
    for(itr in seq_along(res)){
      ret[[itr]] = process_matches(data[itr], res[[itr]])
    }
    ret
  }
  else{
    pattern = reformat_pattern(pattern, options)
    process_matches(data, regexpr(pattern=pattern, data, perl=T))
  }
}

#' @export
"%m%" = function(data, pattern){
  pattern = split_regex_m(pattern)
  m(data, pattern$pattern, pattern$options)
}

#' @export s
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


#' @export
"%s%" = function(data, pattern){
  pattern = split_regex_s(pattern)
  s(data, pattern$pattern, pattern$replacement, pattern$options)
}

#' @export
psplit = function(pattern, data, options, names=NULL){
  res = gregexpr(pattern=pattern, data, perl=T)[[1]]
  lengths = attr(res, 'match.length')
  prev = c(1, res)
  ret = substring(data, prev+c(0, lengths), c(res-lengths, nchar(data)))
  if(!is.null(names))
    names(ret) = names[seq_along(names)] #TODO short circuit like perl
  ret
}

#' @export
pjoin = function(delim = '', ...) paste(unlist(list(...)), collapse=delim)

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

capture = function(...) paste0('(', ..., ')', collapse='')
named_capture = function(name, ...) paste0('(?<', name, '>', ..., ')', collapse='')
group = function(...) paste0('(?:', ..., ')', collpase='')
not = function(...) paste0('[^', ..., ']', collape='')
any1 = function(x) paste0(x, '+')
any = function(x) paste0(x, '*')
not_escaped = function(x) paste0('(?<!\\\\)', x)
cap_class = function(x) paste('[', x, ']')
