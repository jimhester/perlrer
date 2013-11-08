#' \pkg{perlrer} Use perl regular expressions in R!
#' @import assertthat
#' @name perlrer
#' @docType package
NULL

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
      ret[[itr]] = ifelse(starts[,itr] == -1, FALSE, substring(data, starts[,itr], starts[,itr] + lengths[,itr] - 1))
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
psplit = function(data, pattern, options, names=NULL){
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
split_regex_s = function(regex){
  pairs = c('<' = '>', '{' = '}', '\\[' = '\\]', '\\(' = '\\)')
  first = escape_special(substring(regex, 1, 1))
  res = if(!first %in% names(pairs))
    m(regex, paste0(first, named_capture('.+?', 'pattern'),
      not_escaped(first), named_capture('.*?', 'replacement'),
      not_escaped(first), named_capture('[a-z]*', 'options')))
  else
    m(regex, paste0(first, named_capture('.+?', 'pattern'),
      not_escaped(pairs[first]), first, named_capture('.*?', 'replacement'),
      not_escaped(pairs[first]), named_capture('[a-z]*', 'options')))
  if(res$pattern == FALSE)
    stop('regex: ', regex, ' is not a valid regular expression')
  res
}
split_regex_m = function(regex){
  pairs = c('<' = '>', '{' = '}', '\\[' = '\\]', '\\(' = '\\)')
  first = escape_special(substring(regex, 1, 1))
  res = if(!first %in% names(pairs))
    m(regex, paste0(first, named_capture('.+?', 'pattern'),
      not_escaped(first), named_capture('[a-z]*', 'options')))
  else
    m(regex, paste0(first, named_capture('.+?', 'pattern'),
      not_escaped(pairs[first]), named_capture('[a-z]*', 'options')))
  if(res$pattern == FALSE)
    stop('regex: ', regex, ' is not a valid regular expression')
  res
}

expand_paired = function(x){
  pairs = c('<' = '>', '{' = '}', '[' = '\\]', '(' = ')')
  if(x %in% names(pairs))
    pairs[x]
  else
    x
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

capture = function(x) paste0('(', x, ')')
named_capture = function(x, name) paste0('(?<', name, '>', x, ')')
not = function(x) paste0('[^', x, ']')
any1 = function(x) paste0(x, '+')
any = function(x) paste0(x, '*')
not_escaped = function(x) paste0('(?<!\\\\)', x)
cap_class = function(x) paste('[', x, ']')
