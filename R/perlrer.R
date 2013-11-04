#' \pkg{perlrer} Use perl regular expressions in R!
#' @import assertthat
#' @name perlrer
#' @docType package
NULL

#TODO Handle g option

#' @export m
m = function(data, pattern, options=""){
  #check arguments
  assert_that(is.character(data))
  assert_that(is.character(pattern))
  assert_that(is.character(options))

  pattern = reformat_pattern(pattern, options)

  res = regexpr(pattern=pattern, data, perl=T)
  starts = attr(res, 'capture.start')
  if(is.null(starts)){
    return(res != -1)
  }
  lengths = attr(res, 'capture.length')
  names = attr(res, 'capture.names')
  ret = list()
  for(itr in seq_len(ncol(starts))){
    ret[[itr]] = substring(data, starts[,itr], starts[,itr] + lengths[,itr] - 1)
  }
  names(ret) = ifelse(names == "", 1:nrow(starts), names)
  ret
}
#cannot get number of replacements without modifying C code in grep.c
#' @export s
s = function(data, pattern, replacement, options='') {
  #check arguments
  assert_that(is.character(data))
  assert_that(is.character(pattern))
  assert_that(is.character(replacement))
  assert_that(is.character(options))

  pattern = reformat_pattern(pattern, options)

  res = if(grepl(options, 'g') == TRUE)
     res = gsub(x=data, pattern=pattern, replacement=replacement, perl=T)
    else
     res = sub(x=data, pattern=pattern, replacement=replacement, perl=T)
  res
}

reformat_pattern = function(pattern, options){
  #replace any perl style captures ($1) with \\1
  pattern = gsub('\\$([0-9]+)', '\\\\1', pattern)

  #add any given options to the pattern
  if(options != "")
    pattern = paste0("(?", paste0(options, collapse=""), ")", pattern)
  pattern
}
