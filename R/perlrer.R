m = function(data, pattern, options=""){
  #add any options to the pattern
  pattern = paste0("(?", paste0(options, collapse=""), ")", pattern)
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
