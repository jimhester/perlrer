\name{s}
\alias{s}
\title{Substitution function using perl compatible regular expressions}
\usage{
s(data, pattern, replacement, options = "")
}
\arguments{
  \item{data}{character vector to substitute}

  \item{pattern}{regular expression to match}

  \item{replacement}{replacement text to use}

  \item{options}{option flags}
}
\description{
  Substitution function using perl compatible regular
  expressions
}
\examples{
string = c('this is a Test', 'string')
s(string, 'test', 'not a test', 'i')
s(string, 'i', 'x', 'g')
}
\seealso{
  \code{\link{regex}} Section 'Perl-like Regular
  Expressions' for a discussion of the supported options
}

