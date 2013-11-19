\name{\%s\%}
\alias{\%s\%}
\title{Perl style infix substitution function}
\usage{
data \%s\% pattern
}
\arguments{
  \item{pattern}{a character delimited regular expression
  pattern like those in perl}

  \item{data}{character vector to substitute}
}
\description{
  This function allows the substitution syntax to mimic
  perl's very closely.  As in perl the first character is
  used as a delimiter to separate the regular expression
  and options.  Paired delimiters are supported as in perl,
  so both '/this/' and '{this}' are both supported for
  instance.
}
\examples{
string = c('this is a Test', 'string')
string \%s\% '/this/that/'
string \%s\% '{is}{at}g'
}

