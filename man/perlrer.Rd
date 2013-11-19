\docType{package}
\name{perlrer}
\alias{perlrer}
\alias{perlrer-package}
\title{\pkg{perlrer} Use perl regular expressions in R!}
\description{
  This package aims to bring common perl regular expression
  functonality to R There are two major functions, m for
  regular expression matching and s for substitution
}
\examples{
library(perlrer)
string = 'This is a test'

#using the functional style
m(string, 'this', 'i')
#using the perl infix function style
string \%m\% '/this/i'

#substitutions
string = 'This is a test'
s(string, 'this', 'that', 'i')
string \%s\% '/this/that/i'

#perl join and split for strings
pjoin(' ', string)
psplit(' ', string)
}

