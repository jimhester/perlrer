# perlrer #
Use perl regex syntax, get perl results in R!

[![Build Status](https://travis-ci.org/jimhester/perlrer.png)](https://travis-ci.org/jimhester/perlrer)

```perl
$string = 'This is a test';
print scalar $string =~ m/this/i;
```

```
## 1
```


```r
library(perlrer)
string = 'This is a test'
string %m% '/this/i'
```

```
## [1] TRUE
```


```perl
$string = 'This is a test';
$string =~ s/this/that/i;
print $string;
```

```
## that is a test
```


```r
library(perlrer)
string = 'This is a test'
string %s% '/this/that/i'
```

```
## [1] "that is a test"
```


```perl
@alist = ('one', 'two', 'three');
$astring = 'four';
$joined_str = join(':', @alist, $astring);
print $joined_str, "\n";
@split_str = split(/:/, $joined_str);
print "@split_str\n";
```

```
## one:two:three:four
## one two three four
```


```r
library(perlrer)
alist = c('one', 'two', 'three')
astring = 'four'
joined_str = pjoin(':', alist, astring)
joined_str
split_str = psplit(':', joined_str)
split_str
```

```
## [1] "one:two:three:four"
```

```
##     one     two   three    four 
##   "one"   "two" "three"  "four"
```


