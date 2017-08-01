<!-- README.md is generated from README.Rmd. Please edit that file -->
svenssonm
=========

The goal of svenssonm is to make it easier to realize Svensson's Method by R. Svensson's Method is a rank-invariant nonparametric method for the analysis of ordered scales which measures the level of change both from systematic and individual aspects. Please refer to the following dissertation for more detail information about Svensson's Method.

Example
-------

An example about how to get the results you want:

``` r
## basic example code
x <- c (1:5,5:1) #a numeric vector of data values
y <- c(1:5,1,1,5,4,1) #have same length as x
z <- con_ta(x,y,5) #The contengency table, a two-dimension matrix.
sresult(z) #Get all the results for Svensson's method.
```

### Installation Instructions

``` r
install.packages("svenssonm")
library(svenssonm)
```

OR

``` r
install.packages("devtools")
devtools::install_github("lexizhu/svenssonm")
library(svenssonm)
```

#### The Main Components

Contingency Table Generation: con\_ta(x, y, level = 5)

Systematic Change: rp(t); rpse(t); rc(t); rcse(t)

Individual Change: rv(t); rvse(t); iv(t); ralpha(t); pralpha(t)

Percentage Agreement: pa(t)

Summary for Svensson's Method: sresult(t)
