
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/alexpghayes/formulize.svg?branch=master)](https://travis-ci.org/alexpghayes/formulize) [![Coverage status](https://codecov.io/gh/alexpghayes/formulize/branch/master/graph/badge.svg)](https://codecov.io/github/alexpghayes/formulize?branch=master)

formulize
=========

If you:

-   like using formulas and data frames to specify design matrices
-   develop nervous ticks when you come across modelling packages that only offer matrix/vector interfaces
-   don't have the time or motivation to write a formula wrapper around these interfaces
-   like untested and hacky software written by amateurs

then `formulize` may be for you. Formulize is very new, but you can still install formulize from github with:

``` r
# install.packages("devtools")
devtools::install_github("alexpghayes/formulize")
```

Adding a formula interface
--------------------------

Suppose you want to add a formula interface to an existing modelling function, say `cv.glmnet`. Then you could do the following

``` r
library(formulize)
library(glmnet)
#> Loading required package: Matrix
#> Loading required package: foreach
#> Warning: package 'foreach' was built under R version 3.4.3
#> Loaded glmnet 2.0-13

glmnet_cv <- formulize(cv.glmnet)

glmnet_model <- glmnet_cv(mpg ~ drat + hp - 1, mtcars)
predict(glmnet_model, head(mtcars))
#>                          1
#> Mazda RX4         22.35385
#> Mazda RX4 Wag     22.35385
#> Datsun 710        22.85056
#> Hornet 4 Drive    19.97909
#> Hornet Sportabout 17.72895
#> Valiant           19.24104
```

You may also be interested in the more ~~dangerous~~ exciting version `genericize`, which you should call for its side effects.

``` r
genericize(cv.glmnet)

form <- mpg ~ drat + hp - 1
X <- model.matrix(form, mtcars)
y <- mtcars$mpg

set.seed(27)
wrapped_model <- cv.glmnet(form, mtcars, intercept = TRUE)

set.seed(27)
unwrapped_model <- cv.glmnet(X, y, intercept = TRUE)

predict(wrapped_model, head(mtcars))
#>                          1
#> Mazda RX4         22.25028
#> Mazda RX4 Wag     22.25028
#> Datsun 710        22.73249
#> Hornet 4 Drive    20.01959
#> Hornet Sportabout 17.84620
#> Valiant           19.33092
predict(unwrapped_model, head(X))
#>                          1
#> Mazda RX4         22.25028
#> Mazda RX4 Wag     22.25028
#> Datsun 710        22.73249
#> Hornet 4 Drive    20.01959
#> Hornet Sportabout 17.84620
#> Valiant           19.33092
```

This creates a new S3 generic `cv.glmnet`, sets the provided function as the default method (`cv.glmnet.default`), and adds a formula method `cv.glmnet.formula` using `formulize`.

This will mask `cv.glmnet` and features no safety checks because safety isn't fun.

Caveats
-------

-   `formulize` doesn't do anything special with intercepts. This means that you need to careful with functions that require you to specify intercepts in non-standard ways, such as `cv.glmnet` above.
-   If the original modelling function doesn't return a list, `formulize` will probably break.
-   If you're just looking for a formula interface to `glmnet`, take a look at [glmnetUtils](https://github.com/Hong-Revo/glmnetUtils).
