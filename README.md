
<!-- README.md is generated from README.Rmd. Please edit that file -->
formulize
=========

[![Travis build status](https://travis-ci.org/alexpghayes/formulize.svg?branch=master)](https://travis-ci.org/alexpghayes/formulize) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/alexpghayes/formulize?branch=master&svg=true)](https://ci.appveyor.com/project/alexpghayes/formulize) [![Coverage status](https://codecov.io/gh/alexpghayes/formulize/branch/master/graph/badge.svg)](https://codecov.io/github/alexpghayes/formulize?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/formulize)](https://cran.rstudio.com/web/packages/formulize/index.html) ![Downloads](http://cranlogs.r-pkg.org/badges/formulize)

If you:

-   like using formulas, recipes and data frames to specify design matrices
-   develop nervous ticks when you come across modelling packages that only offer matrix/vector interfaces
-   don't have the time or motivation to write a formula wrapper around these interfaces
-   like untested and hacky software written by amateurs

then `formulize` may be for you. Formulize is very new, but you can install it from CRAN if you have R 3.4+ with:

``` r
install.packages("formulize")
```

You can get the development version (recommended) with:

``` r
# install.packages("devtools")
devtools::install_github("alexpghayes/formulize")
```

Adding a formula or recipe interface
------------------------------------

Suppose you want to add a formula interface to an existing modelling function, say `cv.glmnet`. Then you could do the following

``` r
library(recipes)
library(glmnet)
library(formulize)

glmnet_cv <- formulize(cv.glmnet)

glmnet_model <- glmnet_cv(mpg ~ drat + hp - 1, mtcars)
predict(glmnet_model, head(mtcars))
#>                          1
#> Mazda RX4         22.13660
#> Mazda RX4 Wag     22.13660
#> Datsun 710        22.60290
#> Hornet 4 Drive    20.06405
#> Hornet Sportabout 17.97487
#> Valiant           19.42956
```

Similarly `glmnet_cv` works with recipe objects like so

``` r
rec <- recipe(mpg ~ drat + hp, data = mtcars)

glmnet_model2 <- glmnet_cv(rec, mtcars)
predict(glmnet_model2, head(mtcars))
#>             1
#> [1,] 22.44828
#> [2,] 22.44828
#> [3,] 22.95820
#> [4,] 19.94207
#> [5,] 17.62202
#> [6,] 19.15896
```

You may also be interested in the more ~~dangerous~~ exciting version `genericize`, which you should call for its side effects.

``` r
genericize(cv.glmnet)

form <- mpg ~ drat + hp - 1
X <- model.matrix(form, mtcars)
y <- mtcars$mpg

set.seed(27)
mat_model <- cv.glmnet(X, y, intercept = TRUE)

set.seed(27)
frm_model <- cv.glmnet(form, mtcars, intercept = TRUE)

set.seed(27)
rec_model <- cv.glmnet(rec, mtcars, intercept = TRUE)

predict(mat_model, head(X))
#>                          1
#> Mazda RX4         22.25028
#> Mazda RX4 Wag     22.25028
#> Datsun 710        22.73249
#> Hornet 4 Drive    20.01959
#> Hornet Sportabout 17.84620
#> Valiant           19.33092
predict(frm_model, head(mtcars))
#>                          1
#> Mazda RX4         22.25028
#> Mazda RX4 Wag     22.25028
#> Datsun 710        22.73249
#> Hornet 4 Drive    20.01959
#> Hornet Sportabout 17.84620
#> Valiant           19.33092
predict(rec_model, head(mtcars))
#>             1
#> [1,] 22.25035
#> [2,] 22.25035
#> [3,] 22.73255
#> [4,] 20.01946
#> [5,] 17.84608
#> [6,] 19.33070
```

This creates a new S3 generic `cv.glmnet`, sets the provided function as the default method (`cv.glmnet.default`), and adds methods `cv.glmnet.formula` and `cv.glmnet.recipe` using `formulize`.

This will mask `cv.glmnet` and features no safety checks because safety isn't fun.

Caveats
-------

-   `formulize` doesn't do anything special with intercepts. This means that you need to careful with functions that require you to specify intercepts in non-standard ways, such as `cv.glmnet` above.
-   If the original modelling function doesn't return a list, `formulize` will probably break.
-   If you're just looking for a formula interface to `glmnet`, take a look at [glmnetUtils](https://github.com/Hong-Revo/glmnetUtils).
