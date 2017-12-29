
<!-- README.md is generated from README.Rmd. Please edit that file -->
formulize
=========

If you:

-   like using formulas and data frames to specify design matrices
-   develop nervous ticks when you come across modelling packages that only offer matrix/vector interfaces
-   don't have the time or motivation to write a formula wrapper around these interfaces
-   like untested and hacky software written by amateurs

then `formulize` may be for you. Formulize is currently entirely untested, but you can still install formulize from github with:

``` r
# install.packages("devtools")
devtools::install_github("alexpghayes/formulize")
```

Adding a formula interface
--------------------------

Say you want to add a formula interface to an existing modelling function, say `glmnet`. Then you could do the following

``` r
library(formulize)
library(glmnet)
#> Loading required package: Matrix
#> Loading required package: foreach
#> Warning: package 'foreach' was built under R version 3.4.3
#> Loaded glmnet 2.0-13

glmnet_cv <- formulize(cv.glmnet)

glmnet_model <- glmnet_cv(mpg ~ drat + hp, mtcars)
head(predict(glmnet_model, mtcars))
#>                          1
#> Mazda RX4         22.35385
#> Mazda RX4 Wag     22.35385
#> Datsun 710        22.85056
#> Hornet 4 Drive    19.97909
#> Hornet Sportabout 17.72895
#> Valiant           19.24104
```

How it works: `formulize` creates a copy of function, in this case `cv.glmnet`. This wrapper then works like the origninal function, except it also makes the output of the original function a `wrapped` object and adds a new element `formula`:

``` r
class(glmnet_model)
#> [1] "wrapped"   "cv.glmnet"
glmnet_model$formula
#> mpg ~ drat + hp
```

If the original modelling function doesn't return a list, `formulize` will probably break. Everything should work like normal, except for prediction, in which case there's a special S3 method for `wrapped` objects. Methods that require translation into matrix data format will probably not work, but otherwise things should work as normal.

You may also be interested in the more ~~dangerous~~ exciting version `genericize`, which you should call for its side effects.

``` r
genericize(cv.glmnet)  # NOTE: do not do: genericize(glmnet::cv.glmnet) atm
```

This creates a new S3 generic `cv.glmnet`, sets the provided function as the default method (`cv.glmnet.default`), and adds a formula method `cv.glmnet.formula` using `formulize`.

This will mask `cv.glmnet` and features no safety checks because it's fun when things burn down.

Some notes
==========

-   Currently the formula methods always remove intercepts. This will change soon.
-   If you just want a formula interface to `glmnet`, take a look at [glmnetUtils](https://github.com/Hong-Revo/glmnetUtils)
