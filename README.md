# Recipes

[![Travis-CI Build Status](https://travis-ci.org/.svg?branch=master)](https://travis-ci.org/)

The `recipes` package is an alternative method for creating and preprocessing design matrices that can be used for modeling or visualization. From [wikipedia]():

 > In statistics, a **design matrix** (also known as regressor matrix or model matrix) is a matrix of values of explanatory variables of a set of objects, often denoted by X. Each row represents an individual object, with the successive columns corresponding to the variables and their specific values for that object.

While R already has long-standing methods for creating these matrices (e.g. [formulas](https://www.rstudio.com/rviews/2017/02/01/the-r-formula-method-the-good-parts) and `model.matrix`), there are some limitations to what the existing infrastructure can do. 

The idea of the `recipes` package is to define a recipe or blueprint that can be used to sequentially define the encodings and preprocessing of the data (i.e. "feature engineering"). 

The package is still in devleopent and is not on CRAN. To install it, use:

```r
library(devtools)
install_github("topepo/recipes")
```
