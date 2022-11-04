# capelinker

`capelinker` is an R library providing functions and a number of pretrained models to link families and individuals in historical South Africa.

## Installation
Capelinker is not on CRAN and needs to be installed from GitHub. The `remotes` package makes this easy.

```
install.packages("remotes") # if not installed already
library("remotes")
remotes::install_github("rijpma/capelinker")
```

## Usage

1. Use `preflight()` to check whether the datasets are ready to link. 
2. Make candidate links using `candidates()`
3. Calculate distances between candidate pairs using `distcals()`.
4. Predict links and keep best ones using `predict_links`.

## Todo

Add opgaafrollen pretrained models and functions to do linkage in panel setting.