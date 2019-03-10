# capelinker

`capelinker` provides functions and a number of pretrained models to link families and individuals in historical South Africa.

## Installation
```
library("remotes")
install_github("rijpma/capelinker")
```

or

```
library("devtools")
install_github("rijpma/capelinker")
```

## Usage

1. Use `preflight()` to check whether the datasets are ready to link. 
2. Make candidate links using `candidates()`
3. Calculate distances between candidate pairs using `distcals()`.
4. Predict links and keep best ones using `predict_links`.

## Todo

Add opgaafrollen pretrained models and functions to do linkage in panel setting.