
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PLindleyROC

<!-- badges: start -->

[![R-CMD-check](https://github.com/ErtanSU/PLindleyROC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ErtanSU/PLindleyROC/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ErtanSU/PLindleyROC/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ErtanSU/PLindleyROC)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/PLindleyROC)](https://cran.r-project.org/package=PLindleyROC)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[<img src="man/figures/logo.png" align="right" height="139" alt="" />](https://github.com/ErtanSU/PLindleyROC)
<!-- badges: end -->

The goal of PLindleyROC is to evaluate the Receiver Operating
Characteristic (ROC) for Power Lindley Distribution. Additionally, The
performace asssesments can be performed associated with the Bi-Power
Lindley ROC model.

## Installation

You can install the development version of PLindleyROC via the following
code:

``` r
# install.packages("devtools")
devtools::install_github("ErtanSU/PLindleyROC")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PLindleyROC)
dPLD(c(1,2,3,4,5,200),alpha=3,beta=2)
#> [1]  1.082682e+00  1.620507e-05  3.560890e-21  1.070039e-52 3.363180e-105
#> [6]  0.000000e+00
```

``` r
library(PLindleyROC)
pPLD(c(.5,1,2,3,4),alpha=3,beta=2)
#> [1] 0.1562992 0.7744412 0.9999993 1.0000000 1.0000000
```

``` r
library(PLindleyROC)
qPLD(c(.9971,0.5,0.3),alpha=3,beta=2)
#> [1] 1.5220612 0.7868721 0.6362570
```

``` r
library(PLindleyROC)
rPLD(10,alpha=3,beta=2)
#>  [1] 0.3856591 0.7168456 0.8070020 0.6398473 0.9780382 0.5930996 1.6253265
#>  [8] 1.1524593 0.9633790 0.9465301
```

``` r
library(PLindleyROC)
plAUC(alpha1=2,beta1=5,alpha2=6,beta2=1)
#> [1] 0.9688111
```

``` r
library(PLindleyROC)
plJ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
#> $par
#> [1] 0.7345928
#> 
#> $value
#> [1] -0.8241212
#> 
#> $counts
#> function gradient 
#>       16       16 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

``` r
library(PLindleyROC)
plER(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
#> $par
#> [1] 0.7430008
#> 
#> $value
#> [1] 0.1247003
#> 
#> $counts
#> function gradient 
#>       11       11 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

``` r
library(PLindleyROC)
plCZ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
#> $par
#> [1] 0.736217
#> 
#> $value
#> [1] -0.831771
#> 
#> $counts
#> function gradient 
#>       17       17 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

``` r
library(PLindleyROC)
plIU(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
#> $par
#> [1] 0.7345927
#> 
#> $value
#> [1] 0.113501
#> 
#> $counts
#> function gradient 
#>       12       12 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

``` r
library(PLindleyROC)
plNI(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0.5)
#> $par
#> [1] 0.7873601
#> 
#> $value
#> [1] -0.01326588
#> 
#> $counts
#> function gradient 
#>        6        6 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

``` r
library(PLindleyROC)
x=c(1,2,3,4)
y=c(2,3,4)
plROC(x,y,alpha1=2,beta1=5,alpha2=6,beta2=1,empirical=FALSE)
```

<img src="man/figures/README-pressure-1.png" width="100%" />

``` r
library(PLindleyROC)
prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=6,beta2=1)
#>  Sensitivitiy   Specificity 1-Specificity 
#>     0.9921878     0.6538067     0.3461933
```

## Corresponding Author

Department of Statistics, Faculty of Science, Selcuk University, 42250,
Konya, Turkey <br />

Email:<https://www.researchgate.net/profile/Ertan-Akgenc>

## References

Akgenç, E., and Kuş, C., 2023, *ROC Curve Analysis for the Measurements
Distributed Power-Lindley Distribution*, 2nd International E-Conference
On Mathematical And Statistical Sciences: A Selçuk Meeting
(ICOMSS-2023), Konya, 25.

Attwood, K., Hou, S., and Hutson, A., 2022, *Application of the skew
exponential power distribution to ROC curves*, Journal of Applied
Statistics, 1-16.

Ghitany M., Al-Mutairi D. K., Balakrishnan N., and Al-Enezi L., 2013,
*Power lindley distribution and associated inference*, Computational
Statistics & Data Analysis, 64,20–33.

Liu, X., 2012, *Classification accuracy and cut point selection*,
Statistics in medicine, 31(23), 2676-2686.

Nahm, F. S., 2022, *Receiver operating characteristic curve: overview
and practical use for clinicians*, Korean journal of anesthesiology,
75(1), 25-36.

Perkins, N. J., and Schisterman, E. F., 2006, *The inconsistency of
“optimal” cutpoints obtained using two criteria based on the receiver
operating characteristic curve*, American journal of epidemiology,
163(7), 670-675.

Pundir, S. and Amala, R., 2014, *Evaluation of area under the constant
shape bi-weibull roc curve*, Journal of Modern Applied Statistical
Methods, 13(1),1-20.

Youden, W. J., 1950, *Index for rating diagnostic tests*, Cancer, 3(1),
32-35.
