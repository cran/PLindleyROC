library(testthat)        # load testthat package
library(PLindleyROC)            # load our package
# Test whether the output is a list
test_that("functions returns a list", {
  expect_type(qPLD(p=.4,alpha=2,beta=5), "double")
  expect_type(dPLD(x=2,alpha=2,beta=5),"double")
  expect_type(pPLD(x=5,alpha=2,beta=5),"double")
  expect_type(rPLD(n=100,alpha=2,beta=5),"double")
  expect_type(plAUC(alpha1=2,beta1=5,alpha2=6,beta2=1),"double")
  expect_type(plJ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0),"list")
  expect_type(plER(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0),"list")
  expect_type(plCZ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0),"list")
  expect_type(plIU(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0),"list")
  expect_type(plNI(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0),"list")
  expect_type(prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=6,beta2=1),"double")
  expect_type(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=5,alpha2=6,beta2=1,
                    empirical=TRUE), "list")
  expect_type(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=5,alpha2=6,beta2=1,
                    empirical=FALSE), "list")
})
# Test whether the output contains the right number
test_that("functions returns a list with the specified length", {
  expect_length(qPLD(p=c(.1,.7,.5,.9,.4),alpha=2,beta=5), 5)
  expect_length(qPLD(p=.5,alpha=2,beta=5), 1)
  expect_length(pPLD(x=c(1,2,3,4,5),alpha=2,beta=5), 5)
  expect_length(pPLD(x=1,alpha=2,beta=5), 1)
  expect_length(dPLD(x=c(1,2,3,4,5),alpha=2,beta=5), 5)
  expect_length(dPLD(x=1,alpha=2,beta=5), 1)
  expect_length(rPLD(n=c(1,2,3,4,5),alpha=2,beta=5), 5)
  expect_length(rPLD(n=1,alpha=2,beta=5), 1)
  expect_length(plAUC(alpha1=2,beta1=5,alpha2=6,beta2=1), 1)
  expect_length(plJ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), 5)
  expect_length(plER(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), 5)
  expect_length(plCZ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), 5)
  expect_length(plIU(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), 5)
  expect_length(plNI(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), 5)
  expect_length(prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=6,beta2=1), 3)
})
# Test whether the output is a vector with the expected size
test_that("functions returns a  vector with the expected size", {
  expect_vector(qPLD(p=c(.1,.7,.5,.9,.4),alpha=2,beta=5), ptype = double(),
                size = 5)
  expect_vector(dPLD(x=c(1,2,3,4,5),alpha=2,beta=5), ptype = double(), size = 5)
  expect_vector(pPLD(x=c(1,2,3,4,5),alpha=2,beta=5), ptype = double(), size = 5)
  expect_vector(rPLD(n=c(1,2,3,4,5),alpha=2,beta=5), ptype = double(), size = 5)
  expect_vector(plAUC(alpha1=2,beta1=5,alpha2=6,beta2=1), ptype = double(),
                size = 1)
  expect_vector(plJ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), ptype = list(),
                size = 5)
  expect_vector(plER(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), ptype = list(),
                size = 5)
  expect_vector(plCZ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), ptype = list(),
                size = 5)
  expect_vector(plIU(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), ptype = list(),
                size = 5)
  expect_vector(plNI(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0), ptype = list(),
                size = 5)
  expect_vector(prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=6,beta2=1),
                ptype = double(), size = 3)
  })
# Test whether the output is an error
test_that("functions returns error", {
  expect_error(qPLD(p=-.2,alpha=2,beta=5), "p must be between 0 and 1")
expect_error(qPLD(p=.2,alpha=-2,beta=5), "alpha value must be greather than 0")
expect_error(qPLD(p=.2,alpha=2,beta=-5), "beta value must be greather than 0")
  expect_error(dPLD(x=c(1,2,3,4,5),alpha=-2,beta=5),
               "alpha value must be greather than 0")
  expect_error(dPLD(x=c(1,2,3,4,5),alpha=2,beta=-5),
               "beta value must be greather than 0")
  expect_error(pPLD(x=c(1,2,3,4,5),alpha=-2,beta=5),
               "alpha value must be greather than 0")
  expect_error(pPLD(x=c(1,2,3,4,5),alpha=2,beta=-5),
               "beta value must be greather than 0")
  expect_error(rPLD(n=c(1,2,3,4,5),alpha=-2,beta=5),
               "alpha value must be greather than 0")
  expect_error(rPLD(n=c(1,2,3,4,5),alpha=2,beta=-5),
               "beta value must be greather than 0")
  expect_error(rPLD(n=0,alpha=2,beta=5), "n value must be >=1")
  expect_error(plAUC(alpha1=-2,beta1=5,alpha2=6,beta2=1),
               "alpha1 value must be greather than 0")
  expect_error(plAUC(alpha1=2,beta1=-5,alpha2=6,beta2=1),
               "beta1 value must be greather than 0")
  expect_error(plAUC(alpha1=2,beta1=5,alpha2=-6,beta2=1),
               "alpha2 value must be greather than 0")
  expect_error(plAUC(alpha1=2,beta1=5,alpha2=6,beta2=-1),
               "beta2 value must be greather than 0")
  expect_error(plJ(alpha1=-2,beta1=5,alpha2=6,beta2=1,init=0),
               "alpha1 value must be greather than 0")
  expect_error(plJ(alpha1=2,beta1=-5,alpha2=6,beta2=1,init=0),
               "beta1 value must be greather than 0")
  expect_error(plJ(alpha1=2,beta1=5,alpha2=-6,beta2=1,init=0),
               "alpha2 value must be greather than 0")
  expect_error(plJ(alpha1=2,beta1=5,alpha2=6,beta2=-1,init=0),
               "beta2 value must be greather than 0")
  expect_error(plER(alpha1=-2,beta1=5,alpha2=6,beta2=1),
               "alpha1 value must be greather than 0")
  expect_error(plER(alpha1=2,beta1=-5,alpha2=6,beta2=1),
               "beta1 value must be greather than 0")
  expect_error(plER(alpha1=2,beta1=5,alpha2=-6,beta2=1),
               "alpha2 value must be greather than 0")
  expect_error(plER(alpha1=2,beta1=5,alpha2=6,beta2=-1),
               "beta2 value must be greather than 0")
  expect_error(plCZ(alpha1=-2,beta1=5,alpha2=6,beta2=1,init=0),
               "alpha1 value must be greather than 0")
  expect_error(plCZ(alpha1=2,beta1=-5,alpha2=6,beta2=1,init=0),
               "beta1 value must be greather than 0")
  expect_error(plCZ(alpha1=2,beta1=5,alpha2=-6,beta2=1,init=0),
               "alpha2 value must be greather than 0")
  expect_error(plCZ(alpha1=2,beta1=5,alpha2=6,beta2=-1,init=0),
               "beta2 value must be greather than 0")
  expect_error(plIU(alpha1=-2,beta1=5,alpha2=6,beta2=1,init=0),
               "alpha1 value must be greather than 0")
  expect_error(plIU(alpha1=2,beta1=-5,alpha2=6,beta2=1,init=0),
               "beta1 value must be greather than 0")
  expect_error(plIU(alpha1=2,beta1=5,alpha2=-6,beta2=1,init=0),
               "alpha2 value must be greather than 0")
  expect_error(plIU(alpha1=2,beta1=5,alpha2=6,beta2=-1,init=0),
               "beta2 value must be greather than 0")
  expect_error(plNI(alpha1=-2,beta1=5,alpha2=6,beta2=1,init=0),
               "alpha1 value must be greather than 0")
  expect_error(plNI(alpha1=2,beta1=-5,alpha2=6,beta2=1,init=0),
               "beta1 value must be greather than 0")
  expect_error(plNI(alpha1=2,beta1=5,alpha2=-6,beta2=1,init=0),
               "alpha2 value must be greather than 0")
  expect_error(plNI(alpha1=2,beta1=5,alpha2=6,beta2=-1,init=0),
               "beta2 value must be greather than 0")
  expect_error(prfROC(ctp=0.5,alpha1=-2,beta1=5,alpha2=6,beta2=1),
               "alpha1 value must be greather than 0")
  expect_error(prfROC(ctp=0.5,alpha1=2,beta1=-5,alpha2=6,beta2=1),
               "beta1 value must be greather than 0")
  expect_error(prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=-6,beta2=1),
               "alpha2 value must be greather than 0")
  expect_error(prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=6,beta2=-1),
               "beta2 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=-2,beta1=5,alpha2=6,beta2=1,
                     empirical=TRUE), "alpha1 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=-5,alpha2=6,beta2=1,
                     empirical=TRUE), "beta1 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=5,alpha2=-6,beta2=1,
                     empirical=TRUE), "alpha2 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=5,alpha2=6,beta2=-1,
                     empirical=TRUE), "beta2 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=-2,beta1=5,alpha2=6,beta2=1,
                     empirical=FALSE), "alpha1 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=-5,alpha2=6,beta2=1,
                     empirical=FALSE), "beta1 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=5,alpha2=-6,beta2=1,
                     empirical=FALSE), "alpha2 value must be greather than 0")
  expect_error(plROC(x=c(1,2,3,4),y=c(2,3,4),alpha1=2,beta1=5,alpha2=6,beta2=-1,
                     empirical=FALSE), "beta2 value must be greather than 0")
  })
