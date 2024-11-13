library(testthat)        # load testthat package
library(vctrs)           #load vctrs package
library(tibble)          #load tibble package
library(PLindleyROC)            # load our package
# Test whether the output is a list
test_that("functions returns a list", {
  expect_type(qPLD(p=.4,alpha=2,beta=5), "double")
  expect_type(dPLD(x=2,alpha=2,beta=5),"double")
  expect_type(pPLD(x=5,alpha=2,beta=5),"double")
  expect_type(rPLD(n=100,alpha=2,beta=5),"double")
  expect_type(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    method=c("MLE")),"double")
  expect_type(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    method=c("AD")),"double")
  expect_type(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    method=c("CvM")),"double")
  expect_type(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    method=c("LSE")),"double")
  expect_type(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    method=c("WLSE")),"double")
  expect_type(r.pl_auc(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                       method=c("TRUE")),"double")
  expect_type(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                  init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                  init_index=1,method=c("MLE")),"double")
  expect_type(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                  init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                  init_index=1,method=c("AD")),"double")
  expect_type(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                  init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                  init_index=1,method=c("CvM")),"double")
  expect_type(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                  init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                  init_index=1,method=c("LSE")),"double")
  expect_type(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                  init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                  init_index=1,method=c("WLSE")),"double")
  expect_type(r.pl_index(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                         init_index=1,method=c("TRUE")),"double")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   empirical=TRUE,method=c("MLE")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   empirical=TRUE,method=c("AD")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   empirical=TRUE,method=c("CvM")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   empirical=TRUE,method=c("LSE")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   empirical=TRUE,method=c("WLSE")), "list")
  expect_type(r.pl_graph(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                         method=c("TRUE")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    empirical=FALSE,method=c("MLE")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    empirical=FALSE,method=c("AD")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    empirical=FALSE,method=c("CvM")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    empirical=FALSE,method=c("LSE")), "list")
  expect_type(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    empirical=FALSE,method=c("WLSE")), "list")

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
  expect_length(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("MLE")), 1)
  expect_length(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("AD")), 1)
  expect_length(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("CvM")), 1)
  expect_length(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("LSE")), 1)
  expect_length(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("WLSE")), 1)
  expect_length(r.pl_auc(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                         method=c("TRUE")), 1)
  expect_length(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     init_index=1,method=c("MLE")), 16)
  expect_length(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     init_index=1,method=c("AD")), 16)
  expect_length(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     init_index=1,method=c("CvM")), 16)
  expect_length(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     init_index=1,method=c("LSE")), 16)
  expect_length(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     init_index=1,method=c("WLSE")), 16)
  expect_length(r.pl_index(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                           init_index=1,method=c("TRUE")), 16)

})
# Test whether the output is a vector with the expected size
test_that("functions returns a  vector with the expected size", {
expect_vector(qPLD(p=c(.1,.7,.5,.9,.4),alpha=2,beta=5), ptype = double(),
                size = 5)
expect_vector(dPLD(x=c(1,2,3,4,5),alpha=2,beta=5), ptype = double(),
                       size = 5)
expect_vector(pPLD(x=c(1,2,3,4,5),alpha=2,beta=5), ptype = double(),
                       size = 5)
expect_vector(rPLD(n=c(1,2,3,4,5),alpha=2,beta=5), ptype = double(),
                       size = 5)
expect_vector(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("MLE")), ptype = double(),
                size = 1)
expect_vector(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("AD")), ptype = double(),
                size = 1)
expect_vector(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("CvM")), ptype = double(),
                size = 1)
expect_vector(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("LSE")), ptype = double(),
                size = 1)
expect_vector(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                      init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("WLSE")), ptype = double(),
                size = 1)
expect_vector(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    init_index=1,method=c("MLE")), ptype = cbind(),
                size = 4)
expect_vector(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    init_index=1,method=c("AD")), ptype = cbind(),
                size = 4)
expect_vector(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    init_index=1,method=c("CvM")), ptype = cbind(),
                size = 4)
expect_vector(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    init_index=1,method=c("LSE")), ptype = cbind(),
                size = 4)
expect_vector(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    init_index=1,method=c("WLSE")), ptype = cbind(),
                size = 4)
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
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     method=c("MLE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     method=c("AD")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     method=c("CvM")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     method=c("LSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     method=c("WLSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     method=c("MLE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     method=c("AD")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     method=c("CvM")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     method=c("LSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     method=c("WLSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     method=c("MLE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     method=c("AD")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     method=c("CvM")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     method=c("LSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     method=c("WLSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     method=c("MLE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     method=c("AD")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     method=c("CvM")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     method=c("LSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     method=c("WLSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_auc(true_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                        method=c("TRUE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_auc(true_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                        method=c("TRUE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_auc(true_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                        method=c("TRUE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_auc(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                        method=c("TRUE")),
               "beta2 value must be greather than 0")

  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("MLE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("AD")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("CvM")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("LSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("WLSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                   init_index=1,method=c("MLE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                   init_index=1,method=c("AD")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                   init_index=1,method=c("CvM")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                   init_index=1,method=c("LSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                   init_index=1,method=c("WLSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                   init_index=1,method=c("MLE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                   init_index=1,method=c("AD")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                   init_index=1,method=c("CvM")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                   init_index=1,method=c("LSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                   init_index=1,method=c("WLSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                   init_index=1,method=c("MLE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                   init_index=1,method=c("AD")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                   init_index=1,method=c("CvM")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                   init_index=1,method=c("LSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                   init_index=1,method=c("WLSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_index(true_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                          init_index=1,method=c("TRUE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_index(true_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                          init_index=1,method=c("TRUE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_index(true_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                          init_index=1,method=c("TRUE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_index(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                          init_index=1,method=c("TRUE")),
               "beta2 value must be greather than 0")

  expect_error(r.pl_graph(true_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                          method=c("TRUE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(true_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                          method=c("TRUE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(true_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                          method=c("TRUE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                          method=c("TRUE")),
               "beta2 value must be greather than 0")

  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("MLE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("MLE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=TRUE,method=c("MLE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=TRUE,method=c("MLE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("AD")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("AD")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=TRUE,method=c("AD")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=TRUE,method=c("AD")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("CvM")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("CvM")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=TRUE,method=c("CvM")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=TRUE,method=c("CvM")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("LSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("LSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=TRUE,method=c("LSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=TRUE,method=c("LSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("WLSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("WLSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=TRUE,method=c("WLSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=TRUE,method=c("WLSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("MLE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("MLE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=FALSE,method=c("MLE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=FALSE,method=c("MLE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("AD")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("AD")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=FALSE,method=c("AD")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=FALSE,method=c("AD")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("CvM")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("CvM")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=FALSE,method=c("CvM")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=FALSE,method=c("CvM")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("LSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("LSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=FALSE,method=c("LSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=FALSE,method=c("LSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=-1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("WLSE")),
               "alpha1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=-1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("WLSE")),
               "beta1 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=-1,beta2=1),
                     empirical=FALSE,method=c("WLSE")),
               "alpha2 value must be greather than 0")
  expect_error(r.pl_graph(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=-1),
                     empirical=FALSE,method=c("WLSE")),
               "beta2 value must be greather than 0")
  expect_error(r.pl_auc(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     method=c("MLE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_auc(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     method=c("AD")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_auc(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     method=c("CvM")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_auc(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     method=c("LSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_auc(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     method=c("WLSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_index(x=c(2,3,4),y=c(5,6,7,8,9),
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                    init_index=1,method=c("MLE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   empirical=TRUE,method=c("MLE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_index(x=c(2,3,4),y=c(5,6,7,8,9),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("AD")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("AD")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_index(x=c(2,3,4),y=c(5,6,7,8,9),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("CvM")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("CvM")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_index(x=c(2,3,4),y=c(5,6,7,8,9),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("LSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("LSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_index(x=c(2,3,4),y=c(5,6,7,8,9),
                   init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                   init_index=1,method=c("WLSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=TRUE,method=c("WLSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("MLE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("AD")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("CvM")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("LSE")),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.pl_graph(x=c(2,3,4),y=c(5,6,7,8,9),
                     init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                     empirical=FALSE,method=c("WLSE")),
          "Optimization did not converge.Please check your initial parameters.")

  })
