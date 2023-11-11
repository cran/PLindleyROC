#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @name PLindleyROC
#' @param x,y vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'  to be the number required.
#' @param alpha shape parameter.
#' @param beta scale parameter.
#' @param alpha1 shape parameter of first sample.
#' @param beta1 scale parameter of first sample.
#' @param alpha2 shape parameter of second sample.
#' @param beta2 scale parameter of second sample.
#' @param init initial value for the optimization calculation.
#' @param empirical empirical must be TRUE or FALSE.
#' @param ctp cut-off point value.
#' @description Receiver operating characteristic (ROC) analysis is carried out
#' from the Power Lindley distribution. Specificity, sensitivity, area under the
#' curve, and ROC curve are evaluated.
#' @details
#'  The probability density function (PDF) and cumulative distribution function
#'  (CDF) are as follows:
#' \deqn{f\left( x\right) =\frac{\alpha \beta ^{2}}{\beta +1}\left( 1+x^{\alpha}
#' \right) x^{\alpha -1}\exp \left( -\beta x^{\alpha }\right)}
#' \deqn{=zg_{1}\left( t\right) +\left( 1-z\right) g_{2}\left( t\right)}
#'
#' \deqn{F\left( x\right) =P\left( X\leq x\right) =1-\left( 1+zx^{\alpha }
#' \right)
#' \exp \left( -\beta x^{\alpha }\right)}
#' and quantile function is given by,
#' \deqn{Q\left( u\right) =F^{-1}\left( u\right) =\left\{ -\frac{W\left( \left(
#' 1+\beta \right) \left( -1+u\right) \exp \left( -\left( 1+\beta \right)
#'                                               \right) \right) +1+\beta }
#'                                        {\beta }\right\} ^{\frac{1}{\alpha }}}
#' ,where
#' \deqn{z =\frac{\beta }{\beta +1}}
#' \deqn{g_{1}\left( x\right)  =\alpha \beta x^{\alpha -1}\exp \left( -\beta
#' x^{\alpha }\right)}
#' \deqn{g_{2}\left( x\right)  =\alpha \beta ^{2}x^{2\alpha -1}\exp \left(-\beta
#' x^{\alpha }\right)}
#' \eqn{\alpha>0} is a shape parameter, \eqn{\beta>0} is a scale parameter,
#' \eqn{0<u<1} and \eqn{W} is Lambert W function.
#'
#' Let \eqn{c} be the cut-off point, \eqn{X_{1}\sim PL\left( \alpha_{1},
#' \beta_{1}\right) }
#' and \eqn{X_{2}\sim PL\left( \alpha_{2},\beta_{2}\right) } with
#' \eqn{\alpha_{2}>\alpha_{1}}.
#' \eqn{F_{1}} and \eqn{F_{2}} are the cumulative distribution functions related
#'  to \eqn{X_{1}} and \eqn{X_{2}}, respectively.
#' In that case, the 1-specificity (False Positive Rate,FPR) and sensitivity
#' (True Positive Rate,TPR) are given by,
#' \deqn{FPR =P\left( X_{1}>c\right) =1-P\left( X_{1}\leq c\right) =1-F_{1}
#' \left(c\right)}
#' \deqn{TPR =1-F_{2}\left( Q_{1}\left( 1-FPR\right) \right)}
#' and Receiver Operating Characteristic (ROC) curve can be expressed
#' as follows:
#' \deqn{ROC =\left\{\left(r,( 1-F_{2}\left( Q_{1}\left(
#' 1-r; \boldsymbol{\theta }\right) \right),
#' r\in \left( 0,1\right) \right)\right\}},
#' where \eqn{\boldsymbol{\theta }=\left( \alpha _{1},\alpha _{2},
#' \beta _{1},\beta_{2}\right)}.
#'
#'Performance assesments for the ROC analysis are as follows:
#' \deqn{AUC =\int\limits_{0}^{1}ROCdr}
#' \deqn{J\left( c\right)  =\underset{c}{\arg \max }
#' \left\{ \left( 1-F_{2}\left( c\right)
#' \right) +F_{1}\left( c\right) -1\right\}}
#' \deqn{ER\left( c\right)  = \underset{c}{\arg \min }
#'  \left\{ \sqrt{\left( 1-F_{1}\left( c\right) \right)
#' ^{2}+\left( F_{2}\left( c\right) \right) ^{2}} \right\}}
#' \deqn{CZ\left( c\right)  = \underset{c}{\arg \max }
#' \left\{ \left( 1-F_{2}\left( c\right) \right) \times F_{1}\left(
#' c\right)  \right\}}
#' \deqn{IU\left( c\right)  =\left\{ \left\vert \left( 1-F_{2}\left( c\right)
#' -AUC\right) \right\vert +\left\vert F_{1}\left( c\right) -AUC\right\vert
#' \right\}}
#' \deqn{NI\left( c\right)  =\underset{c}{\arg \max }\left\{ \left( \left(
#' 1-F_{1}\left( c\right) \right) \times F_{2}\left( c\right) \times
#' F_{1}\left( c\right) \times \left( 1-F_{2}\left( c\right) \right) \right)
#' \right.   \nonumber \\
#' \left. +\left( F_{1}\left( c\right) \times F_{2}\left( c\right) \times
#'                 \left( 1-F_{1}\left( c\right) \right) \times
#'                 \left( 1-F_{2}\left( c\right)
#'                                                    \right) \right) \right\}}
#',where AUC is area under the ROC curve, J is Youden's J index, ER is the
#'closest to
#'\eqn{\left( 0,1\right)} criteria, CZ is the concordance probability method,
#' IU is index of Union and NI is new index.
#' @return  \code{dPLD} gives the probability density function of Power
#' Lindley Distribution.
#' @references
#' Akgenç, E., and Kuş, C., 2023,
#' *ROC Curve Analysis for the Measurements Distributed Power-Lindley*
#' *Distribution*,
#' 2nd International E-Conference On Mathematical And Statistical Sciences:
#' A Selçuk Meeting (ICOMSS-2023), Konya, 25.
#'
#' Attwood, K., Hou, S., and Hutson, A., 2022,
#' *Application of the skew exponential power distribution to ROC curves*,
#' Journal of Applied Statistics, 1-16.
#'
#' Ghitany M., Al-Mutairi D. K., Balakrishnan N., and Al-Enezi L., 2013,
#' *Power lindley distribution and associated inference*,
#' Computational Statistics & Data Analysis, 64,20–33.
#'
#' Liu, X., 2012,
#' *Classification accuracy and cut point selection*,
#' Statistics in medicine, 31(23), 2676-2686.
#'
#' Nahm, F. S., 2022,
#' *Receiver operating characteristic curve: overview and practical use for*
#' *clinicians*,
#' Korean journal of anesthesiology, 75(1), 25-36.
#'
#' Perkins, N. J., and Schisterman, E. F., 2006,
#' *The inconsistency of “optimal” cutpoints obtained using two*
#' *criteria based on the receiver operating characteristic curve*,
#' American journal of epidemiology, 163(7), 670-675.
#'
#' Pundir, S. and Amala, R., 2014,
#' *Evaluation of area under the constant shape bi-weibull roc curve*,
#' Journal of Modern Applied Statistical Methods, 13(1),1-20.
#'
#' Youden, W. J., 1950,
#' *Index for rating diagnostic tests*, Cancer, 3(1), 32-35.
#'
#' @examples
#' dPLD(c(1,2,3,4,5,200),alpha=3,beta=2)
dPLD<- function(x,alpha,beta) {
  if(any(alpha<=0)) {stop(paste("alpha value must be greather than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greather than 0","\n",""))}
  pdf<-NULL;for(i in seq_along(x))  {
    pdf[i]<-(alpha*(beta^2)*(1+(x[i]^alpha))*x[i]^(alpha-1)
             *exp(-beta*(x[i]^alpha))/(beta+1))
  }
  return(pdf)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{pPLD} gives the cumulative density function of
#' Power Lindley Distribution.
#' @examples
#' pPLD(c(.5,1,2,3,4),alpha=3,beta=2)
pPLD<-function(x,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greather than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greather than 0","\n",""))}
  cdf<-NULL;for (i in seq_along(x)) {
    ifelse(x[i]>0,
           cdf[i]<- 1-(1+(beta/(beta+1))*x[i]^alpha)*exp(-beta*(x[i]^alpha)),
           cdf[i]<- 0)
  }
  return(cdf)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{qPLD} gives the quantile function of
#' Power Lindley Distribution.
#' @examples
#' qPLD(c(.9971,0.5,0.3),alpha=3,beta=2)
qPLD<-function(p,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greather than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greather than 0","\n",""))}
  if(any(p<0)|any(p>1)) {stop("p must be between 0 and 1")}
  qqn<-NULL;for (i in seq_along(p)) {
    quant<-function(x) {cdf<- 1-(1+(beta/(beta+1))*x^alpha)*exp(-beta*(x^alpha))
    return(base::abs(cdf-p[i]))}
    qqn[i]<-suppressWarnings(stats::optim(1,quant)$par)
  }
  return(qqn)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{rPLD} gives random numbers from Power Lindley Distribution.
#' @examples
#' rPLD(10,alpha=3,beta=2)
rPLD<-function(n,alpha,beta){
  n<-base::floor(n)
  if(any(n<1)) {stop(paste("n value must be >=1","\n",""))}
  if(any(alpha<=0)) {stop(paste("alpha value must be greather than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greather than 0","\n",""))}
  rn<-qPLD(stats::runif(n),alpha,beta)
  return(rn)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plAUC} gives area under curve of Power
#' Lindley Distribution. Area under the ROC curve is obtained.
#' @examples
#' \donttest{plAUC(alpha1=2,beta1=5,alpha2=6,beta2=1)}
plAUC<- function(alpha1,beta1,alpha2,beta2)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  stats::integrate(function(c) {
    auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
    return(auc)
  },0,1)$value}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plJ} gives Youden's index for Power Lindley Distribution.
#'  Youden's J statistics is a way of summarising the performance of a
#'  diagnostic test.
#' @examples
#' plJ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
plJ<- function(alpha1,beta1,alpha2,beta2,init=0)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
    j<- function(c) {
    a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
    return(-a)
  }
  J<-stats::optim(init,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
  J$par
  -J$value
  return(J)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plER} gives the Closest to (0,1) Criteria (ER)
#' for Power Lindley Distribution. The Closest to (0,1) Criteria (ER)
#' is a way of summarising the performance of a diagnostic test.
#' @examples
#' plER(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
plER<- function(alpha1,beta1,alpha2,beta2,init=0)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  er<- function(c) {
    a<- base::sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
    return(a)
  }
  ER<-stats::optim(init,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
  ER$par
  -ER$value
  return(ER)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plCZ} gives the Concordance Probability Method
#' for Power Lindley Distribution. The Concordance Probability Method
#' is a way of summarising the performance of a diagnostic test.
#' @examples
#' plCZ(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)
plCZ<- function(alpha1,beta1,alpha2,beta2,init=0)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  cz<- function(c) {
    a<- (1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
    return(-a)
  }
  CZ<-stats::optim(init,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
  CZ$par
  -CZ$value
  return(CZ)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plIU} gives the Index of Union  for Power
#' Lindley Distribution. The Index of Union is a way of summarising
#' the performance of a diagnostic test.
#' @examples
#' \donttest{plIU(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0)}
plIU<- function(alpha1,beta1,alpha2,beta2,init=0)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  iu<- function(c) {
    a<- (base::abs((1-pPLD(c,alpha2,beta2))-plAUC(alpha1,beta1,alpha2,beta2))
         +base::abs((pPLD(c,alpha1,beta1))-plAUC(alpha1,beta1,alpha2,beta2)))
    return(a)
  }
  IU<-stats::optim(init,iu,method="L-BFGS-B",lower = -Inf,upper = Inf)
  IU$par
  -IU$value
  return(IU)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plNI} gives the New Index for Power Lindley Distribution.
#' @examples
#' \donttest{plNI(alpha1=2,beta1=5,alpha2=6,beta2=1,init=0.5)}
plNI<- function(alpha1,beta1,alpha2,beta2,init=0)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  ni<- function(c) {
    ni<-(((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))
          *pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+(pPLD(c,alpha1,beta1)
          *(1-(1-pPLD(c,alpha2,beta2)))*(1-pPLD(c,alpha1,beta1))
          *(1-pPLD(c,alpha2,beta2))))
    return(-ni)
  }
  NI<-stats::optim(init,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
  NI$par
  -NI$value
  return(NI)
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{plROC} gives the ROC graph for Power Lindley Distribution.
#' The ROC graph is given by 1-specificity versus sensitivity.
#' @examples
#' \donttest{x=c(1,2,3,4)}
#' \donttest{y=c(2,3,4)}
#' \donttest{plROC(x,y,alpha1=2,beta1=5,alpha2=6,beta2=1,empirical=FALSE)}
plROC<- function(x,y,alpha1,beta1,alpha2,beta2,empirical=TRUE)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  if (empirical==TRUE) {
  r<-seq(0.00001,1,0.00001)
  roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
  plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
       ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
  graphics::lines(c(0,1),c(0,1))
  oldpar <- graphics::par(new = TRUE)
  on.exit(graphics::par(oldpar))
  Fn<- stats::ecdf(y)
  roc2<- 1-Fn(stats::quantile(x,r))
  plot(1-r,roc2,type="l",lwd=2,ylim = c(0,1),xlim= c(0,1),
       ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
  graphics::legend('bottomright',inset=0.05,c("Empirical ROC Curve",
                        "Fitted ROC Curve"),lty=c(1,2),lwd = c(2,2),col=c(1,1))
  } else if (empirical==FALSE) {
    r<-seq(0.00001,1,0.00001)
    roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
    plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
         ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
    graphics::lines(c(0,1),c(0,1))
    graphics::legend('bottomright',inset=0.05,c("Fitted ROC Curve"),
                     lty=c(2),lwd = c(2),col=c(1))
  }
}
#' Receiver Operating Characteristic for Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{prfROC} gives sensitivity,specificity and 1-specificity
#' for Power Lindley Distribution.
#' @examples
#' prfROC(ctp=0.5,alpha1=2,beta1=5,alpha2=6,beta2=1)
prfROC<- function(ctp=0,alpha1,beta1,alpha2,beta2)
{
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  sen<-1-pPLD(ctp,alpha2,beta2)
  sp<-pPLD(ctp,alpha1,beta1)
  SP<-1-sp
  return(c("Sensitivitiy"=sen,"Specificity"=sp,"1-Specificity"=SP))
}
