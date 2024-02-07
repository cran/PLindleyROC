#' Receiver Operating Characteristic based on Power Lindley Distribution
#' @export
#' @name PLindleyROC
#' @param x,y vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'  to be the number required.
#' @param alpha shape parameter.
#' @param beta scale parameter.
#' @param alpha1 shape parameter of distribution of first sample.
#' @param beta1 scale parameter of distribution of first sample.
#' @param alpha2 shape parameter of distribution of second sample.
#' @param beta2 scale parameter of distribution of second sample.
#' @param init_index initial index value for the optimization calculation.
#' @param init_param initial paremeter values for the estimation method.
#' @param true_param true parameter values.
#' @param method estimation method. The default value for the method is "MLE".
#' @param empirical empirical must be TRUE or FALSE.
#' @description ROC curve analysis is performed assuming samples are from the
#' Power Lindley distribution. Specificity, sensitivity, area under the curve
#' and ROC curve are provided.
#' @details
#'  The probability density function (PDF) and cumulative distribution function
#'  (CDF) are as follows:
#' \deqn{f\left( x;\boldsymbol{\theta }\right) =\frac{\alpha \beta ^{2}}{
#' \beta +1}\left( 1+x^{\alpha}
#' \right) x^{\alpha -1}\exp \left( -\beta x^{\alpha }\right)}
#' \deqn{=zg_{1}\left( t\right) +\left( 1-z\right) g_{2}\left( t\right),}
#'
#' \deqn{F\left( x;\boldsymbol{\theta }\right) =P\left( X\leq x\right) =1-
#' \left( 1+zx^{\alpha }
#' \right)
#' \exp \left( -\beta x^{\alpha }\right),}
#' and quantile function is given by
#' \deqn{Q\left( u;\boldsymbol{\theta }\right) =F^{-1}\left( u;
#' \boldsymbol{\theta }\right) =\left\{ -\frac{W\left( \left(
#' 1+\beta \right) \left( -1+u\right) \exp \left( -\left( 1+\beta \right)
#'                                               \right) \right) +1+\beta }
#'                                      {\beta }\right\} ^{\frac{1}{\alpha }},}
#' where
#' \deqn{z =\frac{\beta }{\beta +1},}
#' \deqn{g_{1}\left( x\right)  =\alpha \beta x^{\alpha -1}\exp \left( -\beta
#' x^{\alpha }\right),}
#' \deqn{g_{2}\left( x\right)  =\alpha \beta ^{2}x^{2\alpha -1}\exp \left(-\beta
#' x^{\alpha }\right),}
#' \eqn{\boldsymbol{\theta =}\left( \alpha ,\beta \right) }, \eqn{0<u<1},
#' \eqn{\alpha>0} is a shape parameter, \eqn{\beta>0} is a scale parameter and
#' W(•) is Lambert W function.
#'
#'Additionally, the estimation methods Anderson-Darling "AD", Cramér-von Mises
#'"CvM", least squares "LS" and weighted least squares "WLS" as well as the
#'"TRUE" option for the true value, are available. Please note that the default
#'value for the method parameter is maximum likelihood "ML" estimation.
#'
#'The cut-off point values corresponding to Youden's J index (J), the criterion
#'closest to (0, 1) (ER), the concordance probability method (CZ), and the newly
#'proposed Ertan-Coskun index (EC) are provided.
#'
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
#' @return  \code{dPLD} gives the probability density function of Power
#' Lindley Distribution.
#' @examples
#' dPLD(c(1,2,3,4,5,200),alpha=3,beta=2)
dPLD<-function(x,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greather than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greather than 0","\n",""))}
  pdf<-NULL;for(i in seq_along(x)){
    pdf[i]<-(alpha*(beta^2)*(1+(x[i]^alpha))*x[i]^(alpha-1)
             *exp((-1)*beta*(x[i]^alpha))/(beta+1))
  }
  return(pdf)
}
#' Receiver Operating Characteristic based on Power Lindley Distribution
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
          cdf[i]<- 1-(1+(beta/(beta+1))*x[i]^alpha)*exp((-1)*beta*(x[i]^alpha)),
          cdf[i]<- 0)
  }
  return(cdf)
}
#' Receiver Operating Characteristic based on Power Lindley Distribution
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
#' Receiver Operating Characteristic based on Power Lindley Distribution
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
#' Receiver Operating Characteristic based on Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{r.pl_auc} gives the Area Under the Curve (AUC) when the data
#' conforms to the Power Lindley Distribution.
#' @examples
#' r.pl_auc(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),
#' true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),method=c("TRUE"))
r.pl_auc<- function(x,y,init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                 true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                 method=c("MLE","AD","CvM","LSE","WLSE","TRUE"))
{
  alpha1<-init_param[[1]]
  beta1<-init_param[[2]]
  alpha2<-init_param[[3]]
  beta2<-init_param[[4]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  method<- base::match.arg(method)

  if (method=="MLE") {
    lfxy<-function(par) {
      alpha1<-par[1]
      beta1<-par[2]
      alpha2<-par[3]
      beta2<-par[4]
      t<--sum(log(dPLD(x,alpha1,beta1)))-sum(log(dPLD(y,alpha2,beta2)))
      return(t)
    }
    mlexy<-try((stats::optim(c(alpha1,beta1,alpha2,beta2),lfxy,method="L-BFGS-B"
                             ,hessian = TRUE)),silent=TRUE)
    if (is.character(mlexy)) {
     stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- mlexy$par[1]
      beta1 <- mlexy$par[2]
      alpha2 <- mlexy$par[3]
      beta2 <- mlexy$par[4]
    }
    stats::integrate(function(c) {
      auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
      return(auc)
    },0,1)$value
  }
  else if (method=="AD") {
    QADx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      adx<-sort(x,decreasing=TRUE)
      n<-NROW(x)
      i<-seq(1:n)
      AD<--n-(1/n)*sum((2*i-1)*(log(pPLD(x,alpha1,beta1))+log(1-pPLD(adx,alpha1,
                                                                     beta1))))
      return(AD)
    }
    QADy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      ady<-sort(y,decreasing=TRUE)
      n<-NROW(y)
      i<-seq(1:n)
      AD<--n-(1/n)*sum((2*i-1)*(log(pPLD(y,alpha2,beta2))+log(1-pPLD(ady,alpha2,
                                                                     beta2))))
      return(AD)
    }
    adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
    adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
    if (is.character(adex)|is.character(adey)) {
     stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- adex$par[1]
      beta1 <- adex$par[2]
      alpha2 <- adey$par[1]
      beta2 <- adey$par[2]
    }
    stats::integrate(function(c) {
      auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
      return(auc)
    },0,1)$value
  }
  else if (method=="CvM") {
    QCVx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      CV<-1/(12*n)+sum((pPLD(x,alpha1,beta1)-(i-0.5)/n)^2)
      return(CV)
    }
    QCVy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      CV<-1/(12*n)+sum((pPLD(y,alpha2,beta2)-(i-0.5)/n)^2)
      return(CV)
    }
    cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
    cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
    if (is.character(cvx)|is.character(cvy)) {
     stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- cvx$par[1]
      beta1 <- cvx$par[2]
      alpha2 <- cvy$par[1]
      beta2 <- cvy$par[2]
    }
    stats::integrate(function(c) {
      auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
      return(auc)
    },0,1)$value
  }
  else if (method=="LSE") {
    QLSEx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      Q<-sum((pPLD(x,alpha1,beta1)-i/(n+1))^2)
      return(Q)
    }
    QLSEy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      Q<-sum((pPLD(y,alpha2,beta2)-i/(n+1))^2)
      return(Q)
    }
    lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
    lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
    if (is.character(lsex)|is.character(lsey)) {
     stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- lsex$par[1]
      beta1 <- lsex$par[2]
      alpha2 <- lsey$par[1]
      beta2 <- lsey$par[2]
    }
    stats::integrate(function(c) {
      auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
      return(auc)
    },0,1)$value
  }
  else if (method=="WLSE") {
    QWLSEx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pPLD(x,alpha1,beta1)-i/(n+1))^2)
      return(QW)
    }
    QWLSEy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pPLD(y,alpha2,beta2)-i/(n+1))^2)
      return(QW)
    }
    wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
    wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
    if (is.character(wlsex)|is.character(wlsey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- wlsex$par[1]
      beta1 <- wlsex$par[2]
      alpha2 <- wlsey$par[1]
      beta2 <- wlsey$par[2]
    }
    stats::integrate(function(c) {
      auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
      return(auc)
    },0,1)$value
  }
  else if (method=="TRUE") {
    x<-NULL
    y<-NULL
    init_param <- NULL
    alpha1<-true_param[[1]]
    beta1<-true_param[[2]]
    alpha2<-true_param[[3]]
    beta2<-true_param[[4]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
    stats::integrate(function(c) {
      auc<- 1-pPLD(qPLD(c,alpha1,beta1),alpha2,beta2)
      return(auc)
    },0,1)$value
  }
}
#' Receiver Operating Characteristic based on Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{r.pl_index} gives index values when the data conforms to the
#' Power Lindley Distribution.
#' @examples
#' r.pl_index(x=c(1,2,2,3,1),y=c(1,3,2,4,2,3),init_param=c(1,1,1,1),
#' init_index=1,method=c("MLE"))
r.pl_index<- function(x,y,init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      init_index=1,
                      true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                      method=c("MLE","AD","CvM","LSE","WLSE","TRUE"))
{
  alpha1<-init_param[[1]]
  beta1<-init_param[[2]]
  alpha2<-init_param[[3]]
  beta2<-init_param[[4]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  method<- base::match.arg(method)

  if (method=="MLE") {
    lfxy<-function(par) {
      alpha1<-par[1]
      beta1<-par[2]
      alpha2<-par[3]
      beta2<-par[4]
      t<--sum(log(dPLD(x,alpha1,beta1)))-sum(log(dPLD(y,alpha2,beta2)))
      return(t)
    }
  mlexy<-try((stats::optim(c(alpha1,beta1,alpha2,beta2),lfxy,method="L-BFGS-B",
                             hessian = TRUE)),silent=TRUE)
    if (is.character(mlexy)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- mlexy$par[1]
      beta1 <- mlexy$par[2]
      alpha2 <- mlexy$par[3]
      beta2 <- mlexy$par[4]
    }
    j<- function(c) {
      a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
      return(-a)
    }
    J<-stats::optim(init_index,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
    J$par
    -J$value
    er<-function(c){
      erx<-sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
      return(erx)
    }
    ER<-stats::optim(init_index,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
    ER$par
    -ER$value
    cz<-function(c){
      czx<-(1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
      return(-czx)
    }
    CZ<-stats::optim(init_index,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
    CZ$par
    -CZ$value
    ni<- function(c) {
      nix<-((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))*
              pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+
        (pPLD(c,alpha1,beta1)*(1-(1-pPLD(c,alpha2,beta2)))*
           (1-pPLD(c,alpha1,beta1))*(1-pPLD(c,alpha2,beta2)))
      return(-nix)
    }
    NI<-stats::optim(init_index,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
    NI$par
    -NI$value
    senJ<-1-pPLD(J$par,alpha2,beta2)
    spJ<-pPLD(J$par,alpha1,beta1)
    SPJ<-1-spJ
    senER<-1-pPLD(ER$par,alpha2,beta2)
    spER<-pPLD(ER$par,alpha1,beta1)
    SPER<-1-spER
    senCZ<-1-pPLD(CZ$par,alpha2,beta2)
    spCZ<-pPLD(CZ$par,alpha1,beta1)
    SPCZ<-1-spCZ
    senNI<-1-pPLD(NI$par,alpha2,beta2)
    spNI<-pPLD(NI$par,alpha1,beta1)
    SPNI<-1-spNI
    row1<-base::rbind(J$par,ER$par,CZ$par,NI$par)
    row2<-base::rbind(senJ,senER,senCZ,senNI)
    row3<-base::rbind(spJ,spER,spCZ,spNI)
    row4<-base::rbind(SPJ,SPER,SPCZ,SPNI)
    col<-base::cbind(row1,row2,row3,row4)
    base::colnames(col)<-c("Cut-off Point","Sensitivity",
                           "Specificity","1-Specificity")
    base::rownames(col)<-c("J","ER","CZ","EC")
    return(col)
  }
  else if (method=="AD") {
    QADx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      adx<-sort(x,decreasing=TRUE)
      n<-NROW(x)
      i<-seq(1:n)
      AD<--n-(1/n)*sum((2*i-1)*(log(pPLD(x,alpha1,beta1))+log(1-pPLD(adx,alpha1,
                                                                     beta1))))
      return(AD)
    }
    QADy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      ady<-sort(y,decreasing=TRUE)
      n<-NROW(y)
      i<-seq(1:n)
      AD<--n-(1/n)*sum((2*i-1)*(log(pPLD(y,alpha2,beta2))+log(1-pPLD(ady,alpha2,
                                                                     beta2))))
      return(AD)
    }
    adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
    adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
    if (is.character(adex)|is.character(adey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- adex$par[1]
      beta1 <- adex$par[2]
      alpha2 <- adey$par[1]
      beta2 <- adey$par[2]
    }
    j<- function(c) {
      a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
      return(-a)
    }
    J<-stats::optim(init_index,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
    J$par
    -J$value
    er<-function(c){
      erx<-sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
      return(erx)
    }
    ER<-stats::optim(init_index,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
    ER$par
    -ER$value
    cz<-function(c){
      czx<-(1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
      return(-czx)
    }
    CZ<-stats::optim(init_index,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
    CZ$par
    -CZ$value
    ni<- function(c) {
      nix<-((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))*
              pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+
        (pPLD(c,alpha1,beta1)*(1-(1-pPLD(c,alpha2,beta2)))*
           (1-pPLD(c,alpha1,beta1))*(1-pPLD(c,alpha2,beta2)))
      return(-nix)
    }
    NI<-stats::optim(init_index,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
    NI$par
    -NI$value
    senJ<-1-pPLD(J$par,alpha2,beta2)
    spJ<-pPLD(J$par,alpha1,beta1)
    SPJ<-1-spJ
    senER<-1-pPLD(ER$par,alpha2,beta2)
    spER<-pPLD(ER$par,alpha1,beta1)
    SPER<-1-spER
    senCZ<-1-pPLD(CZ$par,alpha2,beta2)
    spCZ<-pPLD(CZ$par,alpha1,beta1)
    SPCZ<-1-spCZ
    senNI<-1-pPLD(NI$par,alpha2,beta2)
    spNI<-pPLD(NI$par,alpha1,beta1)
    SPNI<-1-spNI
    row1<-base::rbind(J$par,ER$par,CZ$par,NI$par)
    row2<-base::rbind(senJ,senER,senCZ,senNI)
    row3<-base::rbind(spJ,spER,spCZ,spNI)
    row4<-base::rbind(SPJ,SPER,SPCZ,SPNI)
    col<-base::cbind(row1,row2,row3,row4)
    base::colnames(col)<-c("Cut-off Point","Sensitivity",
                           "Specificity","1-Specificity")
    base::rownames(col)<-c("J","ER","CZ","EC")
    return(col)
  }

  else if (method=="CvM") {
    QCVx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      CV<-1/(12*n)+sum((pPLD(x,alpha1,beta1)-(i-0.5)/n)^2)
      return(CV)
    }
    QCVy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      CV<-1/(12*n)+sum((pPLD(y,alpha2,beta2)-(i-0.5)/n)^2)
      return(CV)
    }
    cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
    cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
    if (is.character(cvx)|is.character(cvy)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- cvx$par[1]
      beta1 <- cvx$par[2]
      alpha2 <- cvy$par[1]
      beta2 <- cvy$par[2]
    }
    j<- function(c) {
      a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
      return(-a)
    }
    J<-stats::optim(init_index,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
    J$par
    -J$value
    er<-function(c){
      erx<-sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
      return(erx)
    }
    ER<-stats::optim(init_index,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
    ER$par
    -ER$value
    cz<-function(c){
      czx<-(1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
      return(-czx)
    }
    CZ<-stats::optim(init_index,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
    CZ$par
    -CZ$value
    ni<- function(c) {
      nix<-((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))*
              pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+
        (pPLD(c,alpha1,beta1)*(1-(1-pPLD(c,alpha2,beta2)))*
           (1-pPLD(c,alpha1,beta1))*(1-pPLD(c,alpha2,beta2)))
      return(-nix)
    }
    NI<-stats::optim(init_index,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
    NI$par
    -NI$value
    senJ<-1-pPLD(J$par,alpha2,beta2)
    spJ<-pPLD(J$par,alpha1,beta1)
    SPJ<-1-spJ
    senER<-1-pPLD(ER$par,alpha2,beta2)
    spER<-pPLD(ER$par,alpha1,beta1)
    SPER<-1-spER
    senCZ<-1-pPLD(CZ$par,alpha2,beta2)
    spCZ<-pPLD(CZ$par,alpha1,beta1)
    SPCZ<-1-spCZ
    senNI<-1-pPLD(NI$par,alpha2,beta2)
    spNI<-pPLD(NI$par,alpha1,beta1)
    SPNI<-1-spNI
    row1<-base::rbind(J$par,ER$par,CZ$par,NI$par)
    row2<-base::rbind(senJ,senER,senCZ,senNI)
    row3<-base::rbind(spJ,spER,spCZ,spNI)
    row4<-base::rbind(SPJ,SPER,SPCZ,SPNI)
    col<-base::cbind(row1,row2,row3,row4)
    base::colnames(col)<-c("Cut-off Point","Sensitivity",
                           "Specificity","1-Specificity")
    base::rownames(col)<-c("J","ER","CZ","EC")
    return(col)
  }

  else if (method=="LSE") {
    QLSEx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      Q<-sum((pPLD(x,alpha1,beta1)-i/(n+1))^2)
      return(Q)
    }
    QLSEy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      Q<-sum((pPLD(y,alpha2,beta2)-i/(n+1))^2)
      return(Q)
    }
    lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
    lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
    if (is.character(lsex)|is.character(lsey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- lsex$par[1]
      beta1 <- lsex$par[2]
      alpha2 <- lsey$par[1]
      beta2 <- lsey$par[2]
    }
    j<- function(c) {
      a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
      return(-a)
    }
    J<-stats::optim(init_index,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
    J$par
    -J$value
    er<-function(c){
      erx<-sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
      return(erx)
    }
    ER<-stats::optim(init_index,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
    ER$par
    -ER$value
    cz<-function(c){
      czx<-(1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
      return(-czx)
    }
    CZ<-stats::optim(init_index,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
    CZ$par
    -CZ$value
    ni<- function(c) {
      nix<-((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))*
              pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+
        (pPLD(c,alpha1,beta1)*(1-(1-pPLD(c,alpha2,beta2)))*
           (1-pPLD(c,alpha1,beta1))*(1-pPLD(c,alpha2,beta2)))
      return(-nix)
    }
    NI<-stats::optim(init_index,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
    NI$par
    -NI$value
    senJ<-1-pPLD(J$par,alpha2,beta2)
    spJ<-pPLD(J$par,alpha1,beta1)
    SPJ<-1-spJ
    senER<-1-pPLD(ER$par,alpha2,beta2)
    spER<-pPLD(ER$par,alpha1,beta1)
    SPER<-1-spER
    senCZ<-1-pPLD(CZ$par,alpha2,beta2)
    spCZ<-pPLD(CZ$par,alpha1,beta1)
    SPCZ<-1-spCZ
    senNI<-1-pPLD(NI$par,alpha2,beta2)
    spNI<-pPLD(NI$par,alpha1,beta1)
    SPNI<-1-spNI
    row1<-base::rbind(J$par,ER$par,CZ$par,NI$par)
    row2<-base::rbind(senJ,senER,senCZ,senNI)
    row3<-base::rbind(spJ,spER,spCZ,spNI)
    row4<-base::rbind(SPJ,SPER,SPCZ,SPNI)
    col<-base::cbind(row1,row2,row3,row4)
    base::colnames(col)<-c("Cut-off Point","Sensitivity",
                           "Specificity","1-Specificity")
    base::rownames(col)<-c("J","ER","CZ","EC")
    return(col)
  }

  else if (method=="WLSE") {
    QWLSEx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pPLD(x,alpha1,beta1)-i/(n+1))^2)
      return(QW)
    }
    QWLSEy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pPLD(y,alpha2,beta2)-i/(n+1))^2)
      return(QW)
    }
    wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
    wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
    if (is.character(wlsex)|is.character(wlsey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- wlsex$par[1]
      beta1 <- wlsex$par[2]
      alpha2 <- wlsey$par[1]
      beta2 <- wlsey$par[2]
    }
    j<- function(c) {
      a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
      return(-a)
    }
    J<-stats::optim(init_index,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
    J$par
    -J$value
    er<-function(c){
      erx<-sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
      return(erx)
    }
    ER<-stats::optim(init_index,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
    ER$par
    -ER$value
    cz<-function(c){
      czx<-(1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
      return(-czx)
    }
    CZ<-stats::optim(init_index,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
    CZ$par
    -CZ$value
    ni<- function(c) {
      nix<-((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))*
              pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+
        (pPLD(c,alpha1,beta1)*(1-(1-pPLD(c,alpha2,beta2)))*
           (1-pPLD(c,alpha1,beta1))*(1-pPLD(c,alpha2,beta2)))
      return(-nix)
    }
    NI<-stats::optim(init_index,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
    NI$par
    -NI$value
    senJ<-1-pPLD(J$par,alpha2,beta2)
    spJ<-pPLD(J$par,alpha1,beta1)
    SPJ<-1-spJ
    senER<-1-pPLD(ER$par,alpha2,beta2)
    spER<-pPLD(ER$par,alpha1,beta1)
    SPER<-1-spER
    senCZ<-1-pPLD(CZ$par,alpha2,beta2)
    spCZ<-pPLD(CZ$par,alpha1,beta1)
    SPCZ<-1-spCZ
    senNI<-1-pPLD(NI$par,alpha2,beta2)
    spNI<-pPLD(NI$par,alpha1,beta1)
    SPNI<-1-spNI
    row1<-base::rbind(J$par,ER$par,CZ$par,NI$par)
    row2<-base::rbind(senJ,senER,senCZ,senNI)
    row3<-base::rbind(spJ,spER,spCZ,spNI)
    row4<-base::rbind(SPJ,SPER,SPCZ,SPNI)
    col<-base::cbind(row1,row2,row3,row4)
    base::colnames(col)<-c("Cut-off Point","Sensitivity",
                           "Specificity","1-Specificity")
    base::rownames(col)<-c("J","ER","CZ","EC")
    return(col)
  }

  else if (method=="TRUE") {
    x<-NULL
    y<-NULL
    init_param <- NULL
    alpha1<-true_param[[1]]
    beta1<-true_param[[2]]
    alpha2<-true_param[[3]]
    beta2<-true_param[[4]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
    j<- function(c) {
      a<- pPLD(c,alpha1,beta1)-pPLD(c,alpha2,beta2)
      return(-a)
    }
    J<-stats::optim(init_index,j,method="L-BFGS-B",lower = -Inf,upper = Inf)
    J$par
    -J$value
    er<-function(c){
      erx<-sqrt(((pPLD(c,alpha2,beta2))^2)+((1-pPLD(c,alpha1,beta1))^2))
      return(erx)
    }
    ER<-stats::optim(init_index,er,method="L-BFGS-B",lower = -Inf,upper = Inf)
    ER$par
    -ER$value
    cz<-function(c){
      czx<-(1-pPLD(c,alpha2,beta2))*(pPLD(c,alpha1,beta1))
      return(-czx)
    }
    CZ<-stats::optim(init_index,cz,method="L-BFGS-B",lower = -Inf,upper = Inf)
    CZ$par
    -CZ$value
    ni<- function(c) {
      nix<-((1-pPLD(c,alpha1,beta1))*(1-(1-pPLD(c,alpha2,beta2)))*
              pPLD(c,alpha1,beta1)*(1-pPLD(c,alpha2,beta2)))+
        (pPLD(c,alpha1,beta1)*(1-(1-pPLD(c,alpha2,beta2)))*
           (1-pPLD(c,alpha1,beta1))*(1-pPLD(c,alpha2,beta2)))
      return(-nix)
    }
    NI<-stats::optim(init_index,ni,method="L-BFGS-B",lower = -Inf,upper = Inf)
    NI$par
    -NI$value
    senJ<-1-pPLD(J$par,alpha2,beta2)
    spJ<-pPLD(J$par,alpha1,beta1)
    SPJ<-1-spJ
    senER<-1-pPLD(ER$par,alpha2,beta2)
    spER<-pPLD(ER$par,alpha1,beta1)
    SPER<-1-spER
    senCZ<-1-pPLD(CZ$par,alpha2,beta2)
    spCZ<-pPLD(CZ$par,alpha1,beta1)
    SPCZ<-1-spCZ
    senNI<-1-pPLD(NI$par,alpha2,beta2)
    spNI<-pPLD(NI$par,alpha1,beta1)
    SPNI<-1-spNI
    row1<-base::rbind(J$par,ER$par,CZ$par,NI$par)
    row2<-base::rbind(senJ,senER,senCZ,senNI)
    row3<-base::rbind(spJ,spER,spCZ,spNI)
    row4<-base::rbind(SPJ,SPER,SPCZ,SPNI)
    col<-base::cbind(row1,row2,row3,row4)
    base::colnames(col)<-c("Cut-off Point","Sensitivity",
                           "Specificity","1-Specificity")
    base::rownames(col)<-c("J","ER","CZ","EC")
    return(col)

  }
}
#' Receiver Operating Characteristic based on Power Lindley Distribution
#' @export
#' @rdname PLindleyROC
#' @return \code{r.pl_graph} gives the ROC curve when the data conforms to the
#' Power Lindley Distribution.
#' @examples
#' \donttest{x=c(1,2,2,3,1)}
#' \donttest{y=c(1,3,2,4,2,3)}
#' \donttest{r.pl_graph(x,y,init_param=c(1,1,1,1),
#' empirical=TRUE,method=c("MLE"))}
r.pl_graph<- function(x,y,init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                 true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1),
                 empirical=TRUE,method=c("MLE","AD","CvM","LSE","WLSE","TRUE"))
{
  alpha1<-init_param[[1]]
  beta1<-init_param[[2]]
  alpha2<-init_param[[3]]
  beta2<-init_param[[4]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
  method<- base::match.arg(method)

  if (method=="MLE") {
    lfxy<-function(par) {
      alpha1<-par[1]
      beta1<-par[2]
      alpha2<-par[3]
      beta2<-par[4]
      t<--sum(log(dPLD(x,alpha1,beta1)))-sum(log(dPLD(y,alpha2,beta2)))
      return(t)
    }
  mlexy<-try((stats::optim(c(alpha1,beta1,alpha2,beta2),lfxy,method="L-BFGS-B",
                             hessian = TRUE)),silent=TRUE)
    if (is.character(mlexy)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- mlexy$par[1]
      beta1 <- mlexy$par[2]
      alpha2 <- mlexy$par[3]
      beta2 <- mlexy$par[4]
    }
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
      graphics::legend('bottomright',inset=0.05,
                       c("Empirical ROC Curve","Fitted ROC Curve"),
                       lty=c(1,2),lwd = c(2,2),col=c(1,1))
    } else if (empirical==FALSE) {
      r<-seq(0.00001,1,0.00001)
      roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
      plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
           ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
      graphics::lines(c(0,1),c(0,1))
      graphics::legend('bottomright',inset=0.05,
                       c("Fitted ROC Curve"),lty=c(2),lwd = c(2),col=c(1))
    }
  }
  else if (method=="AD") {
    QADx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      adx<-sort(x,decreasing=TRUE)
      n<-NROW(x)
      i<-seq(1:n)
      AD<--n-(1/n)*sum((2*i-1)*(log(pPLD(x,alpha1,beta1))+log(1-pPLD(adx,alpha1,
                                                                     beta1))))
      return(AD)
    }
    QADy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      ady<-sort(y,decreasing=TRUE)
      n<-NROW(y)
      i<-seq(1:n)
      AD<--n-(1/n)*sum((2*i-1)*(log(pPLD(y,alpha2,beta2))+log(1-pPLD(ady,alpha2,
                                                                     beta2))))
      return(AD)
    }
    adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
    adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
    if (is.character(adex)|is.character(adey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- adex$par[1]
      beta1 <- adex$par[2]
      alpha2 <- adey$par[1]
      beta2 <- adey$par[2]
    }
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
      graphics::legend('bottomright',inset=0.05,
                       c("Empirical ROC Curve","Fitted ROC Curve"),
                       lty=c(1,2),lwd = c(2,2),col=c(1,1))
    } else if (empirical==FALSE) {
      r<-seq(0.00001,1,0.00001)
      roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
      plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
           ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
      graphics::lines(c(0,1),c(0,1))
      graphics::legend('bottomright',inset=0.05,
                       c("Fitted ROC Curve"),lty=c(2),lwd = c(2),col=c(1))
    }
  }
  else if (method=="CvM") {
    QCVx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      CV<-1/(12*n)+sum((pPLD(x,alpha1,beta1)-(i-0.5)/n)^2)
      return(CV)
    }
    QCVy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      CV<-1/(12*n)+sum((pPLD(y,alpha2,beta2)-(i-0.5)/n)^2)
      return(CV)
    }
    cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
    cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
    if (is.character(cvx)|is.character(cvy)) {
     stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- cvx$par[1]
      beta1 <- cvx$par[2]
      alpha2 <- cvy$par[1]
      beta2 <- cvy$par[2]
    }
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
      graphics::legend('bottomright',inset=0.05,
                       c("Empirical ROC Curve","Fitted ROC Curve"),
                       lty=c(1,2),lwd = c(2,2),col=c(1,1))
    } else if (empirical==FALSE) {
      r<-seq(0.00001,1,0.00001)
      roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
      plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
           ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
      graphics::lines(c(0,1),c(0,1))
      graphics::legend('bottomright',inset=0.05,
                       c("Fitted ROC Curve"),lty=c(2),lwd = c(2),col=c(1))
    }
  }
  else if (method=="LSE") {
    QLSEx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      Q<-sum((pPLD(x,alpha1,beta1)-i/(n+1))^2)
      return(Q)
    }
    QLSEy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      Q<-sum((pPLD(y,alpha2,beta2)-i/(n+1))^2)
      return(Q)
    }
    lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
    lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
    if (is.character(lsex)|is.character(lsey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- lsex$par[1]
      beta1 <- lsex$par[2]
      alpha2 <- lsey$par[1]
      beta2 <- lsey$par[2]
    }
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
      graphics::legend('bottomright',inset=0.05,
                       c("Empirical ROC Curve","Fitted ROC Curve"),
                       lty=c(1,2),lwd = c(2,2),col=c(1,1))
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
  else if (method=="WLSE") {
    QWLSEx<-function(par,x){
      alpha1<-par[1]
      beta1<-par[2]
      x<-sort(x)
      n<-NROW(x)
      i<-seq(1:n)
      QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pPLD(x,alpha1,beta1)-i/(n+1))^2)
      return(QW)
    }
    QWLSEy<-function(par,y){
      alpha2<-par[1]
      beta2<-par[2]
      y<-sort(y)
      n<-NROW(y)
      i<-seq(1:n)
      QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pPLD(y,alpha2,beta2)-i/(n+1))^2)
      return(QW)
    }
    wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
    wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
    if (is.character(wlsex)|is.character(wlsey)) {
    stop("Optimization did not converge.Please check your initial parameters.")
    } else {
      alpha1 <- wlsex$par[1]
      beta1 <- wlsex$par[2]
      alpha2 <- wlsey$par[1]
      beta2 <- wlsey$par[2]
    }
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
      graphics::legend('bottomright',inset=0.05,
                       c("Empirical ROC Curve","Fitted ROC Curve"),
                       lty=c(1,2),lwd = c(2,2),col=c(1,1))
    } else if (empirical==FALSE) {
      r<-seq(0.00001,1,0.00001)
      roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
      plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
           ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
      graphics::lines(c(0,1),c(0,1))
      graphics::legend('bottomright',inset=0.05,
                       c("Fitted ROC Curve"),lty=c(2),lwd = c(2),col=c(1))
    }
  }
  else if (method=="TRUE") {
    x<-NULL
    y<-NULL
    init_param <- NULL
    empirical<-NULL
    alpha1<-true_param[[1]]
    beta1<-true_param[[2]]
    alpha2<-true_param[[3]]
    beta2<-true_param[[4]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greather than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greather than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greather than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greather than 0","\n",""))}
    r<-seq(0.00001,1,0.00001)
    roc<-1-pPLD(qPLD(r,alpha1,beta1),alpha2,beta2)
    plot(1-r,roc,type="l", lty=2, lwd=2,ylim = c(0,1),xlim= c(0,1),
         ylab = "Sensitivity",xlab = "1-Specificity",col=1,main="ROC Graph")
    graphics::lines(c(0,1),c(0,1))
    graphics::legend('bottomright',inset=0.05,
                     c("Fitted ROC Curve"),lty=c(2),lwd = c(2),col=c(1))
  }
}
