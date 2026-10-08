estimate_returns <- function(x, markets, dates, benchmarks, method='历史均值', rf=.02, half_life=12,frequency='monthly') {
  if(!frequency %in% c('daily','monthly','annual'))stop('收益频率无效。')
  factor<-switch(frequency,daily=252,monthly=12,annual=1)
  minimum<-max(ncol(x)+1,switch(frequency,daily=30,monthly=12,annual=3))
  if(nrow(x)<minimum)stop(sprintf('共同有效样本至少需要%d%s。',minimum,switch(frequency,daily='个共同交易日',monthly='个月',annual='个完整自然年')))
  if(any(!is.finite(x)))stop('共同样本中仍有缺失或无效收益。')
  if(!is.finite(rf)||rf<0||rf>1)stop('无风险年利率需介于0%与100%。')
  if(!is.finite(half_life)||half_life<=0)stop('半衰期必须为正数。')
  if(!method %in% c('历史均值','指数加权','CAPM'))stop('请选择支持的估计方法。')
  stopifnot(length(markets)==ncol(x))
  sigma <- cov(x)*factor
  beta <- rep(NA_real_,ncol(x))
  if(method=='历史均值') mu <- colMeans(x)*factor else if(method=='指数加权') {
    age <- as.integer(format(max(dates),'%Y'))*12+as.integer(format(max(dates),'%m'))-(as.integer(format(dates,'%Y'))*12+as.integer(format(dates,'%m')))
    if(frequency=='daily')age<-as.numeric(max(dates)-dates)/(365.25/12)
    if(frequency=='annual')age<-(as.integer(format(max(dates),'%Y'))-as.integer(format(dates,'%Y')))*12
    w <- 2^(-age/half_life); mu <- drop(crossprod(w/sum(w),x))*factor
  } else {
    for(j in seq_len(ncol(x))) {
      b <- benchmarks[,markets[j]]-rf/factor
      if(var(b)<1e-14) stop('市场代理收益没有足够变化，无法估计 CAPM。')
      beta[j] <- coef(lm(I(x[,j]-rf/factor)~b))[2]
    }
    mu <- rf+beta*(colMeans(benchmarks)[markets]*factor-rf)
  }
  list(mu=setNames(as.numeric(mu),colnames(x)),sigma=sigma,beta=beta)
}
