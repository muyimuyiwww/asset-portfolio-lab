.libPaths(c('.Rlib',.libPaths()));source('R/estimate.R');source('R/data.R');source('R/optimize.R')
set.seed(18)
for(f in c('daily','monthly','annual')){
 n<-switch(f,daily=80,monthly=24,annual=10);factor<-switch(f,daily=252,monthly=12,annual=1)
 dates<-seq(as.Date('2010-01-01'),by=switch(f,daily='day',monthly='month',annual='year'),length.out=n)
 b<-rnorm(n,.06/factor,.1/sqrt(factor));x<-cbind(a=.02/factor+.001+1.3*(b-.02/factor),b=rnorm(n,.08/factor,.2/sqrt(factor)))
 h<-estimate_returns(x,c('A','A'),dates,cbind(A=b),frequency=f)
 stopifnot(max(abs(h$mu-colMeans(x)*factor))<1e-12,max(abs(h$sigma-cov(x)*factor))<1e-12)
 c<-estimate_returns(x,c('A','A'),dates,cbind(A=b),method='CAPM',frequency=f)
 stopifnot(abs(c$beta[1]-1.3)<1e-10,abs(c$mu[1]-(.02+1.3*(mean(b)*factor-.02)))<1e-10)
 age<-switch(f,daily=as.numeric(max(dates)-dates)/(365.25/12),monthly=(n-1):0,annual=((n-1):0)*12);w<-2^(-age/12)
 e<-estimate_returns(x,c('A','A'),dates,cbind(A=b),method='指数加权',frequency=f)
 stopifnot(max(abs(e$mu-drop(crossprod(w/sum(w),x))*factor))<1e-12)
}
cat('PASS synthetic frequency estimation and annualization\n')
if(Sys.getenv('RUN_LIVE_TESTS')=='1'){
assets<-data.frame(market=c('A','H','US'),ticker=c('600519','00700','AAPL'))
for(f in c('daily','monthly','annual')){
 d<-fetch_data(assets,as.Date('2021-10-01'),as.Date('2026-09-30'),frequency=f)
 e<-estimate_returns(d$x,d$assets$market,d$dates,d$b,frequency=f);o<-optimize_portfolio(e$mu,e$sigma,allow_short=TRUE)
 stopifnot(all(is.finite(e$mu)),abs(sum(o$gmv)-1)<1e-8,d$frequency==f)
 cat(f,nrow(d$x),'real cached observations PASS\n')
}

}
