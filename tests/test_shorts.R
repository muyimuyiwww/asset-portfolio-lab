.libPaths(c('.Rlib',.libPaths()));source('R/optimize.R')
mu<-c(-.02,.2);S<-diag(c(.04,.09));o<-optimize_portfolio(mu,S,allow_short=TRUE)
stopifnot(max(abs(o$tangent-c(-1,2)))<1e-10,o$tangent[1]< -.2,o$tangent[2]>1)
for(w in c(list(o$gmv,o$tangent),o$weights))stopifnot(abs(sum(w)-1)<1e-7)
stopifnot(o$tangent_stats['sharpe']>=max(o$frontier$sharpe)-1e-8)
# Independently verify the entire two-asset variance curve.
w1<-(mu[2]-o$frontier$return)/(mu[2]-mu[1]);expected<-w1^2*S[1,1]+(1-w1)^2*S[2,2]
stopifnot(max(abs(o$frontier$volatility^2-expected))<1e-10)
S2<-matrix(c(.04,.05,.05,.09),2);g<-optimize_portfolio(c(.1,.2),S2,allow_short=TRUE)$gmv
stopifnot(max(abs(g-c(4/3,-1/3)))<1e-10)
long<-optimize_portfolio(mu,S);stopifnot(all(long$gmv>=0),all(long$tangent>=0))
none<-optimize_portfolio(mu,S,rf=.2,allow_short=TRUE);stopifnot(is.null(none$tangent),grepl('没有有限权重',none$tangent_reason),isTRUE(none$unbounded_frontier))
flat<-optimize_portfolio(c(.1,.1),S,allow_short=TRUE);stopifnot(max(abs(flat$tangent-flat$gmv))<1e-10,nrow(flat$frontier)==1)
cat('PASS unrestricted shorting, -100%/+200% weights, analytic variance and tangency, negative GMV, no finite tangent, identical returns\n')
