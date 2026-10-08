.libPaths(c('.Rlib',.libPaths()));source('R/optimize.R');source('R/model.R');source('R/plot.R')
assets<-'A,8,20\nB,14,30'
corr<-'1,0.2\n0.2,1'
e<-manual_model(assets,corr)
S<-matrix(c(.04,.012,.012,.09),2)
stopifnot(max(abs(e$mu-c(.08,.14)))<1e-12,max(abs(e$sigma-S))<1e-12)
cov_model<-manual_model('A,8\nB,14','0.04,0.012\n0.012,0.09','covariance')
stopifnot(max(abs(cov_model$sigma-e$sigma))<1e-12)
stopifnot(identical(manual_model(paste('资产,收益,标准差',assets,sep='\n'),corr)$mu,e$mu))
reject<-function(a,m,kind='correlation')inherits(try(manual_model(a,m,kind),silent=TRUE),'try-error')
stopifnot(reject('A,8,20\nA,14,30',corr),reject(assets,'1,0.2\n0.3,1'),reject(assets,'1,1.2\n1.2,1'),reject(assets,'1,1\n1,1'),reject('A,8,0\nB,14,30',corr),reject(assets,'1,0,0\n0,1,0'),reject(assets,'1,Inf\nInf,1'),reject(assets,'0.04,0.2\n0.2,0.09','covariance'))
rf<-.02;target<-.06
inv<-solve(S);excess<-e$mu-rf
analytic<-drop(inv%*%excess)*(target-rf)/drop(t(excess)%*%inv%*%excess)
w<-cash_target_weights(e$mu,S,rf,target,FALSE,FALSE)
stopifnot(max(abs(w$risky-analytic))<1e-10,abs(sum(w$risky)+w$cash-1)<1e-12,w$cash>0)
w_high<-cash_target_weights(e$mu,S,rf,.13,FALSE,FALSE)
# Independent two-asset long-only solution above tangency: no cash remains.
stopifnot(abs(w_high$cash)<1e-8,max(abs(w_high$risky-c(1/6,5/6)))<1e-8)
borrow<-cash_target_weights(e$mu,S,rf,.25,FALSE,TRUE)
stopifnot(borrow$cash<0,abs(complete_stats(borrow$risky,borrow$cash,e$mu,S,rf)[1]-.25)<1e-10)
stopifnot(inherits(try(cash_target_weights(e$mu,S,rf,.25,FALSE,FALSE),silent=TRUE),'try-error'))
for(short in c(FALSE,TRUE))for(loan in c(FALSE,TRUE)) {
 s<-list(est=e,opt=optimize_portfolio(e$mu,S,rf,allow_short=short),rf=rf,allow_short=short,has_rf=TRUE,allow_borrow=loan,frequency='manual',input_mode='manual',data=list(dates=as.Date(character())))
 s$samples<-feasible_samples(s);v<-frontier_view(s)
 stopifnot(max(abs(rowSums(s$samples$weights)+s$samples$cash-1))<1e-10)
 if(!short)stopifnot(min(s$samples$weights)> -1e-10)
 if(!loan)stopifnot(min(s$samples$cash)>= -1e-10)
 for(branch in c('upper','lower'))for(p in v$cash[[branch]]$weights){stopifnot(abs(sum(p$risky)+p$cash-1)<1e-8);if(!short)stopifnot(min(p$risky)>= -1e-8);if(!loan)stopifnot(p$cash>= -1e-8)}
 # Query every represented type and independently match its plotted point.
 for(kind in c('risky','lower','cash_upper','cash_lower','sample')) {
  rows<-switch(kind,risky=v$frontier,lower=v$lower,cash_upper=v$cash$upper$stats,cash_lower=v$cash$lower$stats,sample=v$samples$stats)
  i<-min(5,nrow(rows));p<-portfolio_at_id(paste0(kind,':',i),s,v)
  st<-complete_stats(p$risky,p$cash,e$mu,S,rf)
  stopifnot(max(abs(st[1:2]-unlist(rows[i,1:2])))<1e-8)
 }
 stopifnot(portfolio_at_id('rf',s,v)$cash==1,is.null(portfolio_at_id('sample:99999',s,v)))
}
# When risky expected returns are all below rf, long-only efficient set is cash alone.
s$est$mu<-c(A=-.1,B=-.05);s$allow_short<-FALSE;s$allow_borrow<-FALSE;s$opt<-optimize_portfolio(s$est$mu,S,rf)
v<-frontier_view(s);stopifnot(nrow(v$cash$upper$stats)==1,v$cash$upper$stats$volatility==0)
# Flat expected returns and no finite risky tangent still produce a valid cash frontier.
s$est$mu<-c(A=.01,B=.01);s$allow_short<-TRUE;s$opt<-optimize_portfolio(s$est$mu,S,rf,allow_short=TRUE)
v<-frontier_view(s);stopifnot(all(is.finite(v$cash$upper$stats$return)),is.null(s$opt$tangent))
cat('PASS external inputs, malformed matrices, analytic cash allocation, borrowing constraints, opportunity samples and point weights\n')
