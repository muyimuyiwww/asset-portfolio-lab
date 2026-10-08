portfolio_stats <- function(w,mu,sigma,rf) {
  r <- sum(w*mu); v <- sqrt(drop(t(w)%*%sigma%*%w))
  c(return=r,volatility=v,sharpe=if(v>0)(r-rf)/v else NA_real_)
}
optimize_portfolio <- function(mu,sigma,rf=.02,points=100,allow_short=FALSE,include_rf=TRUE) {
  n <- length(mu)
  bounds <- rep(0,n)
  budget <- 1
  if(any(!is.finite(sigma))||min(eigen(sigma,symmetric=TRUE,only.values=TRUE)$values)<=max(diag(sigma))*1e-10)
    stop('协方差矩阵奇异或近奇异。请更换高度重复的股票或延长样本；工具不会自动改变协方差。')
  if(isTRUE(allow_short)) {
    # Equality-only Markowitz frontier, with no position or leverage bounds.
    one<-rep(1,n);v1<-base::solve(sigma,one);vm<-base::solve(sigma,mu)
    A<-sum(v1);g<-v1/A;gm<-sum(g*mu);direction<-vm-gm*v1
    spread<-sum(mu*direction);flat<-diff(range(mu))<1e-12
    at<-function(target) {
      w<-if(flat)g else g+(target-gm)*direction/spread
      if(any(!is.finite(w))||abs(sum(w)-1)>1e-6||abs(sum(w*mu)-target)>1e-6)stop('无约束前沿未通过权重或收益校验。')
      w
    }
    tangent<-NULL;reason<-NULL
    if(include_rf&&gm>rf) {
      tangent<-if(flat)g else (vm-rf*v1)/(A*(gm-rf))
      if(any(!is.finite(tangent))||abs(sum(tangent)-1)>1e-6)stop('无约束切点权重未通过校验。')
    } else if(include_rf)reason<-if(flat)'所有股票的预期收益相同且不超过无风险利率，没有正斜率切点。' else '允许无限制卖空时，当前参数下最大 Sharpe 只在持仓规模趋于无穷时逼近，没有有限权重的切点组合。'
    # This is a plotting window, never a constraint on the optimization.
    span<-max(.1,diff(range(mu)),3*portfolio_stats(g,mu,sigma,rf)['volatility'])
    upper<-if(flat)gm else max(gm+span,if(is.null(tangent))gm else sum(tangent*mu)+span*.1)
    targets<-if(flat)gm else seq(gm,upper,length.out=points)
    weights<-lapply(targets,at)
    frontier<-as.data.frame(t(vapply(weights,portfolio_stats,numeric(3),mu=mu,sigma=sigma,rf=rf)))
    return(list(gmv=g,tangent=tangent,frontier=frontier,weights=weights,gmv_stats=portfolio_stats(g,mu,sigma,rf),tangent_stats=if(!is.null(tangent))portfolio_stats(tangent,mu,sigma,rf),tangent_reason=reason,unbounded_frontier=!flat))
  }
  solve <- function(target=NULL) {
    A <- cbind(rep(1,n),diag(n)); b <- c(1,bounds); eq <- 1
    if(!is.null(target)&&diff(range(mu))>1e-12) {A<-cbind(rep(1,n),mu,diag(n));b<-c(1,target,bounds);eq<-2}
    q <- tryCatch(quadprog::solve.QP(2*sigma,rep(0,n),A,b,meq=eq),error=function(e)stop('最小方差求解失败：',conditionMessage(e)))
    w <- q$solution
    if(any(w<bounds-1e-7)||abs(sum(w)-1)>1e-7||(!is.null(target)&&abs(sum(w*mu)-target)>1e-6)) stop('求解未通过权重或目标收益校验。')
    w[abs(w)<1e-10]<-0; w
  }
  g <- solve(); lower <- sum(g*mu); upper <- sum(bounds*mu)+budget*max(mu)
  endpoint <- function() {
    ii<-which(abs(mu-max(mu))<1e-12); ww<-bounds
    if(length(ii)==1)ww[ii]<-ww[ii]+budget else {
      ww[ii]<-ww[ii]+quadprog::solve.QP(2*sigma[ii,ii,drop=FALSE],-2*drop(sigma[ii,,drop=FALSE]%*%bounds),cbind(rep(1,length(ii)),diag(length(ii))),c(budget,rep(0,length(ii))),meq=1)$solution
    };ww
  }
  at <- function(t) {w<-if(abs(t-upper)<1e-10)endpoint() else solve(t);if(any(!is.finite(w))||any(w<bounds-1e-7)||abs(sum(w)-1)>1e-7||abs(sum(w*mu)-t)>1e-6)stop('前沿端点或权重未通过校验。');w}
  targets <- if(upper-lower<1e-10)lower else seq(lower,upper,length.out=points)
  weights <- lapply(targets,at)
  frontier <- as.data.frame(t(vapply(weights,portfolio_stats,numeric(3),mu=mu,sigma=sigma,rf=rf)))
  tangent <- NULL
  if(include_rf&&upper>rf) {
    if(upper-lower<1e-10)tangent<-g else {
      obj <- function(t) -portfolio_stats(at(t),mu,sigma,rf)['sharpe']
      opt <- optimize(obj,c(lower,upper),tol=1e-10)
      candidates<-c(lower,opt$minimum,upper);tangent<-at(candidates[which.min(vapply(candidates,obj,numeric(1)))])
    }
  }
  list(gmv=g,tangent=tangent,frontier=frontier,weights=weights,gmv_stats=portfolio_stats(g,mu,sigma,rf),tangent_stats=if(!is.null(tangent))portfolio_stats(tangent,mu,sigma,rf),tangent_reason=if(include_rf&&is.null(tangent))'当前不卖空约束下组合预期收益不超过无风险利率，没有正斜率切点。' else NULL)
}
