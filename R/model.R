# External model inputs are annual arithmetic returns and annual covariances.
manual_model <- function(asset_text, matrix_text, matrix_kind='correlation') {
  lines <- trimws(strsplit(gsub('，', ',', asset_text, fixed=TRUE), '\n')[[1]])
  lines <- lines[nzchar(lines)]
  rows <- lapply(lines, function(x) trimws(strsplit(x, ',', fixed=TRUE)[[1]]))
  if(length(rows) && grepl('^(资产|资产名称|asset)$', rows[[1]][1], ignore.case=TRUE) &&
     length(rows[[1]])>1 && !is.finite(suppressWarnings(as.numeric(rows[[1]][2])))) rows <- rows[-1]
  n <- length(rows)
  required <- if(matrix_kind=='correlation')3 else 2
  if(n<2 || n>10 || any(lengths(rows)<required) || any(lengths(rows)>3))
    stop(if(required==3)'请输入2–10行：资产名称,预期收益(%),标准差(%)。' else '请输入2–10行：资产名称,预期收益(%)。')
  labels <- vapply(rows, `[`, character(1), 1)
  if(any(!nzchar(labels)) || anyDuplicated(labels))stop('资产名称不能为空或重复。')
  mu <- suppressWarnings(as.numeric(vapply(rows, `[`, character(1), 2)))/100
  if(any(!is.finite(mu)))stop('预期收益必须是有限数值，单位为%。')
  mlines <- trimws(strsplit(gsub('，', ',', matrix_text, fixed=TRUE), '\n')[[1]])
  mlines <- mlines[nzchar(mlines)]
  cells <- lapply(mlines, function(x) strsplit(trimws(x), '[,;[:space:]]+')[[1]])
  if(length(cells)!=n || any(lengths(cells)!=n))stop(sprintf('矩阵必须是%d行%d列，并与资产顺序一致。', n,n))
  mat <- matrix(suppressWarnings(as.numeric(unlist(cells))), nrow=n, byrow=TRUE)
  if(any(!is.finite(mat)))stop('矩阵只能包含有限数值。')
  asymmetric<-which(upper.tri(mat)&abs(mat-t(mat))>1e-8*max(1,max(abs(mat))),arr.ind=TRUE)
  if(nrow(asymmetric)>0) {
    i<-asymmetric[1,1];j<-asymmetric[1,2]
    value<-function(x)format(x,digits=12,trim=TRUE)
    stop(sprintf('矩阵不对称：%s与%s之间，第%d行第%d列为%s，第%d行第%d列为%s。请将这两个位置填成相同的值。',labels[i],labels[j],i,j,value(mat[i,j]),j,i,value(mat[j,i])))
  }
  if(matrix_kind=='correlation') {
    sd <- suppressWarnings(as.numeric(vapply(rows, `[`, character(1), 3)))/100
    if(any(!is.finite(sd)) || any(sd<=0))stop('标准差必须为正数，单位为%。')
    if(any(abs(mat)>1+1e-10) || max(abs(diag(mat)-1))>1e-8)stop('相关系数需在−1至1之间，对角线必须为1。')
    sigma <- diag(sd)%*%mat%*%diag(sd)
  } else if(matrix_kind=='covariance') {
    sigma <- mat
    if(any(diag(sigma)<=0))stop('协方差矩阵对角线上的方差必须为正数。')
  } else stop('请选择相关系数或协方差矩阵。')
  if(min(eigen(sigma,symmetric=TRUE,only.values=TRUE)$values)<=max(diag(sigma))*1e-10)
    stop('矩阵必须正定，不能奇异或近奇异；请检查各项相关系数或协方差是否一致。')
  dimnames(sigma)<-list(labels,labels)
  list(mu=setNames(mu,labels),sigma=sigma,beta=rep(NA_real_,n))
}

risky_target_weights <- function(mu,sigma,target,allow_short=FALSE) {
  n<-length(mu)
  if(diff(range(mu))<1e-12) {
    if(abs(target-mu[1])>1e-8)stop('目标收益不可行。')
    return(optimize_portfolio(mu,sigma,allow_short=allow_short)$gmv)
  }
  if(allow_short) {
    v1<-solve(sigma,rep(1,n));g<-v1/sum(v1);gm<-sum(g*mu)
    direction<-solve(sigma,mu)-gm*v1
    return(g+(target-gm)*direction/sum(mu*direction))
  }
  endpoint<-which(abs(mu-target)<1e-10)
  if(abs(target-min(mu))<1e-10 || abs(target-max(mu))<1e-10) {
    w<-numeric(n)
    w[endpoint]<-if(length(endpoint)==1)1 else quadprog::solve.QP(2*sigma[endpoint,endpoint,drop=FALSE],rep(0,length(endpoint)),cbind(rep(1,length(endpoint)),diag(length(endpoint))),c(1,rep(0,length(endpoint))),meq=1)$solution
    return(w)
  }
  quadprog::solve.QP(2*sigma,rep(0,n),cbind(rep(1,n),mu,diag(n)),c(1,target,rep(0,n)),meq=2)$solution
}

# Eliminating cash avoids a singular covariance matrix. Cash = 1 - sum(w).
cash_target_weights <- function(mu,sigma,rf,target,allow_short=FALSE,allow_borrow=FALSE) {
  n<-length(mu);excess<-mu-rf;delta<-target-rf
  if(abs(delta)<1e-12)return(list(risky=rep(0,n),cash=1))
  if(max(abs(excess))<1e-12)stop('所有资产收益等于无风险利率，目标收益不可行。')
  A<-matrix(excess,ncol=1);b<-delta
  if(!allow_short){A<-cbind(A,diag(n));b<-c(b,rep(0,n))}
  if(!allow_borrow){A<-cbind(A,-rep(1,n));b<-c(b,-1)}
  w<-quadprog::solve.QP(2*sigma,rep(0,n),A,b,meq=1)$solution
  cash<-1-sum(w)
  if(abs(sum(w*mu)+cash*rf-target)>1e-7 || (!allow_short&&min(w)< -1e-7) || (!allow_borrow&&cash< -1e-7))stop('含无风险资产的组合未通过约束校验。')
  w[abs(w)<1e-10]<-0
  list(risky=w,cash=if(abs(cash)<1e-10)0 else cash)
}
complete_stats <- function(w,cash,mu,sigma,rf) {
  r<-sum(w*mu)+cash*rf;v<-sqrt(max(0,drop(t(w)%*%sigma%*%w)))
  c(return=r,volatility=v,sharpe=if(v>1e-12)(r-rf)/v else NA_real_)
}

cash_frontier <- function(s,v,points=90) {
  e<-s$est;rf<-s$rf;short<-isTRUE(s$allow_short);borrow<-isTRUE(s$allow_borrow)
  span<-max(.1,diff(range(c(e$mu,rf))))
  lo<-min(c(e$mu,rf,v$lower$return));hi<-max(c(e$mu,rf,v$frontier$return))
  if(!short) {
    lo<-if(borrow&&any(e$mu<rf))min(lo,rf-span) else min(c(e$mu,rf))
    hi<-if(borrow&&any(e$mu>rf))max(hi,rf+span) else max(c(e$mu,rf))
  }
  build<-function(end) {
    targets<-unique(rf+(end-rf)*seq(0,1,length.out=points)^2)
    ws<-lapply(targets,function(t)tryCatch(cash_target_weights(e$mu,e$sigma,rf,t,short,borrow),error=function(err)NULL))
    ok<-!vapply(ws,is.null,logical(1));ws<-ws[ok]
    stats<-as.data.frame(t(vapply(ws,function(w)complete_stats(w$risky,w$cash,e$mu,e$sigma,rf),numeric(3))))
    list(stats=stats,weights=ws)
  }
  list(upper=build(hi),lower=build(lo))
}

# A deterministic sample illustrates the opportunity set; it is not an exhaustive list.
feasible_samples <- function(s,count=350) {
  n<-length(s$est$mu)
  had_seed<-exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed<-get('.Random.seed',envir=.GlobalEnv)
  on.exit(if(had_seed)assign('.Random.seed',old_seed,envir=.GlobalEnv) else if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv))
  set.seed(401)
  w<-matrix(rexp(count*n),count,n);w<-w/rowSums(w)
  if(isTRUE(s$allow_short)) {
    z<-matrix(rnorm(count*n),count,n);z<-z-rowMeans(z)
    w<-w+z*rep(seq(.1,1.5,length.out=count),n)
  }
  w<-rbind(diag(n),w,s$opt$gmv)
  cash<-rep(0,nrow(w))
  if(isTRUE(s$has_rf)) {
    scale<-seq(0,if(isTRUE(s$allow_borrow))2 else 1,length.out=nrow(w))
    w2<-w*scale;w<-rbind(w,w2);cash<-c(cash,1-scale)
  }
  stats<-as.data.frame(t(vapply(seq_len(nrow(w)),function(i)complete_stats(w[i,],cash[i],s$est$mu,s$est$sigma,s$rf),numeric(3))))
  list(stats=stats,weights=w,cash=cash)
}

portfolio_at_id <- function(id,s,v) {
  if(!is.character(id)||length(id)!=1)return(NULL)
  parts<-strsplit(id,':',fixed=TRUE)[[1]];kind<-parts[1];i<-if(length(parts)>1)suppressWarnings(as.integer(parts[2])) else NA_integer_
  label<-switch(kind,gmv='风险资产 GMV',tangent='切点组合',rf='100% 无风险资产',risky='有效前沿选点',lower='下半支选点',sample='可行组合选点',cash_upper='含无风险资产前沿选点',cash_lower='含无风险资产下半支选点',stock='单项资产',NULL)
  if(is.null(label))return(NULL)
  if(kind=='gmv')return(list(risky=s$opt$gmv,cash=0,complete=FALSE,label=label))
  if(kind=='tangent'&&!is.null(s$opt$tangent))return(list(risky=s$opt$tangent,cash=0,complete=FALSE,label=label))
  if(kind=='rf'&&isTRUE(s$has_rf))return(list(risky=rep(0,length(s$est$mu)),cash=1,complete=TRUE,label=label))
  if(!is.finite(i)||i<1)return(NULL)
  if(kind%in%c('risky','lower')) {
    rows<-if(kind=='risky')v$frontier else v$lower
    if(i>nrow(rows))return(NULL)
    w<-risky_target_weights(s$est$mu,s$est$sigma,rows$return[i],s$allow_short)
    return(list(risky=w,cash=0,complete=FALSE,label=label))
  }
  if(kind=='stock'&&i<=length(s$est$mu))return(list(risky=as.numeric(seq_along(s$est$mu)==i),cash=0,complete=FALSE,label=label))
  if(kind=='sample'&&i<=nrow(v$samples$weights))return(list(risky=v$samples$weights[i,],cash=v$samples$cash[i],complete=TRUE,label=label))
  if(kind%in%c('cash_upper','cash_lower')&&isTRUE(s$has_rf)) {
    branch<-if(kind=='cash_upper')v$cash$upper else v$cash$lower
    if(i>length(branch$weights))return(NULL)
    return(c(branch$weights[[i]],list(complete=TRUE,label=label)))
  }
  NULL
}
