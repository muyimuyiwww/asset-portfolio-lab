frontier_view <- function(s,view='local',points=300) {
  o<-s$opt;e<-s$est;full<-identical(view,'full');gm<-unname(o$gmv_stats['return'])
  stock_vol<-sqrt(diag(e$sigma))
  local_top<-max(gm+.1,e$mu)+.08*max(.1,diff(range(c(gm,e$mu))))
  f<-o$frontier
  if(isTRUE(o$unbounded_frontier)) {
    tr<-if(is.null(o$tangent))gm else unname(o$tangent_stats['return'])
    # Leave a substantial segment beyond a visible tangent; distant tangents
    # still remain outside the local window.
    extended_top<-gm+1.6*max(0,tr-gm)
    top<-if(full)max(local_top,max(f$return),extended_top) else if(tr<=local_top)max(local_top,extended_top) else local_top
    targets<-sort(unique(c(gm+(local_top-gm)*seq(0,1,length.out=points)^2,gm+(top-gm)*seq(0,1,length.out=points)^2)))
    v1<-solve(e$sigma,rep(1,length(e$mu)));direction<-solve(e$sigma,e$mu)-gm*v1;spread<-sum(e$mu*direction)
    weights<-lapply(targets,function(t)o$gmv+(t-gm)*direction/spread)
    f<-as.data.frame(t(vapply(weights,portfolio_stats,numeric(3),mu=e$mu,sigma=e$sigma,rf=s$rf)))
  }
  lower<-f[FALSE,,drop=FALSE]
  if(isTRUE(o$unbounded_frontier)) {
    targets_lower<-gm-(max(f$return)-gm)*seq(0,1,length.out=points)^2
    lower_weights<-lapply(targets_lower,function(t)o$gmv+(t-gm)*direction/spread)
    lower<-as.data.frame(t(vapply(lower_weights,portfolio_stats,numeric(3),mu=e$mu,sigma=e$sigma,rf=s$rf)))
  } else if(diff(range(e$mu))>1e-12&&gm-min(e$mu)>1e-10) {
    n<-length(e$mu);lowest<-which(abs(e$mu-min(e$mu))<1e-12)
    endpoint<-rep(0,n)
    if(length(lowest)==1)endpoint[lowest]<-1 else endpoint[lowest]<-quadprog::solve.QP(2*e$sigma[lowest,lowest,drop=FALSE],rep(0,length(lowest)),cbind(rep(1,length(lowest)),diag(length(lowest))),c(1,rep(0,length(lowest))),meq=1)$solution
    targets_lower<-gm-(gm-min(e$mu))*seq(0,1,length.out=100)^2
    lower_weights<-lapply(targets_lower,function(t){if(abs(t-min(e$mu))<1e-10)return(endpoint);quadprog::solve.QP(2*e$sigma,rep(0,n),cbind(rep(1,n),e$mu,diag(n)),c(1,t,rep(0,n)),meq=2)$solution})
    lower<-as.data.frame(t(vapply(lower_weights,portfolio_stats,numeric(3),mu=e$mu,sigma=e$sigma,rf=s$rf)))
  }
  xr<-range(c(0,stock_vol,f$volatility,lower$volatility));yr<-range(c(s$rf,e$mu,f$return,lower$return))
  if(full&&!is.null(o$tangent)){xr<-range(c(xr,o$tangent_stats['volatility']));yr<-range(c(yr,o$tangent_stats['return']))}
  # Explicit ranges keep an outlying tangent from stretching the local view.
  xmax<-xr[2]+.08*max(.01,diff(xr));ypad<-.10*max(.05,diff(yr));ylim<-c(yr[1]-ypad,yr[2]+ypad)
  visible<-!is.null(o$tangent)&&o$tangent_stats['volatility']<=xmax&&o$tangent_stats['return']>=ylim[1]&&o$tangent_stats['return']<=ylim[2]
  list(lower=lower,bounded_endpoint=!isTRUE(s$allow_short)&&nrow(f)>1,frontier=f,xlim=c(0,xmax),ylim=ylim,tangent_visible=visible,view=if(full)'full' else 'local')
}
frontier_plotly <- function(s,v) {
  o<-s$opt;e<-s$est
  p<-plotly::plot_ly(v$frontier,x=~volatility*100,y=~return*100,type='scatter',mode='lines',name='有效前沿',line=list(color='#af3548',width=3),hovertemplate='有效前沿<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>')
  if(nrow(v$lower)>0)p<-plotly::add_trace(p,x=v$lower$volatility*100,y=v$lower$return*100,type='scatter',mode='lines',name='下半支（非有效）',line=list(color='#d9a1ae',width=2),hovertemplate='下半支（非有效）<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>',inherit=FALSE)
  p<-plotly::add_trace(p,x=sqrt(diag(e$sigma))*100,y=e$mu*100,text=names(e$mu),type='scatter',mode='markers',name='股票（悬停查看）',marker=list(size=9,color='#8d7b80'),hovertemplate='%{text}<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>',inherit=FALSE)
  p<-plotly::add_trace(p,x=o$gmv_stats['volatility']*100,y=o$gmv_stats['return']*100,type='scatter',mode='markers+text',text='GMV',textposition='bottom right',name='GMV',marker=list(size=13,color='#d69737'),hovertemplate='GMV<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>',inherit=FALSE)
  if(v$bounded_endpoint) {
    last<-tail(v$frontier,1)
    p<-plotly::add_trace(p,x=last$volatility*100,y=last$return*100,type='scatter',mode='markers+text',text='可行端点',textposition='top right',name='可行端点',showlegend=FALSE,marker=list(size=8,symbol='diamond',color='#af3548'),hovertemplate='不卖空的最高收益端点<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>',inherit=FALSE)
  }
  if(!is.null(o$tangent)) {
    if(v$tangent_visible)p<-plotly::add_trace(p,x=o$tangent_stats['volatility']*100,y=o$tangent_stats['return']*100,type='scatter',mode='markers',name='切点（最大 Sharpe）',marker=list(size=13,color='#751b30'),hovertemplate='切点<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>',inherit=FALSE)
    xx<-v$xlim
    p<-plotly::add_trace(p,x=xx*100,y=(s$rf+o$tangent_stats['sharpe']*xx)*100,type='scatter',mode='lines',name='资本配置线（理论）',line=list(dash='dash',color='#b69ba3'),hovertemplate='资本配置线<br>年化波动：%{x:.2f}%<br>年化预期收益：%{y:.2f}%<extra></extra>',inherit=FALSE)
  }
  plotly::config(plotly::layout(p,xaxis=list(title='年化波动率（%）',range=v$xlim*100,tickformat=',.1f',gridcolor='#f2e9eb',zerolinecolor='#d9c9ce'),yaxis=list(title='年化预期收益（%）',range=v$ylim*100,tickformat=',.1f',gridcolor='#f2e9eb',zerolinecolor='#d9c9ce'),legend=list(orientation='h',y=-.22),margin=list(b=110,l=80,r=25),plot_bgcolor='white',paper_bgcolor='white',uirevision=paste(s$method,s$frequency,s$rf,s$allow_short,min(s$data$dates),max(s$data$dates),paste(names(e$mu),collapse=','),v$view)),displaylogo=FALSE)
}
frontier_png <- function(file,s,v) {
  o<-s$opt;e<-s$est
  png(file,width=1500,height=1000,res=150,type=if(capabilities('aqua'))'quartz' else if(capabilities('cairo'))'cairo' else getOption('bitmapType'),family='sans');on.exit(dev.off())
  par(mar=c(5,5,5,2))
  plot(v$frontier$volatility*100,v$frontier$return*100,type='l',col='#af3548',lwd=3,xlab='Annual volatility (%)',ylab='Expected annual return (%)',main=paste('CNY efficient frontier /',v$view,'/',s$frequency),xlim=v$xlim*100,ylim=v$ylim*100)
  grid(col='#f2e9eb');if(nrow(v$lower)>0)lines(v$lower$volatility*100,v$lower$return*100,col='#d9a1ae',lwd=2);lines(v$frontier$volatility*100,v$frontier$return*100,col='#af3548',lwd=3)
  points(sqrt(diag(e$sigma))*100,e$mu*100,pch=19,col='#8d7b80');text(sqrt(diag(e$sigma))*100,e$mu*100,names(e$mu),pos=3,cex=.7)
  points(o$gmv_stats[2]*100,o$gmv_stats[1]*100,pch=19,col='#d69737',cex=1.5)
  if(v$bounded_endpoint){last<-tail(v$frontier,1);points(last$volatility*100,last$return*100,pch=18,col='#af3548');text(last$volatility*100,last$return*100,'Feasible endpoint',pos=4,cex=.7)}
  if(!is.null(o$tangent)) {
    abline(a=s$rf*100,b=o$tangent_stats[3],lty=2,col='#b69ba3')
    if(v$tangent_visible)points(o$tangent_stats[2]*100,o$tangent_stats[1]*100,pch=19,col='#751b30',cex=1.5)
    else mtext(sprintf('Tangency outside local view: volatility %.2f%%, return %.2f%%',o$tangent_stats[2]*100,o$tangent_stats[1]*100),side=3,line=.3,cex=.7)
  }
  labs<-c('Efficient frontier','Stocks','GMV');cols<-c('#af3548','#8d7b80','#d69737');ltys<-c(1,NA,NA);pchs<-c(NA,19,19)
  if(nrow(v$lower)>0){labs<-c(labs,'Lower branch (inefficient)');cols<-c(cols,'#d9a1ae');ltys<-c(ltys,1);pchs<-c(pchs,NA)}
  if(!is.null(o$tangent)){labs<-c(labs,'Capital allocation line');cols<-c(cols,'#b69ba3');ltys<-c(ltys,2);pchs<-c(pchs,NA)}
  if(v$tangent_visible){labs<-c(labs,'Max Sharpe');cols<-c(cols,'#751b30');ltys<-c(ltys,NA);pchs<-c(pchs,19)}
  legend('topleft',labs,col=cols,lty=ltys,pch=pchs,bty='n',cex=.8)
}
