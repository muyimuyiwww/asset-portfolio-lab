.libPaths(c('.Rlib',.libPaths()));source('app.R')
dir.create('outputs',showWarnings=FALSE)
# Deterministic synthetic covariance; no local workbook or downloaded cache.
e<-list(mu=c(A=.08,B=.16),sigma=diag(c(.04,.09)))
gmv<-optimize_portfolio(e$mu,e$sigma,0,allow_short=TRUE)$gmv_stats['return']
far_rf<-unname(gmv-1e-6)
s<-list(est=e,opt=optimize_portfolio(e$mu,e$sigma,far_rf,allow_short=TRUE),rf=far_rf,method='历史均值',frequency='daily',allow_short=TRUE,data=list(dates=as.Date(c('2020-01-02','2022-05-13'))))
a<-frontier_view(s);b<-frontier_view(s,'full')
stopifnot(!a$tangent_visible,b$tangent_visible,a$xlim[2]<1,b$xlim[2]>90,nrow(a$frontier)>=300)
# Independently evaluate the variance formula at all displayed targets.
inv<-solve(e$sigma);A<-sum(inv);B<-sum(inv%*%e$mu);C<-drop(t(e$mu)%*%inv%*%e$mu)
for(v in list(a,b)){
 target<-v$frontier$return;variance<-(A*target^2-2*B*target+C)/(A*C-B^2)
 stopifnot(max(abs(variance-v$frontier$volatility^2)/pmax(1,variance))<1e-10)
}
frontier_png('outputs/有效前沿_局部.png',s,a);frontier_png('outputs/有效前沿_全景.png',s,b)
shiny::testServer(server,{
 state(s);session$setInputs(plot_view='local');stopifnot(grepl('局部范围之外',paste(as.character(output$plot_note),collapse=' ')),!plot_view()$tangent_visible)
 stopifnot(needs_plot_switch(),grepl('plot_view',paste(as.character(output$plot_controls),collapse=' ')))
 before<-state()$opt$tangent;session$setInputs(plot_view='full');stopifnot(plot_view()$tangent_visible,identical(before,state()$opt$tangent))
 built<-plotly::plotly_build(frontier_plotly(state(),plot_view()))
 stopifnot(max(abs(built$x$layout$xaxis$range-plot_view()$xlim*100))<1e-8)
 file<-session$getOutput('png');stopifnot(file.info(file)$size>1000)
 ordinary<-s;ordinary$rf<-.02;ordinary$opt<-optimize_portfolio(e$mu,e$sigma,.02)
 state(ordinary);session$flushReact();stopifnot(!needs_plot_switch(),is.null(output$plot_controls),plot_view()$view=='local')
 missing<-s;missing$rf<-.2;missing$opt<-optimize_portfolio(e$mu,e$sigma,.2,allow_short=TRUE)
 state(missing);session$flushReact();stopifnot(!needs_plot_switch(),is.null(output$plot_controls),plot_view()$view=='local')
 state(s);session$flushReact();stopifnot(needs_plot_switch(),!is.null(output$plot_controls))
})
s$opt<-optimize_portfolio(e$mu,e$sigma,.2,allow_short=TRUE);s$rf<-.2;stopifnot(!frontier_view(s)$tangent_visible)
s$opt<-optimize_portfolio(e$mu,e$sigma,.02);s$rf<-.02;stopifnot(frontier_view(s)$tangent_visible)
cat('PASS extreme tangent local/full bounds, analytic curve, sample density, exports, unchanged weights, no tangent and long-only\n')

# Unrestricted tangency is an interior plotted point, even when far away.
far<-list(est=e,opt=optimize_portfolio(e$mu,e$sigma,far_rf,allow_short=TRUE),rf=far_rf,frequency='daily',allow_short=TRUE,data=s$data)
far_view<-frontier_view(far,'full')
stopifnot(max(far_view$frontier$return)>=far$opt$gmv_stats['return']+1.59*(far$opt$tangent_stats['return']-far$opt$gmv_stats['return']))
near<-far;near$rf<-.02;near$est<-list(mu=c(A=.08,B=.16),sigma=diag(c(.04,.09)));near$opt<-optimize_portfolio(near$est$mu,near$est$sigma,.02,allow_short=TRUE)
near_view<-frontier_view(near)
stopifnot(near_view$tangent_visible,max(near_view$frontier$return)>near$opt$tangent_stats['return'],max(near_view$frontier$volatility)>near$opt$tangent_stats['volatility'])
bounded<-near;bounded$allow_short<-FALSE;bounded$opt<-optimize_portfolio(near$est$mu,near$est$sigma,.02)
bv<-frontier_view(bounded)
stopifnot(bv$bounded_endpoint,abs(max(bv$frontier$return)-max(near$est$mu))<1e-10)
cat('PASS continuation beyond near/far unrestricted tangents and long-only feasible endpoint\n')

# Lower branch has the same GMV vertex and satisfies Markowitz variance.
for(case in list(far,near,bounded)){
 view<-frontier_view(case,if(identical(case,far))'full' else 'local')
 stopifnot(nrow(view$lower)>1,abs(view$lower$return[1]-case$opt$gmv_stats['return'])<1e-10,abs(view$lower$volatility[1]-case$opt$gmv_stats['volatility'])<1e-10,all(diff(view$lower$return)<=0),all(view$lower$volatility>=case$opt$gmv_stats['volatility']-1e-10),view$ylim[1]<min(view$lower$return))
 if(case$allow_short){inv<-solve(case$est$sigma);A<-sum(inv);B<-sum(inv%*%case$est$mu);C<-drop(t(case$est$mu)%*%inv%*%case$est$mu);target<-view$lower$return;stopifnot(max(abs((A*target^2-2*B*target+C)/(A*C-B^2)-view$lower$volatility^2)/pmax(1,view$lower$volatility^2))<1e-10)}else stopifnot(abs(min(view$lower$return)-min(case$est$mu))<1e-10)
}
cat('PASS lower branch variance, GMV junction, feasible bounds and chart ranges\n')
