if(Sys.getenv('RUN_LIVE_TESTS')!='1'){cat('SKIP live integration test: set RUN_LIVE_TESTS=1 to fetch public market data\n');quit(status=0)}
.libPaths(c('.Rlib',.libPaths()));source('app.R')
shiny::testServer(server,{
 session$setInputs(tickers='A,600519\nH,00700\nUS,AAPL',dates=as.Date(c('2025-10-01','2026-09-30')),rf=2,method='历史均值',half=12,refresh=FALSE,calculate=1)
 stopifnot(is.null(error()),nrow(state()$data$x)==12,all(is.finite(state()$data$x)))
 g<-state()$opt$gmv
 for(method in c('历史均值','指数加权','CAPM')){
  session$setInputs(method=method,calculate=input$calculate+1)
  stopifnot(is.null(error()),nrow(state()$data$x)==12,max(abs(state()$opt$gmv-g))<1e-10)
  w<-weights();stopifnot(ncol(w)==3,nrow(w)==3,'切点（最大 Sharpe）'%in%names(w))
  if(!is.null(state()$opt$tangent))stopifnot(identical(w[[3]],sprintf('%.2f%%',state()$opt$tangent*100)),abs(sum(state()$opt$tangent)-1)<1e-7)
 }
 stopifnot(grepl('短样本',paste(as.character(output$short_sample),collapse=' ')))
 session$setInputs(method='历史均值',rf=99,calculate=input$calculate+1)
 stopifnot(is.null(error()),is.null(state()$opt$tangent),all(weights()[[3]]=='不适用'),grepl('没有',paste(as.character(output$weight_note),collapse=' ')))
})
cat('PASS real one-year 12 monthly returns, three methods, unchanged GMV, tangent weights, short-sample warning, no-tangent reason\n')
