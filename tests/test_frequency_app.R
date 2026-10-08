if(Sys.getenv('RUN_LIVE_TESTS')!='1'){cat('SKIP live integration test: set RUN_LIVE_TESTS=1 to fetch public market data\n');quit(status=0)}
.libPaths(c('.Rlib',.libPaths()));source('app.R')
shiny::testServer(server,{
 session$setInputs(tickers='A,600519\nH,00700\nUS,AAPL',dates=as.Date(c('2021-10-01','2026-09-30')),rf=2,method='历史均值',half=12,refresh=FALSE,shorting=TRUE)
 for(i in 1:2){
  f<-c('daily','monthly')[i];session$setInputs(frequency=f,calculate=i)
  stopifnot(is.null(error()),state()$frequency==f,isTRUE(state()$allow_short))
  stopifnot(grepl(switch(f,daily='共同交易日',monthly='个月',annual='完整自然年'),paste(as.character(output$sample),collapse=' ')))
  file<-session$getOutput('csv');d<-read.csv(file);stopifnot(all(d$frequency==f),all(d$allow_short),!('short_lower_bound'%in%names(d)),all(d$annualization_factor==switch(f,daily=252,monthly=12,annual=1)))
 }
 session$setInputs(frequency='monthly',shorting=FALSE,calculate=10)
 stopifnot(is.null(error()),!state()$allow_short,all(state()$opt$gmv>=0),all(state()$opt$tangent>=0))
 session$setInputs(frequency='annual',dates=as.Date(c('2025-10-01','2026-09-30')),calculate=4)
 stopifnot(is.null(state()),grepl('日度或月度',error()))
})
cat('PASS frequency UI state, short constraints, exports and unsupported annual frequency\n')
