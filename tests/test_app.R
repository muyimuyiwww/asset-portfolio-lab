if(Sys.getenv('RUN_LIVE_TESTS')!='1'){cat('SKIP live integration test: set RUN_LIVE_TESTS=1 to fetch public market data\n');quit(status=0)}
.libPaths(c('.Rlib',.libPaths()));a<-source('app.R')$value
shiny::testServer(server,{
 session$setInputs(tickers='A,600519\nH,00700\nUS,AAPL',dates=as.Date(c('2021-10-01','2026-09-30')),rf=2,method='历史均值',half=12,refresh=FALSE)
 session$setInputs(calculate=1)
 stopifnot(is.null(error()),nrow(state()$data$x)==60,!is.null(state()$opt$tangent))
 g<-state()$opt$gmv;print(stats());print(weights())
 for(k in c('png','csv','data_csv')){f<-session$getOutput(k);stopifnot(file.exists(f),file.info(f)$size>100);file.copy(f,file.path('outputs',basename(f)),overwrite=TRUE)}
 stopifnot(grepl('有效前沿',output$frontier))
 session$setInputs(method='CAPM',calculate=2)
 stopifnot(is.null(error()),max(abs(g-state()$opt$gmv))<1e-10)
 session$setInputs(method='指数加权',half=6,calculate=3)
 stopifnot(is.null(error()),state()$half==6)
 session$setInputs(rf=99,calculate=4);stopifnot(is.null(error()),is.null(state()$opt$tangent))
 session$setInputs(tickers='US,AAPL\nUS,AAPL',calculate=5);stopifnot(is.null(state()),grepl('重复',error()))
 session$setInputs(tickers='A,600519\nH,00700\nUS,AAPL',dates=as.Date(c('2021-10-01','2026-09-30')),rf=2,method='历史均值',calculate=6);stopifnot(is.null(error()))
})
cat('PASS app inputs, render, methods, no-positive-premium, duplicate, insufficient sample\n')
