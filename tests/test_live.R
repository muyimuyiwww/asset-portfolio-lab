if(Sys.getenv('RUN_LIVE_TESTS')!='1'){cat('SKIP live integration test: set RUN_LIVE_TESTS=1 to fetch public market data\n');quit(status=0)}
.libPaths(c('.Rlib',.libPaths()));source('R/data.R');source('R/estimate.R');source('R/optimize.R')
d<-fetch_data(data.frame(market=c('A','H','US'),ticker=c('600519','00700','AAPL')),as.Date('2021-10-01'),as.Date('2026-09-30'))
stopifnot(nrow(d$x)>=36,all(is.finite(d$x)),all(d$dates<as.Date('2026-10-01')))
res<-lapply(c('历史均值','指数加权','CAPM'),function(m){e<-estimate_returns(d$x,d$assets$market,d$dates,d$b,m);o<-optimize_portfolio(e$mu,e$sigma);stopifnot(min(o$gmv)>=-1e-7,abs(sum(o$gmv)-1)<1e-7);list(e=e,o=o)})
stopifnot(max(abs(res[[1]]$o$gmv-res[[3]]$o$gmv))<1e-9)
write.csv(data.frame(date=d$dates,d$x,d$b), 'outputs/跨市场示例_人民币月收益.csv',row.names=FALSE)
write.csv(data.frame(股票=colnames(d$x),GMV=res[[1]]$o$gmv,最大Sharpe=res[[1]]$o$tangent),'outputs/跨市场示例_组合权重.csv',row.names=FALSE)
cat('PASS live cross-market',nrow(d$x),'months',as.character(min(d$dates)),as.character(max(d$dates)),'\n')
