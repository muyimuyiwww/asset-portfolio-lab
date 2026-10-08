fetch_data <- function(assets,start,end,refresh=FALSE,frequency='monthly') {
  dir.create('work',showWarnings=FALSE,recursive=TRUE)
  request <- tempfile(tmpdir='work',fileext='.json'); result <- tempfile(tmpdir='work',fileext='.json')
  on.exit(unlink(c(request,result)))
  jsonlite::write_json(list(assets=assets,start=as.character(start),end=as.character(end),refresh=refresh,frequency=frequency),request,auto_unbox=TRUE,dataframe='rows')
  status <- system2(file.path(getwd(),'.venv/bin/python'),c(shQuote('python/data.py'),shQuote(request),shQuote(result)),stdout=FALSE,stderr='work/last-fetch.log')
  if(!file.exists(result))stop('数据程序未能启动。请查看使用说明中的环境修复方法。')
  d<-jsonlite::fromJSON(result)
  if(!is.null(d$error))stop(d$error)
  d$x<-as.matrix(d$values);colnames(d$x)<-paste(d$assets$market,d$assets$ticker,sep=':')
  d$dates<-as.Date(d$dates);d$b<-as.matrix(as.data.frame(d$benchmarks));d
}
