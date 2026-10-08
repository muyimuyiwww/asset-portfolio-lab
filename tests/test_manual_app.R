.libPaths(c('.Rlib',.libPaths()));source('app.R')
fetch_count<-0
fetch_data<-function(...) {fetch_count<<-fetch_count+1;stop('Unexpected network fetch')}
shiny::testServer(server,{
 session$setInputs(input_mode='manual',manual_assets='A,8,20\nB,14,30',manual_matrix='1,0.2\n0.2,1',matrix_kind='correlation',rf=2,has_rf=FALSE,shorting=FALSE,allow_borrow=FALSE,calculate=1)
 stopifnot(is.null(error()),fetch_count==0,identical(state()$input_mode,'manual'),is.null(state()$opt$tangent),is.null(plot_view()$cash),!('Sharpe'%in%names(stats())),ncol(weights())==2)
 stopifnot(is.null(output$source_card),!grepl('data_csv',paste(output$export_controls,collapse=' ')))
 stopifnot(abs(sum(selected_portfolio()$risky)-1)<1e-8)
 session$setInputs(has_rf=TRUE,calculate=2)
 stopifnot(is.null(error()),!is.null(plot_view()$cash),!is.null(state()$opt$tangent),'Sharpe'%in%names(stats()),any(grepl('整体 GMV',stats()$组合)))
 session$setInputs(portfolio_pick='tangent',risky_allocation=60)
 p<-selected_portfolio();stopifnot(abs(sum(p$risky)-.6)<1e-10,abs(p$cash-.4)<1e-10)
 before<-state()$est$sigma
 session$setInputs(allow_borrow=TRUE,calculate=3,portfolio_pick='gmv',risky_allocation=150)
 p<-selected_portfolio();stopifnot(is.null(error()),abs(p$cash+.5)<1e-10,identical(before,state()$est$sigma))
 session$setInputs(shorting=TRUE,calculate=31,risky_allocation=-50)
 stopifnot(length(output$query_controls)>0)
 p<-selected_portfolio();stopifnot(is.null(error()),abs(p$cash-1.5)<1e-10,abs(sum(p$risky)+.5)<1e-10)
 session$setInputs(shorting=FALSE,calculate=32,risky_allocation=150)
 # Exercise the actual Plotly click event input, not just a helper function.
 # Plotly's Shiny binding sends the point record array directly.
 session$setInputs('plotly_click-portfolio'=jsonlite::toJSON(list(list(curveNumber=0,pointNumber=4,x=20,y=8,customdata='risky:5')),auto_unbox=TRUE))
 stopifnot(identical(selection()$label,'有效前沿选点'))
 selection(portfolio_at_id('cash_upper:5',state(),plot_view()));p<-selected_portfolio();stopifnot(isTRUE(p$complete),abs(sum(p$risky)+p$cash-1)<1e-10)
 for(k in c('png','csv','model_csv','selected_csv')){file<-session$getOutput(k);stopifnot(file.exists(file),file.info(file)$size>100)}
 file<-session$getOutput('csv');out<-read.csv(file);stopifnot(all(out$frequency=='manual'),any(grepl('frontier_',out$type)),any(grepl('cash_upper_',out$type)),any(out$asset=='无风险资产'),all(out$has_risk_free),all(out$allow_borrow))
 session$setInputs(matrix_kind='covariance',manual_assets='A,8\nB,14',manual_matrix='0.04,0.012\n0.012,0.09',calculate=4)
 stopifnot(is.null(error()),max(abs(state()$est$sigma-before))<1e-12,fetch_count==0)
 session$setInputs(manual_matrix='0.04,0.02\n0.012,0.09',calculate=5)
 stopifnot(is.null(state()),grepl('对称',error()),is.null(selection()),fetch_count==0)
 session$setInputs(input_mode='history',tickers='US,AAPL\nUS,MSFT',dates=as.Date(c('2020-01-01','2022-01-01')),frequency='monthly',method='历史均值',half=12,calculate=6)
 stopifnot(fetch_count==1,grepl('Unexpected network fetch',error()))
})
cat('PASS manual mode makes no data requests, rf on/off, cash allocation, Plotly clicks, matrix view, exports and validation\n')
