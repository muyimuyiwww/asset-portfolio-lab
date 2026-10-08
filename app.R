.libPaths(c(file.path(getwd(),'.Rlib'),.libPaths()))
library(shiny);library(plotly)
source('R/data.R');source('R/estimate.R');source('R/optimize.R');source('R/plot.R')
end_default<-as.Date(format(Sys.Date(),'%Y-%m-01'))-1
start_default<-seq(as.Date(format(end_default,'%Y-%m-01')),length=2,by='-59 months')[2]
ui<-fluidPage(title='资产组合实验室',tags$head(tags$link(rel='stylesheet',type='text/css',href='theme.css')),
  div(class='app-header',div(class='brand-mark','P'),h2('资产组合实验室')),
  sidebarLayout(sidebarPanel(width=3,
    h4('组合设置'),
    textAreaInput('tickers','股票代码',value='A,600519\nH,00700\nUS,AAPL',rows=5),
    tags$p(class='field-hint','每行 市场,代码 · A / H / US · 2–10只'),
    div(class='quick-actions',actionButton('example','跨市场示例'),actionButton('one_year','最近一年')),
    dateRangeInput('dates','历史区间',start=start_default,end=end_default,language='zh-CN',format='yyyy-mm-dd'),
    selectInput('frequency','收益频率',c('日度'='daily','月度'='monthly'),selected='monthly'),
    numericInput('rf','无风险年利率（%）',2,min=0,max=100,step=.1),
    selectInput('method','预期收益估计',c('历史均值','指数加权','CAPM')),
    conditionalPanel("input.method == '指数加权'",numericInput('half','半衰期（月）',12,min=1,max=120)),
    checkboxInput('shorting','允许卖空',FALSE),
    checkboxInput('refresh','刷新数据缓存',FALSE),
    actionButton('calculate','计算组合',class='btn-primary')),
    mainPanel(width=9,
      div(class='card chart-card',div(class='card-heading',h4('有效前沿'),uiOutput('plot_controls')),uiOutput('status'),uiOutput('short_sample'),uiOutput('plot_note'),plotlyOutput('frontier',height='460px'),div(class='export-actions',downloadButton('png','图表 PNG'),downloadButton('csv','组合 CSV'),downloadButton('data_csv','收益 CSV'))),
      div(class='card',h4('配置比例'),uiOutput('weight_note'),tableOutput('weights'),h4(class='metrics-heading','组合指标'),tags$span(class='field-hint','收益与波动均为年化值'),tableOutput('stats')),
      div(class='card source-card',tags$details(tags$summary('数据与来源'),uiOutput('sample'),tableOutput('sources'),uiOutput('source_note'))))))
server<-function(input,output,session) {
  state<-reactiveVal(NULL);error<-reactiveVal(NULL);busy<-reactiveVal(FALSE)
  observeEvent(input$example,{updateTextAreaInput(session,'tickers',value='A,600519\nH,00700\nUS,AAPL');updateDateRangeInput(session,'dates',start=start_default,end=end_default)})
  observeEvent(input$one_year,{updateDateRangeInput(session,'dates',start=seq(as.Date(format(end_default,'%Y-%m-01')),length=2,by='-11 months')[2],end=end_default)})
  observeEvent(input$calculate,{
    error(NULL);busy(TRUE);on.exit(busy(FALSE))
    tryCatch(withProgress(message='获取真实价格并优化',value=.05,{
      rows<-strsplit(trimws(input$tickers),'\n')[[1]];parts<-lapply(rows,function(r)trimws(strsplit(r,',',fixed=TRUE)[[1]]))
      if(length(rows)<2||length(rows)>10||any(lengths(parts)!=2))stop('请每行填写“市场,代码”，共2至10行。')
      assets<-data.frame(market=vapply(parts,`[`,character(1),1),ticker=vapply(parts,`[`,character(1),2))
      frequency<-if(is.null(input$frequency))'monthly' else input$frequency
      if(!frequency%in%c('daily','monthly'))stop('请选择日度或月度收益。')
      d<-fetch_data(assets,input$dates[1],input$dates[2],input$refresh,frequency);incProgress(.7)
      est<-estimate_returns(d$x,d$assets$market,d$dates,d$b,input$method,input$rf/100,input$half,frequency)
      opt<-optimize_portfolio(est$mu,est$sigma,input$rf/100,allow_short=isTRUE(input$shorting))
      state(list(data=d,est=est,opt=opt,rf=input$rf/100,method=input$method,half=input$half,allow_short=isTRUE(input$shorting),frequency=frequency));incProgress(.25)
    }),error=function(e){state(NULL);error(conditionMessage(e))})
  })
  output$status<-renderUI({if(!is.null(error()))return(tags$p(class='notice error-notice',error()));if(is.null(state()))return(tags$p(class='empty-hint','设置参数后，点击计算组合。'));s<-state();tags$p(class='result-meta',paste(switch(s$frequency,daily='日度',monthly='月度'),s$method,'CNY',sep=' · '))})
  output$short_sample<-renderUI({req(state());s<-state();if((s$frequency=='monthly'&&nrow(s$data$x)<36)||(s$frequency=='daily'&&nrow(s$data$x)<252))tags$p(class='notice','短样本：估计可能不稳定。')})
  needs_plot_switch<-reactive({req(state());s<-state();!is.null(s$opt$tangent)&&!frontier_view(s,'local')$tangent_visible})
  output$plot_controls<-renderUI({req(state());if(needs_plot_switch())radioButtons('plot_view',NULL,c('局部'='local','全景'='full'),selected='local',inline=TRUE)})
  observeEvent(state(),{if(needs_plot_switch())updateRadioButtons(session,'plot_view',selected='local')},ignoreNULL=TRUE)
  plot_view<-reactive({req(state());frontier_view(state(),if(needs_plot_switch()&&identical(input$plot_view,'full'))'full' else 'local')})
  output$plot_note<-renderUI({req(state());s<-state();v<-plot_view();if(!is.null(s$opt$tangent)&&!v$tangent_visible)tags$p(class='notice',sprintf('切点在局部范围之外：波动 %.2f%% · 收益 %.2f%%。切换全景查看。',s$opt$tangent_stats['volatility']*100,s$opt$tangent_stats['return']*100))})
  output$frontier<-renderPlotly({req(state());frontier_plotly(state(),plot_view())})
  stats<-reactive({req(state());s<-state();a<-rbind(GMV=s$opt$gmv_stats);if(!is.null(s$opt$tangent))a<-rbind(a,'最大 Sharpe'=s$opt$tangent_stats);data.frame(组合=rownames(a),年化预期收益=paste0(round(a[,1]*100,2),'%'),年化波动=paste0(round(a[,2]*100,2),'%'),Sharpe=round(a[,3],3),row.names=NULL,check.names=FALSE)})
  weights<-reactive({req(state());s<-state();data.frame(股票=names(s$est$mu),`GMV 权重`=sprintf('%.2f%%',s$opt$gmv*100),`切点权重（最大 Sharpe）`=if(is.null(s$opt$tangent))rep('不适用',length(s$est$mu)) else sprintf('%.2f%%',s$opt$tangent*100),check.names=FALSE)})
  output$weight_note<-renderUI({req(state());s<-state();tagList(if(s$allow_short)tags$p(class='field-hint','负权重表示卖空 · 净权重合计 100%'),if(is.null(s$opt$tangent))tags$p(class='notice',s$opt$tangent_reason))})
  output$stats<-renderTable(stats(),digits=3);output$weights<-renderTable(weights())
  output$sample<-renderUI({req(state());s<-state();d<-s$data;tags$p(class='field-hint',sprintf('%s — %s · %d %s · 剔除 %d 个缺失观测',format(min(d$dates),if(s$frequency=='daily')'%Y-%m-%d' else '%Y-%m'),format(max(d$dates),if(s$frequency=='daily')'%Y-%m-%d' else '%Y-%m'),nrow(d$x),if(s$frequency=='daily')'个共同交易日' else '个月',d$dropped_months))})
  output$source_note<-renderUI({req(state());d<-state()$data$metadata;notes<-character();if(!is.null(d$refresh_error)&&any(!is.na(d$refresh_error)))notes<-c(notes,'刷新失败，使用原缓存。');if(any(grepl('qfq;',d$source)))notes<-c(notes,'新浪复权处理与 Yahoo 有差异。');tags$p(class='field-hint',paste(notes,collapse=' '))})
  output$sources<-renderTable({req(state());d<-state()$data$metadata;data.frame(代码=d$ticker,来源=vapply(d$source,function(x){if(grepl('FRED',x))return('美联储 FRED 日汇率（纽约午间）');if(grepl('qfq;',x))return('新浪美股前复权（已核验因子）');if(grepl('hfq',x))return('新浪股票后复权');if(grepl('Sina',x))return('新浪价格指数');'Yahoo 调整价 / 收盘价'},character(1)),获取时间=substr(d$retrieved_at,1,16),模式=ifelse(d$cache_used,'真实缓存','联网获取'),check.names=FALSE)})
  output$csv<-downloadHandler(filename=function()'组合结果.csv',content=function(file){req(state());s<-state();d<-data.frame(type='frontier',asset=NA,weight=NA,s$opt$frontier,check.names=FALSE);for(k in c('gmv','tangent'))if(!is.null(s$opt[[k]]))d<-rbind(d,data.frame(type=k,asset=names(s$est$mu),weight=s$opt[[k]],return=unname(s$opt[[paste0(k,'_stats')]][1]),volatility=unname(s$opt[[paste0(k,'_stats')]][2]),sharpe=unname(s$opt[[paste0(k,'_stats')]][3])));d$allow_short<-s$allow_short;d$constraint<-if(s$allow_short)'sum(weights)=1; unrestricted short selling' else 'sum(weights)=1; weights>=0';d$method<-s$method;d$rf<-s$rf;d$half_life_months<-if(s$method=='指数加权')s$half else NA;d$currency<-'CNY';d$frequency<-s$frequency;d$annualization_factor<-switch(s$frequency,daily=252,monthly=12,annual=1);d$sample_observations<-nrow(s$data$x);d$sample_months<-if(s$frequency=='monthly')nrow(s$data$x) else NA;d$sample_start<-as.character(min(s$data$dates));d$sample_end<-as.character(max(s$data$dates));write.csv(d,file,row.names=FALSE,fileEncoding='UTF-8')})
  output$data_csv<-downloadHandler(filename=function()paste0('人民币',switch(state()$frequency,daily='日',monthly='月',annual='年'),'收益.csv'),content=function(file){req(state());d<-state()$data;write.csv(data.frame(date=d$dates,frequency=state()$frequency,d$x,d$b,check.names=FALSE),file,row.names=FALSE,fileEncoding='UTF-8')})
  output$png<-downloadHandler(filename=function()paste0('有效前沿_',if(identical(plot_view()$view,'full'))'全景' else '局部','.png'),content=function(file){req(state());frontier_png(file,state(),plot_view())})
}
shinyApp(ui,server)
