.libPaths(c(file.path(getwd(),'.Rlib'),.libPaths()))
library(shiny);library(plotly)
source('R/data.R');source('R/estimate.R');source('R/optimize.R');source('R/model.R');source('R/plot.R')
end_default<-as.Date(format(Sys.Date(),'%Y-%m-01'))-1
start_default<-seq(as.Date(format(end_default,'%Y-%m-01')),length=2,by='-59 months')[2]
ui<-fluidPage(title='资产组合实验室',tags$head(tags$link(rel='stylesheet',type='text/css',href='theme.css')),
  div(class='app-header',div(class='brand-mark','M'),h2('资产组合实验室')),
  sidebarLayout(sidebarPanel(width=3,
    h4('组合设置'),
    radioButtons('input_mode','参数来源',c('历史数据'='history','手动输入'='manual'),inline=TRUE),
    conditionalPanel("input.input_mode == 'history'",
      textAreaInput('tickers','股票代码',value='A,600519\nH,00700\nUS,AAPL',rows=5),
      tags$p(class='field-hint','每行 市场,代码 · A / H / US · 2–10只'),
      div(class='quick-actions',actionButton('example','跨市场示例'),actionButton('one_year','最近一年')),
      dateRangeInput('dates','历史区间',start=start_default,end=end_default,language='zh-CN',format='yyyy-mm-dd'),
      selectInput('frequency','收益频率',c('日度'='daily','月度'='monthly'),selected='monthly'),
      selectInput('method','预期收益估计',c('历史均值','指数加权','CAPM')),
      conditionalPanel("input.method == '指数加权'",numericInput('half','半衰期（月）',12,min=1,max=120)),
      checkboxInput('refresh','刷新数据缓存',FALSE)),
    conditionalPanel("input.input_mode == 'manual'",
      selectInput('matrix_kind','风险参数',c('标准差 + 相关系数'='correlation','协方差矩阵'='covariance')),
      uiOutput('manual_labels'),
      textAreaInput('manual_assets',NULL,value='资产A,8,20\n资产B,14,30\n资产C,11,25',rows=4),
      tags$p(class='field-hint','2–10项风险资产 · 全部参数为年化值'),
      uiOutput('matrix_label'),
      textAreaInput('manual_matrix',NULL,value='1,0.2,0.3\n0.2,1,0.4\n0.3,0.4,1',rows=4)),
    checkboxInput('shorting','允许卖空',FALSE),
    checkboxInput('has_rf','包含无风险资产',TRUE),
    conditionalPanel("input.has_rf || (input.input_mode == 'history' && input.method == 'CAPM')",
      numericInput('rf','无风险年利率（%）',2,min=0,max=100,step=.1)),
    conditionalPanel('input.has_rf',checkboxInput('allow_borrow','允许按无风险利率借款',FALSE)),
    actionButton('calculate','计算组合',class='btn-primary')),
    mainPanel(width=9,
      div(class='card chart-card',div(class='card-heading',h4('投资机会集与前沿'),uiOutput('plot_controls')),uiOutput('status'),uiOutput('short_sample'),uiOutput('plot_note'),plotlyOutput('frontier',height='500px'),uiOutput('export_controls')),
      div(class='card',h4('配置比例'),uiOutput('weight_note'),tableOutput('weights'),h4(class='metrics-heading','组合指标'),tags$span(class='field-hint','收益与波动均为年化值'),tableOutput('stats')),
      div(class='card',h4('组合查询'),uiOutput('query_controls'),uiOutput('query_note'),tableOutput('query_weights'),tableOutput('query_stats'),downloadButton('selected_csv','当前组合 CSV')),
      div(class='card',tags$details(tags$summary('资产参数与矩阵'),tableOutput('asset_stats'),radioButtons('matrix_view',NULL,c('相关系数'='correlation','协方差'='covariance'),inline=TRUE),div(class='matrix-scroll',tableOutput('matrix_table')),downloadButton('model_csv','模型参数 CSV'))),
      uiOutput('source_card'))))
server<-function(input,output,session) {
  state<-reactiveVal(NULL);error<-reactiveVal(NULL);busy<-reactiveVal(FALSE);selection<-reactiveVal(NULL)
  output$manual_labels<-renderUI(tags$label(class='control-label',`for`='manual_assets',if(identical(input$matrix_kind,'covariance'))'每行：资产名称,预期收益(%)' else '每行：资产名称,预期收益(%),标准差(%)'))
  output$matrix_label<-renderUI(tagList(tags$label(class='control-label',`for`='manual_matrix',if(identical(input$matrix_kind,'covariance'))'年化协方差矩阵' else '相关系数矩阵'),tags$p(class='field-hint',if(identical(input$matrix_kind,'covariance'))'按资产顺序输入；使用小数收益单位，例如20%标准差对应方差0.04。' else '按资产顺序输入；每行用逗号分隔，对角线为1。')))
  observeEvent(input$matrix_kind,{
    # Change only untouched examples; preserve a user's custom matrix.
    if(identical(input$manual_matrix,'1,0.2,0.3\n0.2,1,0.4\n0.3,0.4,1')&&input$matrix_kind=='covariance')updateTextAreaInput(session,'manual_matrix',value='0.04,0.012,0.015\n0.012,0.09,0.03\n0.015,0.03,0.0625')
    if(identical(input$manual_matrix,'0.04,0.012,0.015\n0.012,0.09,0.03\n0.015,0.03,0.0625')&&input$matrix_kind=='correlation')updateTextAreaInput(session,'manual_matrix',value='1,0.2,0.3\n0.2,1,0.4\n0.3,0.4,1')
  },ignoreInit=TRUE)
  observeEvent(input$example,{updateTextAreaInput(session,'tickers',value='A,600519\nH,00700\nUS,AAPL');updateDateRangeInput(session,'dates',start=start_default,end=end_default)})
  observeEvent(input$one_year,{updateDateRangeInput(session,'dates',start=seq(as.Date(format(end_default,'%Y-%m-01')),length=2,by='-11 months')[2],end=end_default)})
  observeEvent(input$calculate,{
    error(NULL);busy(TRUE);on.exit(busy(FALSE))
    tryCatch(withProgress(message='计算组合',value=.05,{
      mode<-if(identical(input$input_mode,'manual'))'manual' else 'history'
      has_rf<-if(is.null(input$has_rf))TRUE else isTRUE(input$has_rf)
      rf_input<-if(is.null(input$rf)).02 else input$rf/100
      if(!is.finite(rf_input)||rf_input<0||rf_input>1)stop('无风险年利率需介于0%与100%。')
      if(mode=='manual') {
        est<-manual_model(input$manual_assets,input$manual_matrix,input$matrix_kind)
        d<-list(dates=as.Date(character()));frequency<-'manual';method<-'外部输入';half<-NA_real_
      } else {
        rows<-strsplit(trimws(input$tickers),'\n')[[1]];parts<-lapply(rows,function(r)trimws(strsplit(r,',',fixed=TRUE)[[1]]))
        if(length(rows)<2||length(rows)>10||any(lengths(parts)!=2))stop('请每行填写“市场,代码”，共2至10行。')
        assets<-data.frame(market=vapply(parts,`[`,character(1),1),ticker=vapply(parts,`[`,character(1),2))
        frequency<-if(is.null(input$frequency))'monthly' else input$frequency
        if(!frequency%in%c('daily','monthly'))stop('请选择日度或月度收益。')
        d<-fetch_data(assets,input$dates[1],input$dates[2],input$refresh,frequency)
        method<-input$method;half<-if(is.null(input$half))12 else input$half
        est<-estimate_returns(d$x,d$assets$market,d$dates,d$b,method,rf_input,half,frequency)
      }
      incProgress(.7)
      rf<-if(has_rf)rf_input else 0
      opt<-optimize_portfolio(est$mu,est$sigma,rf,allow_short=isTRUE(input$shorting),include_rf=has_rf)
      if(!has_rf){opt$tangent<-NULL;opt$tangent_stats<-NULL;opt$tangent_reason<-NULL}
      s<-list(data=d,est=est,opt=opt,rf=rf,method=method,half=half,allow_short=isTRUE(input$shorting),frequency=frequency,input_mode=mode,has_rf=has_rf,allow_borrow=has_rf&&isTRUE(input$allow_borrow))
      s$samples<-feasible_samples(s)
      state(s);selection(portfolio_at_id('gmv',s,NULL));updateSelectInput(session,'portfolio_pick',selected='gmv')
      incProgress(.25)
    }),error=function(e){state(NULL);selection(NULL);error(conditionMessage(e))})
  })
  output$status<-renderUI({if(!is.null(error()))return(tags$p(class='notice error-notice',error()));if(is.null(state()))return(tags$p(class='empty-hint','设置参数后，点击计算组合。'));s<-state();tags$p(class='result-meta',paste(if(identical(s$input_mode,'manual'))'手动年化参数' else paste(switch(s$frequency,daily='日度',monthly='月度'),s$method,'CNY',sep=' · '),if(isTRUE(s$has_rf))'含无风险资产' else '仅风险资产',sep=' · '))})
  output$short_sample<-renderUI({req(state());s<-state();if(identical(s$input_mode,'manual'))return(NULL);if((s$frequency=='monthly'&&nrow(s$data$x)<36)||(s$frequency=='daily'&&nrow(s$data$x)<252))tags$p(class='notice','短样本：估计可能不稳定。')})
  needs_plot_switch<-reactive({req(state());s<-state();!is.null(s$opt$tangent)&&!frontier_view(s,'local')$tangent_visible})
  output$plot_controls<-renderUI({req(state());if(needs_plot_switch())radioButtons('plot_view',NULL,c('局部'='local','全景'='full'),selected='local',inline=TRUE)})
  observeEvent(state(),{if(needs_plot_switch())updateRadioButtons(session,'plot_view',selected='local')},ignoreNULL=TRUE)
  plot_view<-reactive({req(state());frontier_view(state(),if(needs_plot_switch()&&identical(input$plot_view,'full'))'full' else 'local')})
  output$plot_note<-renderUI({req(state());s<-state();v<-plot_view();if(!is.null(s$opt$tangent)&&!v$tangent_visible)tags$p(class='notice',sprintf('切点在局部范围之外：波动 %.2f%% · 收益 %.2f%%。切换全景查看。',s$opt$tangent_stats['volatility']*100,s$opt$tangent_stats['return']*100))})
  output$frontier<-renderPlotly({req(state());s<-state();s$plot_width<-session$clientData$output_frontier_width;frontier_plotly(s,plot_view())})
  format_stats<-function(a,has_rf) {
    d<-data.frame(组合=rownames(a),年化预期收益=paste0(round(a[,1]*100,2),'%'),年化波动=paste0(round(a[,2]*100,2),'%'),row.names=NULL,check.names=FALSE)
    if(has_rf)d$Sharpe<-ifelse(is.na(a[,3]),'—',format(round(a[,3],3),nsmall=3))
    d
  }
  stats<-reactive({req(state());s<-state();a<-rbind('风险资产 GMV'=s$opt$gmv_stats);if(!is.null(s$opt$tangent))a<-rbind(a,'切点（最大 Sharpe）'=s$opt$tangent_stats);if(isTRUE(s$has_rf))a<-rbind(a,'整体 GMV（100% 无风险）'=c(s$rf,0,NA));format_stats(a,isTRUE(s$has_rf))})
  weights<-reactive({req(state());s<-state();d<-data.frame(资产=names(s$est$mu),`风险资产 GMV`=sprintf('%.2f%%',s$opt$gmv*100),check.names=FALSE);if(isTRUE(s$has_rf))d[['切点（最大 Sharpe）']]<-if(is.null(s$opt$tangent))rep('不适用',length(s$est$mu)) else sprintf('%.2f%%',s$opt$tangent*100);d})
  output$weight_note<-renderUI({req(state());s<-state();tagList(if(s$allow_short)tags$p(class='field-hint','负权重表示卖空 · 风险资产组合净权重合计 100%'),if(isTRUE(s$has_rf)&&is.null(s$opt$tangent))tags$p(class='notice',s$opt$tangent_reason))})
  output$stats<-renderTable(stats());output$weights<-renderTable(weights())
  output$asset_stats<-renderTable({req(state());e<-state()$est;data.frame(资产=names(e$mu),`预期收益（%）`=round(e$mu*100,3),`标准差（%）`=round(sqrt(diag(e$sigma))*100,3),方差=format(signif(diag(e$sigma),6),scientific=FALSE,trim=TRUE),check.names=FALSE,row.names=NULL)})
  output$matrix_table<-renderTable({req(state());e<-state()$est;m<-if(identical(input$matrix_view,'covariance'))e$sigma else cov2cor(e$sigma);data.frame(资产=names(e$mu),round(m,6),check.names=FALSE)},digits=6)
  output$query_controls<-renderUI({req(state());s<-state();choices<-c('风险资产 GMV'='gmv');if(!is.null(s$opt$tangent))choices<-c(choices,'切点组合'='tangent');choices<-c(choices,'图中选点'='chart');tagList(tags$p(class='field-hint','点击边界、资产或可行组合点查看配置。散点为可行集的代表性样本。'),selectInput('portfolio_pick','查看组合',choices,selected=if(length(input$portfolio_pick)!=1L||!input$portfolio_pick%in%choices)'gmv' else input$portfolio_pick),if(isTRUE(s$has_rf)&&!isTRUE(selection()$complete))numericInput('risky_allocation','风险资产投入（%）',min=if(s$allow_short)NA_real_ else 0,max=if(isTRUE(s$allow_borrow))NA_real_ else 100,value=isolate(if(is.null(input$risky_allocation))100 else min(if(isTRUE(s$allow_borrow))Inf else 100,max(if(s$allow_short)-Inf else 0,input$risky_allocation))),step=5))})
  observeEvent(input$portfolio_pick,{req(state());if(input$portfolio_pick%in%c('gmv','tangent'))selection(portfolio_at_id(input$portfolio_pick,state(),plot_view()))})
  observeEvent(input[['plotly_click-portfolio']],{
    req(state());event<-plotly::event_data('plotly_click',source='portfolio');id<-event$customdata
    if(is.list(id))id<-unlist(id)
    if(length(id)==1){picked<-portfolio_at_id(as.character(id),state(),plot_view());if(!is.null(picked)){selection(picked);updateSelectInput(session,'portfolio_pick',selected='chart')}}
  })
  selected_portfolio<-reactive({req(state(),selection());s<-state();p<-selection();if(isTRUE(s$has_rf)&&!isTRUE(p$complete)){a<-if(is.null(input$risky_allocation))1 else input$risky_allocation/100;if(!is.finite(a)||(!s$allow_short&&a<0)||(!isTRUE(s$allow_borrow)&&a>1))stop('风险资产投入不符合借款限制。');p$risky<-p$risky*a;p$cash<-1-a;if(abs(a)<1e-12)p$label<-'100% 无风险资产' else if(abs(a-1)>1e-12)p$label<-paste0(p$label,' + 无风险资产')};p$stats<-complete_stats(p$risky,p$cash,s$est$mu,s$est$sigma,s$rf);p})
  output$query_note<-renderUI({req(selected_portfolio());p<-selected_portfolio();tagList(tags$p(class='result-meta',p$label),if(isTRUE(state()$has_rf)&&isTRUE(p$complete))tags$p(class='field-hint','此点已包含现金比例，直接显示图中配置。'),if(isTRUE(state()$has_rf)&&p$cash<0)tags$p(class='field-hint','无风险资产为负权重，表示借款。'))})
  selected_weights<-reactive({req(selected_portfolio());p<-selected_portfolio();labels<-names(state()$est$mu);w<-p$risky;if(isTRUE(state()$has_rf)){labels<-c(labels,'无风险资产');w<-c(w,p$cash)};data.frame(资产=c(labels,'合计'),`配置比例（%）`=sprintf('%.2f',c(w,sum(w))*100),check.names=FALSE)})
  output$query_weights<-renderTable(selected_weights());output$query_stats<-renderTable({p<-selected_portfolio();a<-matrix(p$stats,nrow=1,dimnames=list(p$label,names(p$stats)));format_stats(a,isTRUE(state()$has_rf))})
  output$export_controls<-renderUI({req(state());div(class='export-actions',downloadButton('png','图表 PNG'),downloadButton('csv','组合 CSV'),if(!identical(state()$input_mode,'manual'))downloadButton('data_csv','收益 CSV'))})
  output$source_card<-renderUI({req(state());if(identical(state()$input_mode,'manual'))return(NULL);div(class='card source-card',tags$details(tags$summary('数据与来源'),uiOutput('sample'),tableOutput('sources'),uiOutput('source_note')))})
  output$sample<-renderUI({req(state());s<-state();if(identical(s$input_mode,'manual'))return(NULL);d<-s$data;tags$p(class='field-hint',sprintf('%s — %s · %d %s · 剔除 %d 个缺失观测',format(min(d$dates),if(s$frequency=='daily')'%Y-%m-%d' else '%Y-%m'),format(max(d$dates),if(s$frequency=='daily')'%Y-%m-%d' else '%Y-%m'),nrow(d$x),if(s$frequency=='daily')'个共同交易日' else '个月',d$dropped_months))})
  output$source_note<-renderUI({req(state());if(identical(state()$input_mode,'manual'))return(NULL);d<-state()$data$metadata;notes<-character();if(!is.null(d$refresh_error)&&any(!is.na(d$refresh_error)))notes<-c(notes,'刷新失败，使用原缓存。');if(any(grepl('qfq;',d$source)))notes<-c(notes,'新浪复权处理与 Yahoo 有差异。');tags$p(class='field-hint',paste(notes,collapse=' '))})
  output$sources<-renderTable({req(state());if(identical(state()$input_mode,'manual'))return(NULL);d<-state()$data$metadata;data.frame(代码=d$ticker,来源=vapply(d$source,function(x){if(grepl('FRED',x))return('美联储 FRED 日汇率（纽约午间）');if(grepl('qfq;',x))return('新浪美股前复权（已核验因子）');if(grepl('hfq',x))return('新浪股票后复权');if(grepl('Sina',x))return('新浪价格指数');'Yahoo 调整价 / 收盘价'},character(1)),获取时间=substr(d$retrieved_at,1,16),模式=ifelse(d$cache_used,'真实缓存','联网获取'),check.names=FALSE)})
  output$csv<-downloadHandler(filename=function()'组合结果.csv',content=function(file){
    req(state());s<-state();v<-frontier_view(s,'full');d<-data.frame(type=character(),asset=character(),weight=numeric(),return=numeric(),volatility=numeric(),sharpe=numeric(),check.names=FALSE)
    add<-function(type,w,cash=0,st=NULL){if(is.null(st))st<-complete_stats(w,cash,s$est$mu,s$est$sigma,s$rf);assets<-names(s$est$mu);if(isTRUE(s$has_rf)){assets<-c(assets,'无风险资产');w<-c(w,cash)};d<<-rbind(d,data.frame(type=type,asset=assets,weight=w,return=unname(st[1]),volatility=unname(st[2]),sharpe=unname(st[3])))}
    add('gmv',s$opt$gmv);if(!is.null(s$opt$tangent))add('tangent',s$opt$tangent)
    for(branch in c('frontier','lower')){rows<-v[[branch]];for(i in seq_len(nrow(rows)))add(paste0(branch,'_',i),risky_target_weights(s$est$mu,s$est$sigma,rows$return[i],s$allow_short))}
    if(isTRUE(s$has_rf))for(branch in c('upper','lower')){p<-v$cash[[branch]];for(i in seq_along(p$weights))add(paste0('cash_',branch,'_',i),p$weights[[i]]$risky,p$weights[[i]]$cash)}
    p<-selected_portfolio();add('selected',p$risky,p$cash)
    if(!isTRUE(s$has_rf))d$sharpe<-NA_real_;d$allow_short<-s$allow_short;d$has_risk_free<-isTRUE(s$has_rf);d$allow_borrow<-isTRUE(s$allow_borrow);d$constraint<-if(s$allow_short)'net budget=1; unrestricted risky short selling' else 'net budget=1; risky weights>=0';d$method<-s$method;d$rf<-if(isTRUE(s$has_rf))s$rf else NA_real_;d$half_life_months<-if(s$method=='指数加权')s$half else NA;d$currency<-if(identical(s$input_mode,'manual'))'external model units' else 'CNY';d$frequency<-s$frequency;d$annualization_factor<-switch(s$frequency,daily=252,monthly=12,manual=NA_real_);d$sample_observations<-if(identical(s$input_mode,'manual'))NA_integer_ else nrow(s$data$x);d$sample_months<-if(s$frequency=='monthly')nrow(s$data$x) else NA;d$sample_start<-if(length(s$data$dates))as.character(min(s$data$dates)) else NA_character_;d$sample_end<-if(length(s$data$dates))as.character(max(s$data$dates)) else NA_character_;write.csv(d,file,row.names=FALSE,fileEncoding='UTF-8')})
  output$model_csv<-downloadHandler(filename=function()'年化模型参数.csv',content=function(file){req(state());e<-state()$est;d<-data.frame(asset=names(e$mu),expected_return=e$mu,standard_deviation=sqrt(diag(e$sigma)),e$sigma,check.names=FALSE);write.csv(d,file,row.names=FALSE,fileEncoding='UTF-8')})
  output$selected_csv<-downloadHandler(filename=function()'当前组合.csv',content=function(file){req(state());p<-selected_portfolio();d<-data.frame(asset=names(state()$est$mu),weight=p$risky);if(isTRUE(state()$has_rf))d<-rbind(d,data.frame(asset='无风险资产',weight=p$cash));d$expected_return<-unname(p$stats[1]);d$volatility<-unname(p$stats[2]);write.csv(d,file,row.names=FALSE,fileEncoding='UTF-8')})
  output$data_csv<-downloadHandler(filename=function()paste0('人民币',switch(state()$frequency,daily='日',monthly='月'),'收益.csv'),content=function(file){req(state(),!identical(state()$input_mode,'manual'));d<-state()$data;write.csv(data.frame(date=d$dates,frequency=state()$frequency,d$x,d$b,check.names=FALSE),file,row.names=FALSE,fileEncoding='UTF-8')})
  output$png<-downloadHandler(filename=function()paste0('投资机会集_',if(identical(plot_view()$view,'full'))'全景' else '局部','.png'),content=function(file){req(state());frontier_png(file,state(),plot_view())})
}
shinyApp(ui,server)
