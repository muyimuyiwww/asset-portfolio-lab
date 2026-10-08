# 资产组合实验室 / Asset Portfolio Lab

本地运行的资产组合可视化。支持 A 股、港股和美股，使用简单收益，展示最小方差边界、GMV 和最大 Sharpe 组合。

## 功能

- 日度、月度收益，默认月度；收益和波动统一年化。
- 历史均值、指数加权、CAPM 三种预期收益估计。
- 单一卖空开关：关闭时权重非负；开启时不限制负权重，净权重合计 100%。
- 深红色有效上半支、浅红色非有效下半支，保留 GMV 与切点标记。
- 切点超出局部范围时才显示局部/全景切换。允许卖空时曲线继续延伸过可见切点；不卖空时标注最高收益可行端点。
- 导出当前视图 PNG、组合结果 CSV 和样本收益 CSV。

## 安装与运行

需要先安装 R 和 Python 3，并让 `Rscript`、`python3` 可以从终端调用。依赖版本记录于 `renv.lock` 和 `requirements.lock`。首次安装需要网络；原开发环境使用 R 4.6.1、Python 3.14。

macOS：在项目目录打开终端，执行以下命令安装依赖并启动：

```sh
zsh setup.command
zsh 启动工具.command
```

网页下载的文件可能没有执行权限。若希望以后双击启动，先在项目目录执行 `chmod +x setup.command 启动工具.command`。

浏览器地址为 `http://127.0.0.1:8765`。关闭启动窗口或按 Control+C 停止服务。服务只监听本机。

跨平台手动安装：创建 `.venv`，使用其中的 Python 安装 `requirements.lock`；创建 `.Rlib`，通过 `renv::restore(library=".Rlib", prompt=FALSE)` 恢复 R 依赖。在项目根目录运行 `Rscript start.R`。

每行股票输入采用 `市场,代码`，例如：

```text
A,600519
H,00700
US,AAPL
```

输入 2–10 只股票，选择区间和参数后点击计算。首次获取行情可能较慢；缓存仅保存在本机。

## 计算口径

日度对齐共同交易日；月度使用完整月份的月末价格。历史均值采用收益算术均值乘年化系数（日 252、月 12）；协方差乘相同系数。年化预期收益不是复合年增长率。日度至少 30 个共同交易日，月度至少 12 个月，观测数需多于股票数。

指数加权半衰期以月为单位，按实际时间距离衰减。CAPM 使用含截距回归估计 beta，预期收益采用无风险年利率加 beta 乘市场代理年化风险溢价，不加入回归 alpha。A/H/US 市场代理分别为沪深300、恒生和标普500价格指数。

允许卖空时使用无约束 Markowitz 解析解；不允许卖空时采用二次规划。某些参数不存在有限权重的最大 Sharpe 组合，页面会说明原因。无限制卖空时边界无界，图中仅展示有限区段，不通过绘图范围限制持仓。不计交易、融资和借券费用。

## 数据来源

A/H 股使用 AKShare 新浪复权价格；美股优先 Yahoo/yfinance 调整收盘价，备用为经因子重建校验的新浪前复权价格。汇率优先 Yahoo，备用为美联储 FRED。两源的复权、股息与报价时间口径可能不同。汇率只使用当天或此前最多 7 天的有效报价；股票缺价不填零、不向前填补。外部接口可能限流或发生变化。

不需要用户提供 API 密钥。该工具用于课堂和方法探索。

## 测试

无需行情或个人文件的检查：

```sh
Rscript tests/test_core.R
Rscript tests/test_shorts.R
Rscript tests/test_frequency.R
Rscript tests/test_plot.R
.venv/bin/python tests/test_data.py
.venv/bin/python tests/test_frequency_data.py
```

涉及外部行情的集成测试为可选项，需要网络或相同区间的本地缓存：

```sh
RUN_LIVE_TESTS=1 Rscript tests/test_app.R
RUN_LIVE_TESTS=1 Rscript tests/test_frequency_app.R
RUN_LIVE_TESTS=1 Rscript tests/test_one_year.R
RUN_LIVE_TESTS=1 Rscript tests/test_live.R
```

