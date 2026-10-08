# 资产组合实验室 / Asset Portfolio Lab

本地运行的资产组合可视化。支持 A 股、港股和美股，使用简单收益，展示最小方差边界、GMV 和最大 Sharpe 组合。

## 功能

- 历史数据或手动参数两种模式；日度、月度历史收益，默认月度，结果统一年化。
- 手动输入预期收益、标准差与相关矩阵，或直接输入协方差矩阵，无需获取行情。
- 可选择是否包含无风险资产，以及是否允许按无风险利率借款。
- 历史均值、指数加权、CAPM 三种预期收益估计。
- 单一卖空开关：关闭时权重非负；开启时不限制负权重，净权重合计 100%。
- 显示可行组合散点；点击曲线、散点或资产查看配置、收益与波动。
- 导出当前视图 PNG、完整组合结果、模型参数、选中组合，以及历史样本收益 CSV。

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

## 手动参数与无风险资产

选择“手动输入”，输入 2–10 项风险资产。所有参数均为年化值。

标准差与相关系数模式，每行输入 `资产名称,预期收益(%),标准差(%)`：

```text
A,8,20
B,14,30
```

对应相关矩阵：

```text
1,0.2
0.2,1
```

协方差模式，每行输入 `资产名称,预期收益(%)`，矩阵使用小数收益单位；上例的协方差矩阵为：

```text
0.04,0.012
0.012,0.09
```

矩阵按资产顺序填写，必须对称、正定。协方差模式的标准差由矩阵对角线计算。

关闭“包含无风险资产”时，仅分析风险资产的机会集和最小方差边界。开启后，显示无风险资产、含无风险资产的有效前沿、可定义时的切点及整体 GMV（100% 无风险资产）。风险资产边界的上下半支均保留；含无风险资产的下半支不绘制。

“允许卖空”控制风险资产能否出现负权重；“允许按无风险利率借款”控制无风险资产能否出现负权重。组合查询中的风险资产投入超过 100% 表示借款，低于 0% 需要允许卖空。图中散点仅为可行集的代表性样本。CAPM 估计仍使用无风险利率，即使无风险资产未被列为可投资资产。

## 计算口径

日度对齐共同交易日；月度使用完整月份的月末价格。历史均值采用收益算术均值乘年化系数（日 252、月 12）；协方差乘相同系数。年化预期收益不是复合年增长率。日度至少 30 个共同交易日，月度至少 12 个月，观测数需多于股票数。

指数加权半衰期以月为单位，按实际时间距离衰减。CAPM 使用含截距回归估计 beta，预期收益采用无风险年利率加 beta 乘市场代理年化风险溢价，不加入回归 alpha。A/H/US 市场代理分别为沪深300、恒生和标普500价格指数。

## 数据来源

A/H 股使用 AKShare 新浪复权价格；美股优先 Yahoo/yfinance 调整收盘价，备用为经因子重建校验的新浪前复权价格。汇率优先 Yahoo，备用为美联储 FRED。两源的复权、股息与报价时间口径可能不同。汇率只使用当天或此前最多 7 天的有效报价；股票缺价不填零、不向前填补。外部接口可能限流或发生变化。

## 测试

```sh
Rscript tests/test_core.R
Rscript tests/test_shorts.R
Rscript tests/test_frequency.R
Rscript tests/test_plot.R
Rscript tests/test_manual.R
Rscript tests/test_manual_app.R
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
