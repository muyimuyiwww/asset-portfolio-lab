"""Daily adjusted prices -> complete-month CNY simple returns. Never synthesize data."""
import os,sys,json,time,hashlib,re
from pathlib import Path
import pandas as pd
import numpy as np
import akshare as ak
import yfinance as yf
import requests
_original_request=requests.sessions.Session.request
def _bounded_request(self,*args,**kwargs):
 kwargs.setdefault("timeout",20)
 return _original_request(self,*args,**kwargs)
requests.sessions.Session.request=_bounded_request
ROOT=Path(__file__).resolve().parents[1]; CACHE=ROOT/'cache'; CACHE.mkdir(exist_ok=True)
def normalize(m,t):
 t=t.strip().upper()
 if m=='A':
  t=t.lower()
  if re.fullmatch(r'\d{6}',t):t=('sh' if t.startswith(('6','9')) else 'bj' if t.startswith(('4','8')) else 'sz')+t
  if not re.fullmatch(r'(sh|sz|bj)\d{6}',t):raise ValueError('A股请填写六位代码或 sh/sz/bj 前缀代码。')
 elif m=='H':
  t=t.removesuffix('.HK').zfill(5)
  if not re.fullmatch(r'\d{5}',t):raise ValueError('港股请填写五位代码，例如 00700。')
 elif m=='US':
  if not re.fullmatch(r'[A-Z][A-Z0-9.\-]{0,14}',t):raise ValueError('美股代码格式无效。')
 else:raise ValueError('市场只能为 A、H、US。')
 return t

def yahoo(t,start,end,stock=False):
 d=yf.Ticker(t).history(start=str(start.date()),end=str((end+pd.Timedelta(days=1)).date()),auto_adjust=False,actions=True,raise_errors=True,timeout=20)
 if d.empty:raise ValueError('Yahoo 没有返回价格')
 column='Adj Close' if stock else 'Close'
 if column not in d:raise ValueError('未返回明确调整价，拒绝用未复权收盘价替代')
 idx=pd.to_datetime(d.index).tz_localize(None).normalize()
 out=pd.DataFrame({'date':idx,'price':d[column].to_numpy()})
 if 'Volume' in d and stock:out=out[d.Volume.to_numpy()>0]
 return out,f'Yahoo/yfinance {column}; auto_adjust=False'

def fallback(kind,t,start,end):
 if kind=='US':
  if t in {'AI','CIEN'}:raise ValueError('新浪对此代码存在已知复权因子异常，备用源已禁用')
  adjusted=ak.stock_us_daily(t,adjust='qfq')
  raw=ak.stock_us_daily(t,adjust='')
  factors=ak.stock_us_daily(t,adjust='qfq-factor')
  for d in (adjusted,raw,factors):d['date']=pd.to_datetime(d.date).astype('datetime64[ns]')
  factors['qfq_factor']=pd.to_numeric(factors.qfq_factor);factors['adjust']=pd.to_numeric(factors.adjust)
  verify=pd.merge_asof(raw.sort_values('date'),factors.sort_values('date'),on='date',direction='backward').merge(adjusted[['date','close']],on='date',suffixes=('_raw','_adj'))
  verify=verify[(verify.date>=start)&(verify.date<=end)]
  expected=verify.close_raw*verify.qfq_factor+verify.adjust
  if verify.empty or not np.allclose(expected,verify.close_adj,atol=.011,rtol=1e-5):raise ValueError('备用美股复权价未通过因子重建校验')
  if ((verify.qfq_factor!=1)|(verify.adjust!=0)).any() and np.allclose(verify.close_raw,verify.close_adj):raise ValueError('备用美股复权参数未实际生效')
  adjusted=adjusted.rename(columns={'close':'price'})
  adjusted=adjusted[adjusted.volume>0]
  return adjusted[['date','price']], 'AKShare/Sina qfq; factor reconstruction verified; cash adjustment differs from Yahoo'
 if kind=='INDEX':
  d=ak.stock_hk_index_daily_sina('HSI') if t=='^HSI' else ak.index_us_stock_sina('.INX')
  return d.rename(columns={'close':'price'})[['date','price']], 'AKShare/Sina '+('HSI' if t=='^HSI' else 'S&P500')+' price index'
 if kind=='FX':
  import io
  series={'CNY=X':'DEXCHUS','HKD=X':'DEXHKUS'}[t]
  r=requests.get('https://fred.stlouisfed.org/graph/fredgraph.csv',params={'id':series,'cosd':start.strftime('%Y-%m-%d'),'coed':end.strftime('%Y-%m-%d')});r.raise_for_status()
  d=pd.read_csv(io.StringIO(r.text)).rename(columns={'observation_date':'date',series:'price'})
  return d[['date','price']], 'Federal Reserve/FRED '+series+' daily noon buying rate; units per USD'
 raise ValueError('无可核实备用来源')

def daily(kind,t,start,end,refresh=False):
 key=hashlib.sha256(f'v3|{kind}|{t}|{start}|{end}'.encode()).hexdigest();p=CACHE/(key+'.json')
 if p.exists() and not refresh:
  payload=json.loads(p.read_text());payload['cache_used']=True
  return pd.DataFrame(payload['rows']).assign(date=lambda d:pd.to_datetime(d.date).astype('datetime64[ns]')),payload
 try:
  if kind=='A':
   d=ak.stock_zh_a_daily(symbol=t,start_date=start.strftime('%Y%m%d'),end_date=end.strftime('%Y%m%d'),adjust='hfq');src='AKShare/Sina stock_zh_a_daily hfq'
  elif kind=='H':d=ak.stock_hk_daily(symbol=t,adjust='hfq');src='AKShare/Sina stock_hk_daily hfq'
  elif kind=='CSI':d=ak.stock_zh_index_daily(symbol='sh000300');src='AKShare/Sina CSI300 price index'
  else:
   try:d,src=yahoo(t,start,end,stock=kind=='US')
   except Exception as primary_error:
    d,src=fallback(kind,t,start,end);src+=' [Yahoo unavailable: '+str(primary_error)+']'
  if kind in ('A','H','CSI'):
   if 'volume' in d and kind!='CSI':d=d[pd.to_numeric(d.volume,errors='coerce')>0]
   d=d.rename(columns={'close':'price'})[['date','price']]
  d=d.copy();d['date']=pd.to_datetime(d.date).astype('datetime64[ns]').dt.normalize();d['price']=pd.to_numeric(d.price,errors='coerce')
  d=d[(d.date>=start)&(d.date<=end)&np.isfinite(d.price)&(d.price>0)].sort_values('date').drop_duplicates('date',keep='last')
  if d.empty:raise ValueError('返回空价格或非正价格')
  payload={'source':src,'retrieved_at':pd.Timestamp.now(tz='Asia/Shanghai').isoformat(),'kind':kind,'ticker':t,'native_currency':{'A':'CNY','H':'HKD','US':'USD','CSI':'CNY','INDEX':('HKD' if t=='^HSI' else 'USD'),'FX':'units per USD'}[kind],'cache_used':False,'rows':json.loads(d.assign(date=d.date.dt.strftime('%Y-%m-%d')).to_json(orient='records'))}
  temp=p.with_suffix('.tmp');temp.write_text(json.dumps(payload,ensure_ascii=False));temp.replace(p)
  return d,payload
 except Exception as e:
  if p.exists():
   payload=json.loads(p.read_text());payload['cache_used']=True;payload['refresh_error']=str(e)
   return pd.DataFrame(payload['rows']).assign(date=lambda d:pd.to_datetime(d.date).astype('datetime64[ns]')),payload
  raise ValueError(f'{kind} {t} 获取失败且没有同区间真实缓存：{e}') from e

def month_prices(d,fx,start,end,calendar=None):
 # Use each market's actual final trading date. A suspended stock missing it loses this month.
 d=d.copy();d['month']=d.date.dt.to_period('M')
 p=d.groupby('month',sort=True).tail(1).copy()
 if calendar is not None:
  cal=calendar.copy();cal['month']=cal.date.dt.to_period('M');last=cal.groupby('month').date.max()
  p=p[p.apply(lambda r:r.date==last.get(r.month,pd.NaT),axis=1)]
 if fx is not None:
  merged=pd.merge_asof(p.sort_values('date'),fx.rename(columns={'date':'fx_date','price':'fx'}).sort_values('fx_date'),left_on='date',right_on='fx_date',direction='backward',tolerance=pd.Timedelta(days=7))
  p=merged; p['price']=p.price*p.fx
 p=p[(p.month.dt.start_time>=start)&(p.month.dt.end_time.dt.normalize()<=end)]
 return p.set_index('month').price

def monthly_returns(p):
 # Reindex calendar before shifting so gaps never turn into multi-month returns.
 if p.empty:raise ValueError('所选区间没有有效完整月价格，请更换代码或延长日期区间。')
 full=p.reindex(pd.period_range(p.index.min(),p.index.max(),freq='M'))
 return full/full.shift(1)-1

def converted_daily(d,fx):
 p=d.sort_values('date').copy()
 if fx is not None:
  p=pd.merge_asof(p,fx.rename(columns={'date':'fx_date','price':'fx'}).sort_values('fx_date'),left_on='date',right_on='fx_date',direction='backward',tolerance=pd.Timedelta(days=7))
  p['price']=p.price*p.fx
 return p.set_index('date').price

def annual_prices(d,fx,start,end,calendar=None):
 p=d.copy();p['year']=p.date.dt.to_period('Y')
 p=p.groupby('year',sort=True).tail(1)
 if calendar is not None:
  cal=calendar.copy();cal['year']=cal.date.dt.to_period('Y');last=cal.groupby('year').date.max()
  p=p[p.apply(lambda r:r.date==last.get(r.year,pd.NaT),axis=1)]
 if fx is not None:
  p=pd.merge_asof(p.sort_values('date'),fx.rename(columns={'date':'fx_date','price':'fx'}).sort_values('fx_date'),left_on='date',right_on='fx_date',direction='backward',tolerance=pd.Timedelta(days=7))
  p['price']=p.price*p.fx
 p=p[(p.year.dt.end_time.dt.normalize()>=start)&(p.year.dt.end_time.dt.normalize()<=end)]
 return p.set_index('year').price

def annual_returns(p):
 if p.empty:raise ValueError('年频需要完整自然年的年末价格，请延长历史区间。')
 full=p.reindex(pd.period_range(p.index.min(),p.index.max(),freq='Y'))
 return full/full.shift(1)-1

def build(req):
 frequency=req.get('frequency','monthly')
 if frequency not in ('daily','monthly','annual'):raise ValueError('收益频率必须为日、月或年。')
 assets=req['assets'];start=pd.Timestamp(req['start']);end=min(pd.Timestamp(req['end']),(pd.Timestamp.now(tz='Asia/Shanghai').tz_localize(None).normalize()-pd.Timedelta(days=1) if frequency=='daily' else pd.Timestamp.now(tz='Asia/Shanghai').tz_localize(None).normalize().replace(day=1)-pd.Timedelta(days=1)))
 if start>=end:raise ValueError('日期区间无效，请选至少12个月收益所需的完整月份。')
 if not 2<=len(assets)<=10:raise ValueError('请选择2至10只股票。')
 for a in assets:a['ticker']=normalize(a['market'],a['ticker'])
 if len({(a['market'],a['ticker']) for a in assets})!=len(assets):raise ValueError('同一市场的股票代码重复。')
 fetch_start=start-pd.DateOffset(months=1)-pd.Timedelta(days=10);meta=[];refresh=req.get('refresh',False)
 def get(k,t):
  d,m=daily(k,t,fetch_start,end,refresh);meta.append({x:y for x,y in m.items() if x!='rows'});return d
 markets=list(dict.fromkeys(a['market'] for a in assets));bench={};series={};fxs={'A':None}
 if any(m in markets for m in ('H','US')):
  cny=get('FX','CNY=X');fxs['US']=cny
  if 'H' in markets:
   hkd=get('FX','HKD=X');joined=pd.merge_asof(cny.sort_values('date'),hkd.rename(columns={'price':'hkd','date':'hkd_date'}).sort_values('hkd_date'),left_on='date',right_on='hkd_date',direction='backward',tolerance=pd.Timedelta(days=7));fxs['H']=joined[['date','price']].assign(price=joined.price/joined.hkd).dropna()
 for m in markets:
  b=get('CSI' if m=='A' else 'INDEX',{'A':'sh000300','H':'^HSI','US':'^GSPC'}[m]);bench[m]=b
  series['benchmark_'+m]=(converted_daily(b,fxs[m]) if frequency=='daily' else annual_returns(annual_prices(b,fxs[m],fetch_start,end)) if frequency=='annual' else monthly_returns(month_prices(b,fxs[m],fetch_start,end)))
 for a in assets:
  d=get(a['market'],a['ticker']);series[a['market']+':'+a['ticker']]=(converted_daily(d,fxs[a['market']]) if frequency=='daily' else annual_returns(annual_prices(d,fxs[a['market']],fetch_start,end,bench[a['market']])) if frequency=='annual' else monthly_returns(month_prices(d,fxs[a['market']],fetch_start,end,bench[a['market']])))
 joined=pd.DataFrame(series)
 if frequency=='daily':
  # All prices share the same observed trading dates before returns are computed.
  # Missing/suspended prices break adjacent returns; they are never filled.
  calendars=[set(b.date) for b in bench.values()]
  common_calendar=sorted(set.intersection(*calendars))
  joined=joined.reindex(common_calendar)
  joined=joined/joined.shift(1)-1
  joined=joined[(joined.index>=start)&(joined.index<=end)]
 else:joined=joined[(joined.index.start_time>=start)&(joined.index.end_time.normalize()<=end)]
 requested=len(joined);common=joined.dropna()
 minimum=max(30,len(assets)+1) if frequency=='daily' else max(3,len(assets)+1) if frequency=='annual' else max(12,len(assets)+1)
 unit={'daily':'个共同交易日','monthly':'个月','annual':'个完整自然年'}[frequency]
 if len(common)<minimum:raise ValueError(f'共同有效收益只有{len(common)}{unit}，至少需要{minimum}{unit}。请延长区间或更换上市较短、停牌或缺数的股票。')
 return {'dates':[str(p.date() if frequency=='daily' else p.to_timestamp(how='end' if frequency=='annual' else 'start').date()) for p in common.index],'assets':assets,'values':common[[a['market']+':'+a['ticker'] for a in assets]].to_numpy().tolist(),'benchmarks':{m:common['benchmark_'+m].tolist() for m in markets},'metadata':meta,'dropped_months':requested-len(common),'effective_end':str(end.date()),'currency':'CNY','frequency':frequency,'annualization':{'daily':252,'monthly':12,'annual':1}[frequency]}
if __name__=='__main__':
 try:
  req=json.loads(Path(sys.argv[1]).read_text());out=build(req);Path(sys.argv[2]).write_text(json.dumps(out,ensure_ascii=False,allow_nan=False))
 except Exception as e:
  Path(sys.argv[2]).write_text(json.dumps({'error':str(e)},ensure_ascii=False));sys.exit(1)
