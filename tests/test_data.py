import sys,unittest,tempfile,json
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'python'))
import data
import pandas as pd
from unittest.mock import patch
class DataTests(unittest.TestCase):
 def test_codes(self):
  self.assertEqual(data.normalize('A','600519'),'sh600519');self.assertEqual(data.normalize('H','700'),'00700')
  for m,t in [('A','foo'),('H','abc'),('US','../../'),('XX','AAPL')]:
   with self.assertRaises(ValueError):data.normalize(m,t)
 def test_gap(self):
  p=pd.Series([100.,120.],index=pd.PeriodIndex(['2020-01','2020-03'],freq='M'));r=data.monthly_returns(p);self.assertTrue(r.isna().all())
 def test_fx_asof(self):
  d=pd.DataFrame({'date':pd.to_datetime(['2020-01-31','2020-02-28']),'price':[10.,20.]});fx=pd.DataFrame({'date':pd.to_datetime(['2020-01-30','2020-02-29']),'price':[7.,99.]})
  p=data.month_prices(d,fx,pd.Timestamp('2020-01-01'),pd.Timestamp('2020-02-29'));self.assertEqual(p.iloc[0],70.);self.assertTrue(pd.isna(p.iloc[1]))
 def test_suspension(self):
  d=pd.DataFrame({'date':pd.to_datetime(['2020-01-20','2020-02-28']),'price':[10.,20.]});cal=pd.DataFrame({'date':pd.to_datetime(['2020-01-31','2020-02-28'])})
  p=data.month_prices(d,None,pd.Timestamp('2020-01-01'),pd.Timestamp('2020-02-29'),cal);self.assertEqual(len(p),1)
 def test_incomplete_month(self):
  d=pd.DataFrame({'date':pd.to_datetime(['2020-01-31','2020-02-10']),'price':[10.,20.]});p=data.month_prices(d,None,pd.Timestamp('2020-01-01'),pd.Timestamp('2020-02-15'));self.assertEqual(len(p),1)
 def test_fx_direction(self):self.assertAlmostEqual(7.2/7.8,.923076923)
 def test_cache(self):
  with tempfile.TemporaryDirectory() as tmp,patch.object(data,'CACHE',Path(tmp)),patch.object(data,'yahoo',return_value=(pd.DataFrame({'date':pd.to_datetime(['2020-01-02']),'price':[10.]}),'real-source')):
   _,m=data.daily('US','TEST',pd.Timestamp('2020-01-01'),pd.Timestamp('2020-01-31'));self.assertFalse(m['cache_used'])
   with patch.object(data,'yahoo',side_effect=RuntimeError('offline')),patch.object(data,'fallback',side_effect=RuntimeError('offline')):
    d,m=data.daily('US','TEST',pd.Timestamp('2020-01-01'),pd.Timestamp('2020-01-31'),True);self.assertTrue(m['cache_used']);self.assertEqual(m['refresh_error'],'offline');self.assertEqual(d.price.iloc[0],10.)
    with self.assertRaises(ValueError):data.daily('US','OTHER',pd.Timestamp('2020-01-01'),pd.Timestamp('2020-01-31'))
 def test_minimum_one_year(self):
  fixture=pd.DataFrame({'date':pd.date_range('2020-11-30','2021-12-31',freq='ME'),'price':range(100,114)})
  def fake_daily(kind,t,start,end,refresh=False):return fixture,{'source':'synthetic unit-test fixture','ticker':t}
  req={'assets':[{'market':'A','ticker':'600519'},{'market':'A','ticker':'601318'}],'start':'2021-01-01','end':'2021-12-31'}
  with patch.object(data,'daily',side_effect=fake_daily):
   self.assertEqual(len(data.build(req)['dates']),12)
   req['end']='2021-11-30'
   with self.assertRaisesRegex(ValueError,'至少需要12个月'):data.build(req)
 def test_invalid_request(self):
  with self.assertRaises(ValueError):data.build({'assets':[{'market':'US','ticker':'AAPL'}]*2,'start':'2020-01-01','end':'2025-01-01'})
if __name__=='__main__':unittest.main()
