import sys,unittest
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'python'))
import data
import pandas as pd
from unittest.mock import patch
class FrequencyTests(unittest.TestCase):
 def test_annual_gaps_and_incomplete(self):
  d=pd.DataFrame({'date':pd.to_datetime(['2019-12-31','2020-12-31','2022-12-30','2023-06-30']),'price':[100.,110.,150.,200.]})
  p=data.annual_prices(d,None,pd.Timestamp('2019-11-01'),pd.Timestamp('2023-06-30'))
  self.assertEqual(len(p),3)
  r=data.annual_returns(p);self.assertAlmostEqual(r.loc['2020'],.1);self.assertTrue(pd.isna(r.loc['2022']))
 def test_daily_common_calendar_and_missing(self):
  days=pd.bdate_range('2020-01-01','2020-06-30')
  def fake(kind,t,start,end,refresh=False):
   d=pd.DataFrame({'date':days,'price':range(100,100+len(days))})
   if t=='^GSPC':d=d[d.date!=pd.Timestamp('2020-02-10')]
   if t=='AAPL':d=d[d.date!=pd.Timestamp('2020-03-10')]
   return d,{'source':'unit test fixture','ticker':t}
  req={'assets':[{'market':'A','ticker':'600519'},{'market':'US','ticker':'AAPL'}],'start':'2020-01-01','end':'2020-06-30','frequency':'daily'}
  with patch.object(data,'daily',side_effect=fake):d=data.build(req)
  for day in ['2020-02-10','2020-03-10','2020-03-11']:self.assertNotIn(day,d['dates'])
  self.assertIn('2020-02-11',d['dates']);self.assertEqual(d['annualization'],252)
 def test_short_annual(self):
  fixture=pd.DataFrame({'date':pd.date_range('2019-12-31','2021-12-31',freq='YE'),'price':[100,110,120]})
  req={'assets':[{'market':'A','ticker':'600519'},{'market':'A','ticker':'601318'}],'start':'2020-01-01','end':'2021-12-31','frequency':'annual'}
  with patch.object(data,'daily',return_value=(fixture,{'ticker':'fixture'})):
   with self.assertRaisesRegex(ValueError,'至少需要3个完整自然年'):data.build(req)
if __name__=='__main__':unittest.main()
