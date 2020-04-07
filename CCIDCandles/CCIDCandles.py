import pandas as pd
# import pandas_datareader as datareader
import numpy as np
import matplotlib.pyplot as plt
import datetime
from datetime import datetime, timedelta
#from matplotlib.finance import candlestick2_ohlc
from mpl_finance import candlestick2_ohlc
import matplotlib.dates as mdates

for symbol in ['bch','btc','dash','etc','eth','ltc','rep','str','xmr','xrp','zec']:
   #symbol = 'bch'
   df = pd.read_csv('/Data/{}_usdt.csv'.format(symbol),
                    names = ["Date","High","Low","Open","Close","volume", "quoteVolume", "weightedAverage"])
   df = df[["Date","High","Low","Open","Close"]]

   # Converting date to pandas datetime format
   df['Date'] = [datetime.fromtimestamp(dat) - timedelta(hours=2) for dat in df['Date'].tolist()]

   ohlc = df[['Date', 'Open', 'High', 'Low','Close']].copy()
   ohlc = ohlc.groupby(df.index // 12 * 12).agg({'Date': lambda x: x.head(1),
                    'Open': lambda x: x.head(1),
                    'High':'max',
                    'Low': 'min',
                    'Close': lambda x: x.tail(1)})

   fig, ax = plt.subplots()
   plt.xlabel("Date")
   plt.ylabel("{} Price". format(symbol.upper()))
   ax.set_xticks(np.arange(len(ohlc),step = 7*24))
   ax.set_xticklabels([ohlc['Date'].tolist()[i].to_pydatetime().date() for i in np.arange(len(ohlc),step = 7*24)], fontsize=6, rotation=-90)
   candlestick2_ohlc(ax, ohlc.Open, ohlc.High, ohlc.Low, ohlc.Close, width=0.5, colorup='g')
   plt.tight_layout()
   plt.savefig('/CCID/Figures/{}_HF_60min.pdf'.format(symbol),transparent=True)