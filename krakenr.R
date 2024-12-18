# install.packages('KrakenR')
library(KrakenR)


l <- getAssets(assets = "All")
head(l)

getAssets("BTC")

getTickers(pairs = "All")

p <- getPairs(pairs = "all", info = "info", country_code = NULL)
head(p)
p%>%filter(base=="BTC")
https://api.kraken.com/0/public/AssetPairs
getOB("XXBTZEUR", count = 10)
ob <- getOB("XXBTZEUR",count=500)
colnames(ob)
ask <- ob%>%select(Order_Type,Ask_Price,Ask_Volume,Ask_Timestamp)%>%complete()
ggplot(ask)+geom_line(aes(Ask_Price,Ask_Volume))
bid <- ob%>%select(Order_Type,Bid_Price,Bid_Volume,Bid_Timestamp)%>%complete()
ggplot(bid)+geom_line(aes(Bid_Price,Bid_Volume))

t <- getTrades("XXBTZEUR", since = NULL, count = NULL)
t%>%filter(Trade_ID==91810701)
colnames(t)
t%>%group_by(Order_Type,Execution_Type)%>%summarise(sum(Volume),mean(Price),median(Price))
getSpreads("XXBTZEUR", since = NULL, timestamp = FALSE)
s <- getSpreads("XXBTZEUR", since = "2024-10-01 00:00:00")
s
o <- getOHLC("XXBTZEUR", interval = "4h", since = "2024-01-01 00:00:00")
head(o)
tail(o)

test <- read.csv("data/data.csv")
head(test)
ggplot(test)+geom_line(aes(Date,Open,group='1'))
