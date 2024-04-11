library(tidyverse)
library(crypto2)
library(lubridate)
library(tidyquant)

color1='#E69F00'
color2='#3F61A6'
color3='#E600BA'



#data from here
#https://www.investing.com/crypto/bitcoin/historical-data
btc_csv <- read_csv(unzip('data/btc.zip','btc.csv'))
btc_csv$dt <- mdy(btc_csv$Date)


#crypto2
list_coins<-crypto_list(only_active = TRUE)
btc_ddbb<- crypto_history(limit =  1,)
btc_ddbb$dt <- as.Date(btc_ddbb$timestamp)



#chose dates of csv that are not in crypto2
aux <- btc_csv%>%filter(dt<min(btc_ddbb$dt))
max(aux$dt);min(btc_ddbb$dt)

head(btc_ddbb)
head(btc_csv)

#chose dt, open to merge, and use open as price
btc <- btc_ddbb[c('dt','open')]
colnames(btc) <- c('dt','price')

aux <- aux[c('dt','Open')]
colnames(aux)<- c('dt','price')

btc <- rbind(btc,aux)%>%arrange(dt)

#add row for printing purposes
btc$row <- 1:nrow(btc)

#-----------------
#Add non btc data
#-----------------
tidyquant::quandl_api_key('z1yzxQMhKEy_YBe8Xast')   
help(tq_get)
#interest rate
# https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html
library(fredr)
FRED_API_KEY="f833b8856f2712f70499e0eaf5a81303"
fredr_set_key(FRED_API_KEY)
rate <- fredr(
  series_id = "DPRIME",
  observation_start = as.Date("2010-12-16"),
  observation_end = as.Date("2024-01-01")
)

tail(rate)

#sp500 index
sp500 <- tq_get("^GSPC",get='stock.prices',from='2010-02-01')
#monetary base
money <- tq_get("BOGMBASE", get = "economic.data",from='2010-02-01')
#assets in fed
assets <- tq_get("WALCL", get = "economic.data",from='2010-02-01')
#assets that are securities
securities_assets <- tq_get("WSHOSHO", get = "economic.data",from='2010-02-01')
#join data
rate <- rate[,c('date','value')]
colnames(rate) <- c('dt','rate')
sp500 <- sp500[,c('date','adjusted')]
colnames(sp500) <- c('dt','sp500')
money <- money[,c('date','price')]
colnames(money) <- c('dt','money')
assets <- assets[,c('date','price')]
colnames(assets) <- c('dt','assets')
securities_assets <- securities_assets[,c('date','price')]
colnames(securities_assets) <- c('dt','securities')

tail(rate)

back <- btc
#btc <- back
btc <- btc%>%filter(price>0)
btc <- btc%>%left_join(sp500)
btc <- btc%>%left_join(money)
btc <- btc%>%left_join(rate)
btc <- btc%>%left_join(assets)
btc <- btc%>%left_join(securities_assets)


btc_1 <- btc%>%filter(price>1)
btc_1 <- btc_1[,2]

btc_1$text <- as.character(btc_1$price)
btc_1$f <- as.numeric(substr(btc_1$text,1,1))
btc_1 <- btc_1%>%group_by(f)%>%summarise(n=n())%>%group_by()%>%mutate(p=n/sum(n))
btc_1$b <- with(btc_1,log(1+1/f,base = 10))
ggplot(btc_1)+geom_line(aes(as.factor(f),p),group=1)+geom_line(aes(f,b,color='beford'))

#remove small volumen of btc?
#btc <- btc%>%filter(dt>='2011-04-01')

#halving dates

first_halving=as.Date('2012-11-28')
second_halving=as.Date('2016-07-09')
third_halving=as.Date('2020-05-11')

ggplot(btc,aes(dt,price))+geom_line()+scale_y_log10()+
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  labs(x='date',y='log($price)')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')

#find the max value between halvings
btc <- btc%>%mutate(
  periode = case_when(
    dt<=first_halving ~ 1,
    dt<=second_halving ~ 2,
    dt<=third_halving ~ 3,
    dt<=as.Date('2024-01-01')~4
  ))
    #TRUE~4))

btc <- btc%>%group_by(periode)%>%mutate(max=ifelse(max(price)==price,price,NA),
                                        flag=ifelse(max(price)==price,1,0))

#find the min value between the max value and the next halving.
btc <- btc%>%mutate(
  btw_periodes = case_when(
    dt>=(btc%>%filter(periode==1,flag==1))$dt & dt<=first_halving ~ 1,
    dt>=(btc%>%filter(periode==2,flag==1))$dt & dt<=second_halving ~ 2,
    dt>=(btc%>%filter(periode==3,flag==1))$dt & dt<=third_halving ~ 3,
    dt>=(btc%>%filter(periode==4,flag==1))$dt  ~ 4,
    TRUE~-1
  ))

btc <- btc%>%group_by(btw_periodes)%>%
  mutate(
    min=ifelse(min(price)==price & btw_periodes>0,price,NA),
    flag=ifelse(min(price)==price & btw_periodes>0,2,flag)
  )

#plot: colors = periodes between halvings, max and mins
p1 <- ggplot(btc)+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)+
  scale_y_log10()+  
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  labs(x='date',y='log($price)')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 18),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'none')

p1
png(file="plots/btc_halvings.png",width=600*1.5, height=350*1.5)
p1
dev.off()






#--------------
#crypto winter
#--------------

#get dates of max and min values per periode
max_v <- (btc%>%filter(flag==1))$dt
min_v <- (btc%>%filter(flag==2))$dt


#create a separate dataframe
delta=0 #days after minimum to be shown
btc_winter <- btc%>%filter(
  case_when(
    dt<=min_v[1]+delta & dt>=max_v[1]  ~ TRUE,
    dt<=min_v[2]+delta & dt>=max_v[2]  ~ TRUE,
    dt<=min_v[3]+delta & dt>=max_v[3]  ~ TRUE,
    dt<=min_v[4]+delta & dt>=max_v[4]  ~ TRUE,
    TRUE~F))


#work with days after maximum and 
#with ratios from that maximum
btc_winter <- btc_winter%>%group_by(periode)%>%mutate(days_after_max=row_number(),base=max((days_after_max==1)*price))
btc_winter <- btc_winter%>%group_by(periode)%>%mutate(i=(price/base-1))
btc_winter <- btc_winter%>%group_by(periode)%>%mutate(min_i=(min/base-1))
#hand made naming periodes
btc_winter$t <- with(btc_winter,
                     case_when(
                       periode==1~'2011',
                       periode==2~'2013-2015',
                       periode==3~'2017-2018',
                       periode==4~'2021-2022'
                     ))
btc_winter$point <- with(btc_winter,(dt%in%max_v)*days_after_max)
btc_winter$now <- with(btc_winter,(dt==max(btc$dt))*days_after_max)

p2 <- ggplot(btc_winter)+
  geom_line(aes(days_after_max,i,color=factor(t)),linewidth=1)+
  labs(x='days since maximum',y='% from maximum',title='Bitcoin winter, from maximum to minimum',
       color='Crypto Winters')+
  theme(legend.position = c(0.8, 0.8))+
  geom_point(aes(days_after_max,min_i),size=3,shape=25,fill=color2)+
  scale_y_continuous(labels = scales::percent)+
  theme(text = element_text(size = 18))

p2
png(file="plots/winter.png",width=600*1.5, height=350*1.5)
p2
dev.off()


#-----------non crypto data
head(btc_winter,n=20)


#--------------
#crypto bubble
#--------------
#get dates of max and min values per periode
max_v <- (btc%>%filter(flag==1))$dt
min_v <- (btc%>%filter(flag==2))$dt


#create a separate dataframe
delta=0 #days after minimum to be shown
btc_bubble <- btc%>%dplyr::filter(
  case_when(
    dt<=max_v[1]  ~ TRUE,
    dt<=max_v[2]+delta & dt>=min_v[1]  ~ TRUE,
    dt<=max_v[3]+delta & dt>=min_v[2]  ~ TRUE,
    dt<=max_v[4]+delta & dt>=min_v[3]  ~ TRUE,
    dt>=min_v[4]  ~ TRUE,
    TRUE~FALSE
  )
)

btc_bubble <- btc_bubble%>%mutate(
  periode = case_when(
    dt<=max_v[1]  ~ 1,
    dt<=max_v[2]+delta & dt>=min_v[1]  ~ 2,
    dt<=max_v[3]+delta & dt>=min_v[2]  ~ 3,
    dt<=max_v[4]+delta & dt>=min_v[3]  ~ 4,
    dt>=min_v[4]  ~ 5,
    TRUE~10))
#work with days after maximum and 
#with ratios from that maximum
btc_bubble <- btc_bubble%>%group_by(periode)%>%mutate(days_after_max=row_number(),
                                                      base=max((days_after_max==1)*price))
btc_bubble <- btc_bubble%>%group_by(periode)%>%mutate(i=price/base-1)
btc_bubble <- btc_bubble%>%group_by(periode)%>%mutate(max_i=max/base-1)
#hand made naming periodes
btc_bubble$t <- with(btc_bubble,
                     case_when(
                       periode==1~'2011',
                       periode==2~'2013',
                       periode==3~'2017',
                       periode==4~'2021',
                       periode==5~'current'
                     ))

btc_bubble%>%group_by(periode,t)%>%summarise(n(),max(dt),min(dt))

today <- btc_bubble%>%filter(dt==max(btc_bubble$dt))
today$i
p3 <-   ggplot(btc_bubble)+
  geom_line(aes(days_after_max,i,color=factor(t)),size=1)+
  labs(x='days since minimum',y='log(% from the previous minimum)',title='Bitcoin bubble, from minimum to maximum',
       color='Crypto Bubbles \n (year of maximum)')+
  theme(legend.position = c(0.8, 0.3))+
  geom_point(aes(days_after_max,max_i),size=3,shape=24,fill=color1)+
  geom_point(data=today,aes(days_after_max,i),size=4,fill=color2,shape=21)+
  geom_label(data=today,aes(days_after_max+100,i-0.6,label='you are here'),hjust=0,size=6)+
  geom_segment(data=today,aes(x = days_after_max+100, y = i-0.6, xend = days_after_max+2, yend = i-0.1),
               arrow = arrow(type = 'closed',length = unit(0.3, "cm")))+
  scale_y_log10(breaks=c(0.1,0.2,0.5,1,2,5,10,20,50,100,200,400,600),labels = scales::percent)+
  theme(text = element_text(size = 18))



p3
png(file="plots/bubble.png",width=600*1.5, height=350*1.5)
p3
dev.off()

#-------------------
#extra info
#-------------------


data.frame(head(btc,n=30))
btc_extra <- btc

btc_extra <- btc_extra%>%filter(dt>=first_halving)
data.frame(head(btc_extra,n=30))
names(btc)
head(btc_extra)
btc_extra <- btc_extra%>%group_by()%>%mutate(
  btc_i=price/max(price*(row_number()==1),na.rm = T),
  sp500_i=sp500/max(sp500*(row_number()==1),na.rm = T),
  money_i=money/max(money*(row_number()==4),na.rm = T),
  rate_i=rate/max(rate*(row_number()==1),na.rm = T),
  assets_i=assets/max(assets*(row_number()==1),na.rm = T),
  securities_i=securities/max(securities*(row_number()==1),na.rm = T)
)


p10 <- ggplot(btc_extra)+
  geom_line(aes(dt,log(10*btc_i,base = 10),color='BTC price (log)'))+
  geom_line(aes(dt,sp500_i,color='SP500 index'))+
  geom_point(aes(dt,money_i,color='Money supply'),size=1)+
  geom_path(aes(dt,(rate_i),color='Interest Rate'),size=1)+
  geom_point(aes(dt,assets_i,color='Assets hold by FED'),size=0.5)+
  labs(x='date',y='index',colour='')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'top')+
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme(text = element_text(size = 20))

p10
png(file="plots/extra_info2.png",width=600*1.5, height=350*1.5)
p10
dev.off()

btt <- btc_extra[complete.cases(btc_extra[c('price','sp500')]),c('price','sp500')]
cor((btt$price),(btt$sp500))
a <- lag(btt$price)
b <- lead(btt$sp500)
a <- a[!is.na(a)]
b <- b[!is.na(b)]
cor(a,b)