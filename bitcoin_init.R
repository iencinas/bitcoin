library(tidyverse)
library(crypto2)
library(lubridate)

color1='#E69F00'
color2='#3F61A6'
color3='#E600BA'



#data from here
#https://www.investing.com/crypto/bitcoin/historical-data
btc_csv <- read_csv('data/btc.csv')
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
    TRUE~4))

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
ggplot(btc)+
  geom_line(aes(dt,price,color=factor(periode)))+
  geom_point(aes(dt,max),size=2,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=2,shape=25,fill=color2)+
  scale_y_log10()+  
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  labs(x='date',y='log($price)')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'none')


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
                       periode==4~'2021-now'
                       ))
btc_winter$point <- with(btc_winter,(dt%in%max_v)*days_after_max)
btc_winter$now <- with(btc_winter,(dt==max(btc$dt))*days_after_max)

ggplot(btc_winter)+
  geom_line(aes(days_after_max,i,color=factor(t)),size=1)+
  labs(x='days since maximum',y='% from maximum',title='Bitcoin winter, from maximum to minimum',
       color='Crypto Winters')+
  theme(legend.position = c(0.8, 0.8))+
  geom_point(aes(days_after_max,min_i),size=3,shape=25,fill=color2)+
  scale_y_continuous(labels = scales::percent)

  


#--------------
#crypto bubble
#--------------
#get dates of max and min values per periode
max_v <- (btc%>%filter(flag==1))$dt
min_v <- (btc%>%filter(flag==2))$dt


#create a separate dataframe
delta=0 #days after minimum to be shown
btc_bubble <- btc%>%filter(
  case_when(
    price==0 ~ F,
    dt<=max_v[1]  ~ TRUE,
    dt<=max_v[2]+delta & dt>=min_v[1]  ~ TRUE,
    dt<=max_v[3]+delta & dt>=min_v[2]  ~ TRUE,
    dt<=max_v[4]+delta & dt>=min_v[3]  ~ TRUE,
    dt>=min_v[4]  ~ TRUE,
    TRUE~F))

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

btc%>%filter(flag==1)



ggplot(btc_bubble)+
  geom_line(aes(days_after_max,i,color=factor(t)),size=1)+
  labs(x='days since minimum',y='log(% from the previous minimum)',title='Bitcoin bubble, from minimum to maximum',
       color='Crypto Bubbles \n (year of maximum)')+
  theme(legend.position = c(0.8, 0.3))+
  geom_point(aes(days_after_max,max_i),size=3,shape=24,fill=color1)+
  scale_y_log10(breaks=c(0.1,0.2,0.5,1,2,5,10,20,50,100,200,400,600),labels = scales::percent)



