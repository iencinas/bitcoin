# linear analyss

linear

ggplot(linear1)+
  geom_line(aes(dt,fit.max))+
  geom_line(aes(dt,fit.max1),linetype='longdash')+
  geom_line(aes(dt,fit.min))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max1),size=3,shape=0,fill=color1)+
  geom_point(aes(dt,max),size=4,shape=1,fill=color1)+
  geom_point(aes(dt,min1),size=4,shape=1,fill=color2)+
  geom_hline(aes(yintercept=200000),size=0.5,color='black',linetype='dotted')+
  scale_y_log10()




l3 <- linear1%>%filter(periode==3)

head(data.frame(l3),n=30)

l3 <- l3%>%group_by()%>%mutate(
  btc_i=price/max(price*(row_number()==1),na.rm = T),
  sp500_i=sp500/max(sp500*(row_number()==2),na.rm = T),
  money_i=money/max(money*(row_number()==23),na.rm = T),
  rate_i=rate/max(rate*(row_number()==2),na.rm = T),
  assets_i=assets/max(assets*(row_number()==4),na.rm = T),
  securities_i=securities/max(securities*(row_number()==4),na.rm = T)
)

ggplot(l3)+
  geom_line(aes(dt,log(10*btc_i,base = 10),color='BTC price (log)'))+
  geom_line(aes(dt,sp500_i,color='SP500 index'))+
  geom_point(aes(dt,money_i,color='Money supply'),size=1)+
  geom_path(aes(dt,(rate_i),color='Interest Rate'),size=1)+
  geom_point(aes(dt,assets_i,color='Assets hold by FED'),size=0.5)+
  labs(x='date',y='index',colour='')+
  # scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'top')+
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))


l4 <- linear1%>%filter(periode==4)

l4 <- l4%>%group_by()%>%mutate(
  btc_i=price/max(price*(row_number()==1),na.rm = T),
  sp500_i=sp500/max(sp500*(row_number()==1),na.rm = T),
  money_i=money/max(money*(row_number()==21),na.rm = T),
  rate_i=rate/max(rate*(row_number()==1),na.rm = T),
  assets_i=assets/max(assets*(row_number()==2),na.rm = T),
  securities_i=securities/max(securities*(row_number()==2),na.rm = T)
)
head(data.frame(l4),n=30)
ggplot(l4)+
  geom_line(aes(dt,log(10*btc_i,base = 10),color='BTC price (log)'))+
  geom_line(aes(dt,sp500_i,color='SP500 index'))+
  geom_point(aes(dt,money_i,color='Money supply'),size=1)+
  # geom_path(aes(dt,(rate_i),color='Interest Rate'),size=1)+
  geom_point(aes(dt,assets_i,color='Assets hold by FED'),size=0.5)+
  labs(x='date',y='index',colour='')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'top')+
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))



l5 <- linear1%>%filter(periode==5)

head(data.frame(l5),n=30)

l5 <- l5%>%group_by()%>%mutate(
  btc_i=price/max(price*(row_number()==1),na.rm = T),
  sp500_i=sp500/max(sp500*(row_number()==3),na.rm = T),
  money_i=money/max(money*(row_number()==12),na.rm = T),
  rate_i=rate/max(rate*(row_number()==3),na.rm = T),
  assets_i=assets/max(assets*(row_number()==5),na.rm = T),
  securities_i=securities/max(securities*(row_number()==2),na.rm = T)
)

ggplot(l5)+
  geom_line(aes(dt,log(10*btc_i,base = 10),color='BTC price (log)'))+
  geom_line(aes(dt,sp500_i,color='SP500 index'))+
  geom_point(aes(dt,money_i,color='Money supply'),size=1)+
  geom_path(aes(dt,(rate_i),color='Interest Rate'),size=1)+
  geom_point(aes(dt,assets_i,color='Assets hold by FED'),size=0.5)+
  labs(x='date',y='index',colour='')+
  # scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'top')+
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

#l4 deeper look

ggplot(l4)+
  geom_line(aes(dt,price,color='BTC price (log)'))+
  labs(x='date',y='index',colour='')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'top')+
  geom_vline(aes(xintercept=first_halving))+
  geom_vline(aes(xintercept=second_halving))+
  geom_vline(aes(xintercept=third_halving))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

l41 <- l4%>%filter(dt>'2021-01-01',dt<'2022-01-01')
ggplot(l41)+
  geom_line(aes(dt,log(btc_i,base = 10),color='BTC price (log)'))+
  labs(x='date',y='index',colour='')+
  scale_x_date(date_breaks = "1 year",date_labels='%Y')+
  theme(text = element_text(size = 16),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
        legend.position = 'top')
#find periodes to llok up


#halving dates

a=as.Date('2021-02-01')
b=as.Date('2021-03-10')
c=as.Date('2021-04-10')
d=as.Date('2021-05-01')
e=as.Date('2021-09-01')

ggplot(l41,aes(dt,price))+geom_line()+
  geom_vline(aes(xintercept=a))+
  geom_vline(aes(xintercept=b))+
  geom_vline(aes(xintercept=c))+
  geom_vline(aes(xintercept=d))+
  geom_vline(aes(xintercept=e))+
  labs(x='date',y='log($price)')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')

#find the max value between halvings
l41 <- l41%>%mutate(
  periode = case_when(
    dt<=a ~ 1,
    dt<=b ~ 2,
    dt<=c ~ 3,
    dt<=d ~ 4, #unfortunately htere are 2 maximums
    dt<=e ~ 5,
    TRUE ~6
  ))


l41 <- l41%>%group_by(periode)%>%mutate(max=ifelse(max(price)==price,price,NA),
                                        flag=ifelse(max(price)==price,1,0))

#find the min value between the max value and the next halving.
l41 <- l41%>%mutate(
  btw_periodes = case_when(
    dt>=(l41%>%filter(periode==1,flag==1))$dt & dt<=a ~ 1,
    dt>=(l41%>%filter(periode==2,flag==1))$dt & dt<=b ~ 2,
    dt>=(l41%>%filter(periode==3,flag==1))$dt & dt<=c ~ 3,
    dt>=(l41%>%filter(periode==4,flag==1))$dt & dt<=c ~ 4,  
    dt>=(l41%>%filter(periode==5,flag==1))$dt & dt<=e ~ 5,  
    dt>=(l41%>%filter(periode==6,flag==1))$dt ~ 6,
    TRUE ~ -1
  ))
# fourth_halving as.Date('2024-01-01') 

l41 <- l41%>%group_by(btw_periodes)%>%
  mutate(
    min=ifelse(min(price)==price & btw_periodes>0,price,NA),
    flag=ifelse(min(price)==price & btw_periodes>0,2,flag)
  )

ggplot(l41)+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)+
  scale_y_log10()+  
  geom_vline(aes(xintercept=a))+
  geom_vline(aes(xintercept=b))+
  geom_vline(aes(xintercept=c))+
  geom_vline(aes(xintercept=d))+
  geom_vline(aes(xintercept=e))+
  labs(x='date',y='price')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
