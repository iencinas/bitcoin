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


