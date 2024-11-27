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
e=as.Date('2021-05-29')

f=as.Date('2021-07-01')
h=as.Date('2021-10-02')
i=as.Date('2021-11-01')



ggplot(l41,aes(dt,price))+geom_line()+
  geom_vline(data=data.frame(l=c(a,b,c,d,e,f,h,i)),aes(xintercept=l))+
  labs(x='date',y='price')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')

#find the max value between halvings
l41 <- l41%>%mutate(
  periode = case_when(
    dt<=a ~ 1,
    dt<=b ~ 2,
    dt<=c ~ 3,
    dt<=d ~ 4, #unfortunately htere are 2 maximums
    dt<=e ~ 5,
    dt<=f ~ 6,
    dt<=h ~ 7,
    dt<=i ~ 8,
    TRUE ~9
  ))


l41 <- l41%>%group_by(periode)%>%mutate(max=ifelse(max(price)==price,price,NA),
                                        flag=ifelse(max(price)==price,1,0))

#find the min value between the max value and the next halving.
l41 <- l41%>%mutate(
  btw_periodes = case_when(
    dt>=(l41%>%filter(periode==1,flag==1))$dt & dt<=a ~ 1,
    dt>=(l41%>%filter(periode==2,flag==1))$dt & dt<=b ~ 2,
    dt>=(l41%>%filter(periode==3,flag==1))$dt & dt<=c ~ 3,
    dt>=(l41%>%filter(periode==4,flag==1))$dt & dt<=d ~ 4,  
    dt>=(l41%>%filter(periode==5,flag==1))$dt & dt<=e ~ 5,  
    dt>=(l41%>%filter(periode==6,flag==1))$dt & dt<=f ~ 6,  
    dt>=(l41%>%filter(periode==7,flag==1))$dt & dt<=h ~ 7,  
    dt>=(l41%>%filter(periode==8,flag==1))$dt & dt<=i ~ 8,  
    dt>=(l41%>%filter(periode==9,flag==1))$dt ~ 9,
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
  geom_vline(data=data.frame(l=c(a,b,c,d,e,f,h,i)),aes(xintercept=l))+
  labs(x='date',y='price')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



#--------------
#crypto winter
#--------------

#get dates of max and min values per periode
max_v <- (l41%>%filter(flag==1))$dt
min_v <- (l41%>%filter(flag==2))$dt


#create a separate dataframe
delta=0 #days after minimum to be shown
l41b <- l41%>%filter(
  case_when(
    dt<=min_v[1]+delta & dt>=max_v[1]  ~ TRUE,
    dt<=min_v[2]+delta & dt>=max_v[2]  ~ TRUE,
    dt<=min_v[3]+delta & dt>=max_v[3]  ~ TRUE,
    dt<=min_v[4]+delta & dt>=max_v[4]  ~ TRUE,
    dt<=min_v[5]+delta & dt>=max_v[5]  ~ TRUE,
    dt<=min_v[6]+delta & dt>=max_v[6]  ~ TRUE,
    dt<=min_v[7]+delta & dt>=max_v[7]  ~ TRUE,
    dt<=min_v[8]+delta & dt>=max_v[8]  ~ TRUE,
    dt<=min_v[9]+delta & dt>=max_v[9]  ~ TRUE,
    TRUE~F))


#work with days after maximum and 
#with ratios from that maximum
l41b <- l41b%>%group_by(periode)%>%mutate(days_after_max=row_number(),base=max((days_after_max==1)*price))
l41b <- l41b%>%group_by(periode)%>%mutate(i=(price/base-1))
l41b <- l41b%>%group_by(periode)%>%mutate(min_i=(min/base-1))
#hand made naming periodes
l41b$t <- with(l41b,
                     case_when(
                       periode==1~'a',
                       periode==2~'b',
                       periode==3~'c',
                       periode==4~'d',
                       periode==5~'e',
                       periode==6~'f',
                       periode==7~'h',
                       periode==8~'i',
                       periode==9~'j',
                     ))
l41b$point <- with(l41b,(dt%in%max_v)*days_after_max)
l41b$now <- with(l41b,(dt==max(btc$dt))*days_after_max)

ggplot(l41b)+
  geom_line(aes(days_after_max,i,color=factor(t)),linewidth=1)+
  labs(x='days since maximum',y='% from maximum',title='Bitcoin winter, from maximum to minimum',
       color='Crypto Winters')+
  theme(legend.position = c(0.8, 0.8))+
  geom_point(aes(days_after_max,min_i),size=3,shape=25,fill=color2)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(0,100,by=10))+
  theme(text = element_text(size = 18))

# -3 days to -10 days of each pick
max_v <- (l41%>%filter(flag==1))$dt
de=10
d=2
l41$deltas <- with(l41,
  case_when(
    dt<=max_v[1]+de & dt>=max_v[1]-d  ~ 1,
    dt<=max_v[2]+de & dt>=max_v[2]-d  ~ 2,
    dt<=max_v[3]+de & dt>=max_v[3]-d  ~ 3,
    dt<=max_v[4]+de & dt>=max_v[4]-d  ~ 4,
    dt<=max_v[5]+de & dt>=max_v[5]-d  ~ 5,
    dt<=max_v[6]+de & dt>=max_v[6]-d  ~ 6,
    TRUE~-1))

ggplot(l41%>%filter(deltas==1))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)

ggplot(l41%>%filter(deltas==2))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)

ggplot(l41%>%filter(deltas==3))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)

ggplot(l41%>%filter(deltas==4))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)




#l5 deeper look

ggplot(l5)+
  geom_line(aes(dt,price,color='BTC price (log)'))+
  labs(x='date',y='index',colour='')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')

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
e=as.Date('2021-05-29')

f=as.Date('2021-07-01')
h=as.Date('2021-10-02')
i=as.Date('2021-11-01')



ggplot(l41,aes(dt,price))+geom_line()+
  geom_vline(data=data.frame(l=c(a,b,c,d,e,f,h,i)),aes(xintercept=l))+
  labs(x='date',y='price')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')

#find the max value between halvings
l41 <- l41%>%mutate(
  periode = case_when(
    dt<=a ~ 1,
    dt<=b ~ 2,
    dt<=c ~ 3,
    dt<=d ~ 4, #unfortunately htere are 2 maximums
    dt<=e ~ 5,
    dt<=f ~ 6,
    dt<=h ~ 7,
    dt<=i ~ 8,
    TRUE ~9
  ))


l41 <- l41%>%group_by(periode)%>%mutate(max=ifelse(max(price)==price,price,NA),
                                        flag=ifelse(max(price)==price,1,0))

#find the min value between the max value and the next halving.
l41 <- l41%>%mutate(
  btw_periodes = case_when(
    dt>=(l41%>%filter(periode==1,flag==1))$dt & dt<=a ~ 1,
    dt>=(l41%>%filter(periode==2,flag==1))$dt & dt<=b ~ 2,
    dt>=(l41%>%filter(periode==3,flag==1))$dt & dt<=c ~ 3,
    dt>=(l41%>%filter(periode==4,flag==1))$dt & dt<=d ~ 4,  
    dt>=(l41%>%filter(periode==5,flag==1))$dt & dt<=e ~ 5,  
    dt>=(l41%>%filter(periode==6,flag==1))$dt & dt<=f ~ 6,  
    dt>=(l41%>%filter(periode==7,flag==1))$dt & dt<=h ~ 7,  
    dt>=(l41%>%filter(periode==8,flag==1))$dt & dt<=i ~ 8,  
    dt>=(l41%>%filter(periode==9,flag==1))$dt ~ 9,
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
  geom_vline(data=data.frame(l=c(a,b,c,d,e,f,h,i)),aes(xintercept=l))+
  labs(x='date',y='price')+
  scale_x_date(date_breaks = "1 months",date_labels='%Y-%m')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



#--------------
#crypto winter
#--------------

#get dates of max and min values per periode
max_v <- (l41%>%filter(flag==1))$dt
min_v <- (l41%>%filter(flag==2))$dt


#create a separate dataframe
delta=0 #days after minimum to be shown
l41b <- l41%>%filter(
  case_when(
    dt<=min_v[1]+delta & dt>=max_v[1]  ~ TRUE,
    dt<=min_v[2]+delta & dt>=max_v[2]  ~ TRUE,
    dt<=min_v[3]+delta & dt>=max_v[3]  ~ TRUE,
    dt<=min_v[4]+delta & dt>=max_v[4]  ~ TRUE,
    dt<=min_v[5]+delta & dt>=max_v[5]  ~ TRUE,
    dt<=min_v[6]+delta & dt>=max_v[6]  ~ TRUE,
    dt<=min_v[7]+delta & dt>=max_v[7]  ~ TRUE,
    dt<=min_v[8]+delta & dt>=max_v[8]  ~ TRUE,
    dt<=min_v[9]+delta & dt>=max_v[9]  ~ TRUE,
    TRUE~F))


#work with days after maximum and 
#with ratios from that maximum
l41b <- l41b%>%group_by(periode)%>%mutate(days_after_max=row_number(),base=max((days_after_max==1)*price))
l41b <- l41b%>%group_by(periode)%>%mutate(i=(price/base-1))
l41b <- l41b%>%group_by(periode)%>%mutate(min_i=(min/base-1))
#hand made naming periodes
l41b$t <- with(l41b,
               case_when(
                 periode==1~'a',
                 periode==2~'b',
                 periode==3~'c',
                 periode==4~'d',
                 periode==5~'e',
                 periode==6~'f',
                 periode==7~'h',
                 periode==8~'i',
                 periode==9~'j',
               ))
l41b$point <- with(l41b,(dt%in%max_v)*days_after_max)
l41b$now <- with(l41b,(dt==max(btc$dt))*days_after_max)

ggplot(l41b)+
  geom_line(aes(days_after_max,i,color=factor(t)),linewidth=1)+
  labs(x='days since maximum',y='% from maximum',title='Bitcoin winter, from maximum to minimum',
       color='Crypto Winters')+
  theme(legend.position = c(0.8, 0.8))+
  geom_point(aes(days_after_max,min_i),size=3,shape=25,fill=color2)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(0,100,by=10))+
  theme(text = element_text(size = 18))

# -3 days to -10 days of each pick
max_v <- (l41%>%filter(flag==1))$dt
de=10
d=2
l41$deltas <- with(l41,
                   case_when(
                     dt<=max_v[1]+de & dt>=max_v[1]-d  ~ 1,
                     dt<=max_v[2]+de & dt>=max_v[2]-d  ~ 2,
                     dt<=max_v[3]+de & dt>=max_v[3]-d  ~ 3,
                     dt<=max_v[4]+de & dt>=max_v[4]-d  ~ 4,
                     dt<=max_v[5]+de & dt>=max_v[5]-d  ~ 5,
                     dt<=max_v[6]+de & dt>=max_v[6]-d  ~ 6,
                     TRUE~-1))

ggplot(l41%>%filter(deltas==1))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)

ggplot(l41%>%filter(deltas==2))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)

ggplot(l41%>%filter(deltas==3))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)

ggplot(l41%>%filter(deltas==4))+
  geom_line(aes(dt,price,color=factor(periode)),size=1)+
  geom_point(aes(dt,max),size=4,shape=24,fill=color1)+
  geom_point(aes(dt,min),size=4,shape=25,fill=color2)
