w <- btc_ddbb%>%filter(dt >= '2020-11-01', dt<='2021-01-15')
w <- btc_ddbb%>%filter(dt >= '2024-11-01', dt<='2025-01-15')
w <- btc_ddbb%>%filter(dt >= '2020-06-01', dt<='2020-10-25')

w <- btc_ddbb%>%filter(dt >= '2021-04-01', dt<='2021-07-01')
w <- btc_ddbb%>%filter(dt >= '2021-01-01', dt<='2021-07-01')
w <- w%>%select(dt,high,low,open,close,time_high, time_low)

ggplot(w)+
  geom_line(aes(dt,close))


w <- w %>%
  arrange(dt) %>%                                 # Ensure data is sorted by date
  mutate(
    maxp1 = cummax(high),                   # Running maximum
    is_new_max = (high == maxp1),           # Flag if this is a new maximum
    max_group = cumsum(is_new_max)                  # Create groups based on new max
  ) %>%
  group_by(max_group) %>%                           # Group by max occurrences
  mutate(
    minp1 = cummin(low)
    ) %>% 
  ungroup() %>%                                     # Ungroup for clean output
  select(-is_new_max, -max_group)   

w%>%arrange(low)
w%>%arrange(minp2)

p <- ggplot(w)+
  geom_line(aes(dt,open))+
  geom_line(aes(dt,maxp1),color='green')+
  # geom_line(aes(dt,maxp2),color='blue')+
  geom_line(aes(dt,minp1),color='red')+
  # geom_line(aes(dt,minp2),color='grey')+
  theme_bw()+
  theme(legend.position = 'none')+
  scale_y_log10()
p

w <- w %>%
  arrange(dt) %>%                                 # Ensure data is sorted by date
  mutate(
    change = minp1<lag(minp1,n=1,default = 0),                   # Running maximum
    min_group = cumsum(change)                  # Create groups based on new max
  ) %>%
  group_by(min_group) %>%                           # Group by max occurrences
  mutate(
    maxp2 = cummax(high)
  ) %>% 
  ungroup() %>%                                     # Ungroup for clean output
  select(-min_group)  
p <- ggplot(w)+
  geom_line(aes(dt,open))+
  geom_line(aes(dt,maxp1),color='green')+
  geom_line(aes(dt,maxp2),color='blue')+
  geom_line(aes(dt,minp1),color='red')+
  geom_point(aes(dt,change*minp1))+
  # geom_line(aes(dt,minp2),color='grey')+
  theme_bw()+
  theme(legend.position = 'none')+
  scale_y_log10()
p

p <- ggplot(w)+
  geom_line(aes(dt,open))+
  geom_line(aes(dt,high),color='darkgrey',linetype='dashed')+
  geom_line(aes(dt,low),color='darkgrey',linetype='dashed')+
  geom_line(aes(dt,maxp),color='green')+
  geom_line(aes(dt,minp),color='red')+
  geom_point(aes(dt,open,color=factor(oh),group='1'))+
  theme_bw()+
  theme(legend.position = 'none')+
  scale_y_log10()
p
w <- w%>%arrange(dt)%>%mutate(fall=low/maxp-1)
w <- w%>%arrange(dt)%>%mutate(ol=low/open-1)
w <- w%>%arrange(dt)%>%mutate(lh=high/low-1)
p
q <- ggplot(w)+geom_line(aes(dt,fall))

library(gridExtra)  

grid.arrange(p, q, ncol = 1)

ggplot(w)+
  geom_line(aes(dt,ol))+
  geom_line(aes(dt,lh,color='h'))

ggplot(w)+geom_density(aes(ol))+geom_density(aes(lh),color='red')
