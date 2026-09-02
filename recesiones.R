library(tidyverse)
library(fredr)
library(tidyquant)
library(RcppRoll)
library(zoo)
library(lubridate)

ten_years_ago <- today() - years(70)


FRED_API_KEY="f833b8856f2712f70499e0eaf5a81303"
fredr_set_key(FRED_API_KEY)


#leer recesiones
recesion <- fredr(
  series_id = "USREC",
  observation_start = ten_years_ago,
  observation_end = as.Date(today())
)

#fecha de inicio y final de las recesiones
recessions <- recession_data %>%
  mutate(is_recession = value == 1,
         prev = lag(is_recession, default = FALSE),
         start = is_recession & !prev,
         end = !is_recession & prev) %>%
  mutate(start_date = ifelse(start, as.character(date), NA),
         end_date = ifelse(end, as.character(date), NA)) %>%
  select(date, start_date, end_date) %>%
  filter(!is.na(start_date) | !is.na(end_date))

#check rapido que todo es ok
ggplot(recesion)+geom_line(aes(date,value))

#10 años interest rate
rate10 <- fredr(
  series_id = "GS10",
  observation_start = ten_years_ago,
  observation_end = as.Date(today())
)

#check rapido que todo es ok
ggplot(rate10)+geom_line(aes(date,value))

#1 año interest rate
rate1 <- fredr(
  series_id = "GS1",
  observation_start = ten_years_ago,
  observation_end = as.Date(today())
)

#check rapido que todo es ok
ggplot(rate1)+geom_line(aes(date,value))

#3 meses interest rate
rate3m <- fredr(
  series_id = "TB3MS",
  observation_start = ten_years_ago,
  observation_end = as.Date(today())
)

#check rapido que todo es ok
ggplot(rate3m)+geom_line(aes(date,value))


sentiment <- fredr(series_id = "UMCSENT", 
                   observation_start = ten_years_ago,
                   observation_end = as.Date(today())
                   )


#sp500
sp500 <- tq_get("^GSPC",get='stock.prices',from=ten_years_ago)

#datos del paro
unrate <- fredr(series_id = "UNRATE", observation_start = ten_years_ago)

#leading economic index
lei <- fredr(series_id = "USSLIND", 
             observation_start = ten_years_ago,
             observation_end = as.Date(today())
             )
wei <- fredr(
  series_id   = "WEI",
  observation_start = ten_years_ago,
  observation_end = as.Date(today())
)

#housing
housing_starts <- fredr(series_id = "HOUST", 
                        observation_start = ten_years_ago,
                        observation_end = as.Date(today())
)

#industrial production
ind_production <- fredr(series_id = "INDPRO",
                        observation_start = ten_years_ago,
                        observation_end = as.Date(today())
)

#cambios de nombres
recesion <- recesion[c('date','value')]
colnames(recesion) <- c('dt','recesion')
rate10 <- rate10[c('date','value')]
colnames(rate10) <- c('dt','rate10')
rate1 <- rate1[c('date','value')]
colnames(rate1) <- c('dt','rate1')
rate3m <- rate3m[c('date','value')]
colnames(rate3m) <- c('dt','rate3m')
sp500 <- sp500[,c('date','adjusted')]
colnames(sp500) <- c('dt','sp500')
unrate <- unrate[,c('date','value')]
colnames(unrate) <- c('dt','unrate')
lei <- lei[,c('date','value')]
colnames(lei) <- c('dt','lei')
wei <- wei[,c('date','value')]
colnames(wei) <- c('dt','wei')
sentiment <- sentiment[,c('date','value')]
colnames(sentiment) <- c('dt','sentiment')
housing_starts <- housing_starts[,c('date','value')]
colnames(housing_starts) <- c('dt','housing_starts')
ind_production <- ind_production[,c('date','value')]
colnames(ind_production) <- c('dt','ind_production')


wei <- wei%>%mutate(dt=floor_date(dt, "month"))%>%
  group_by(dt)%>%summarise(wei=mean(wei))

#todo en un data frame
df <- rate10%>%left_join(rate1)%>%left_join(rate3m)%>%left_join(recesion)%>%left_join(sp500)%>%
  left_join(unrate)%>%
  left_join(lei)%>%
  left_join(sentiment)%>%
  left_join(housing_starts)%>%
  left_join(ind_production)%>%
  left_join(wei)

#diferencia entre 10 años y 1año/3meses
df$diff3m <- with(df,rate10-rate3m)
df$diff1 <- with(df,rate10-rate1)

#llenar "aujeros", los dias de fiesta de sp500
df$sp500aux <- df$sp500
df$sp500aux<- na.locf(df$sp500aux, na.rm = FALSE)

df <- df%>%
  mutate(
    aux=abs(recesion-lag(recesion,default = 0)),
    aux=cumsum(aux),
    aux=ifelse(recesion==0,NA,aux)
    )
###---- crear bull and bear market:
df <- df%>%mutate(down=(sp500-lag(sp500aux))>0)
df <- df%>%mutate(min12=roll_minr(unrate,12,na.rm=T),Sahm=(unrate-min12)/min12)


#indicador 1:
# Yield curve
ggplot(df)+geom_line(aes(dt,diff1,group='1',color=diff1<0),size=1)+
  geom_line(aes(dt,recesion))+
  theme_bw()+theme(text = element_text(size = 16),legend.position = 'none')+
  labs(y='yield curve diff')


#lei
ggplot(df%>%filter(dt>as.Date('1980-01-01')))+
  geom_line(aes(dt,wei,group='1',color='wei'),size=1)+
  geom_line(aes(dt,lei,group='1',color='lei'),size=1)+
  geom_line(aes(dt,recesion))+
  theme_bw()+theme(text = element_text(size = 16),legend.position = 'top')+
  labs(y='yield curve diff')

#sentiment
ggplot(df%>%filter(dt>as.Date('1975-01-01')))+
  geom_line(aes(dt,sentiment,group='1'),color=color1,size=1)+
  geom_line(aes(dt,recesion*100))+
  theme_bw()+theme(text = element_text(size = 16),legend.position = 'top')+
  labs(y='yield curve diff')


#housing
ggplot(df)+
  geom_line(aes(dt,housing_starts,group='1'),color=color1,size=1)+
  geom_line(aes(dt,recesion*2500))+
  theme_bw()+theme(text = element_text(size = 16),legend.position = 'top')+
  labs(y='yield curve diff')

#industril
ggplot(df)+
  geom_line(aes(dt,ind_production,group='1'),color=color1,size=1)+
  geom_line(aes(dt,recesion*150))+
  theme_bw()+theme(text = element_text(size = 16),legend.position = 'top')+
  labs(y='yield curve diff')


ggplot(df)+
  geom_line(aes(dt,Sahm,group='1',color=Sahm>0.05))+
  geom_line(aes(dt,recesion))


df <- df %>%
  arrange(df) %>%
  mutate(
    ma_short = roll_mean(sp500aux, n = 2, fill = NA, align = "right"),
    ma_long  = roll_mean(sp500aux, n = 12, fill = NA, align = "right"),
    state = case_when(
      ma_short > ma_long ~ "bull",
      ma_short < ma_long ~ "bear",
      TRUE ~ NA_character_
    )
  )

ggplot(df)+geom_line(aes(dt,sp500))+geom_line(aes(dt,recesion*4000))+scale_y_log10()

ggplot(df)+
  geom_line(aes(dt,sp500aux,group='1',color=factor(state)),size=1)+
  geom_line(aes(dt,recesion*4000))+
  geom_line(aes(dt,10,group='1',color=factor(state)),size=3)+
  scale_y_log10()

df%>%group_by(aux)%>%summarise(min(dt),max(dt))


