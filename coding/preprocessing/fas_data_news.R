library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(reshape2)
library(data.table)

ai_95 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_95.xlsx")
ai_92_93 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_92_93.xlsx")
ai_76_80 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_76_80.xlsx")

news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_filtered.csv")

fas <- rbind(ai_95, ai_92_93)
fas <- rbind(fas, ai_76_80)

fas$Дата <- as.Date(fas$Дата, format="%m/%d/%Y")
news$article_date <- as.Date(news$article_date, format ="%d.%m.%Y")
fas$year <- year(fas$Дата)
news$year <- year(news$article_date)
fas$week <- week(fas$Дата)
news$week <- week(news$article_date)

fas$year_week <- paste(fas$year, fas$week) 
news$year_week <- paste(news$year, news$week)

#unique(fas$Винк)
#unique(news$company)

news$company <- str_replace(news$company, "rosneft", "ОАО 'НК 'Роснефть'")
news$company <- str_replace(news$company, "bashneft-ank-ao", "ОАО АНК 'Башнефть'")
news$company <- str_replace(news$company, "gazprom-neft", "ОАО 'Газпром нефть'")
news$company <- str_replace(news$company, "lukoil", "ОАО 'Лукойл'")
news$company <- str_replace(news$company, "tatneft-3", "ОАО 'Татнефть'")

colnames(news)[6] <- 'Винк'
fas <- left_join(fas, news, by = c('year_week', 'Винк'))

fas$Cредняя_Розничная_цена <- as.numeric(fas$Cредняя_Розничная_цена)
fas$event_predupr <- as.numeric(as.logical(fas$event_predupr))
fas$event_predost <- as.numeric(as.logical(fas$event_predost))
fas$event_delo <- as.numeric(as.logical(fas$event_delo))
fas$event_resh <- as.numeric(as.logical(fas$event_resh))
fas$event_predp <- as.numeric(as.logical(fas$event_predp))

fas$event_predupr<-replace(fas$event_predupr, is.na(fas$event_predupr), 0)
fas$event_predost<-replace(fas$event_predost, is.na(fas$event_predost), 0)
fas$event_delo<-replace(fas$event_delo, is.na(fas$event_delo), 0)
fas$event_resh<-replace(fas$event_resh, is.na(fas$event_resh), 0)
fas$event_predp<-replace(fas$event_predp, is.na(fas$event_predp), 0)

fas <- fas[,-c(8,12,13,26,27)]
colnames(fas)[8] <- 'year'
colnames(fas)[9] <- 'week'

brent <- read_excel("~/Documents/master's_thesis/coding/data/brent/prepared_brent/brent.xlsx")
brent <- brent[,c(3,8)]
colnames(brent)[1] <- 'Дата'
brent$year <- year(brent$Дата)
brent$week <- week(brent$Дата)
brent$year_week <- paste(brent$year, brent$week) 
fas <- left_join(fas, brent, by = c('year_week'))
fas <- fas[,-c(25,26)]
colnames(fas)[23] <- 'date_brent'
colnames(fas)[24] <- 'Цена на нефть'

neer <- read_excel("~/Documents/master's_thesis/coding/data/rub_usd_exchange_rate/RC_F01_01_2012_T31_12_2019.xlsx")
neer <- neer[,c(2,3)]
colnames(neer)[1] <- 'Дата'
neer$year <- year(neer$Дата)
neer$week <- week(neer$Дата)
neer$year_week <- paste(neer$year, neer$week) 
fas <- left_join(fas, neer, by = c('year_week'))
fas <- fas[,-c(27,28)]
colnames(fas)[26] <- 'Курс доллара'
colnames(fas)[25] <- 'date_neer'

bulk <- read_excel("~/Documents/master's_thesis/coding/data/petrol/СВОД ПО ОПТОВЫМ ЦЕНАМ НПЗ ЗА 2018-2021 гг.xls")
bulk <- melt(setDT(bulk), id.vars = c("Год", "Бензин","ВИНК", "НПЗ", "Субъект РФ"), variable.name = "Месяц")
bulk <- bulk %>% group_by(`Год`, `Месяц`, `Субъект РФ`) %>% summarise(`Цена за 1 т бензина` = mean(value))

#bulk <- bulk[!is.na(bulk$`Цена за 1 т бензина`), ]       

bulk$Месяц <- str_replace(bulk$Месяц, "январь\n", "1")
bulk$Месяц <- str_replace(bulk$Месяц, "февраль\n", "2")
bulk$Месяц <- str_replace(bulk$Месяц, "март\n", "3")
bulk$Месяц <- str_replace(bulk$Месяц, "апрель\n", "4")
bulk$Месяц <- str_replace(bulk$Месяц, "май\n", "5")
bulk$Месяц <- str_replace(bulk$Месяц, "июнь\n", "6")
bulk$Месяц <- str_replace(bulk$Месяц, "июль\n", "7")
bulk$Месяц <- str_replace(bulk$Месяц, "август\n", "8")
bulk$Месяц <- str_replace(bulk$Месяц, "сентябрь\n", "9")
bulk$Месяц <- str_replace(bulk$Месяц, "октябрь\n", "10")
bulk$Месяц <- str_replace(bulk$Месяц, "ноябрь\n", "11")
bulk$Месяц <- str_replace(bulk$Месяц, "декабрь\n", "12")

# library(zoo)
# bulk$`Дата` <- as.yearmon(paste(bulk$Год, bulk$Месяц), "%Y %m")
# library(ggplot2)

# cairo_pdf(file = 'prices_cdu.pdf', width = 12, height = 5)
# ggplot(bulk) + geom_line(aes(x=Дата,y=`Цена за 1 т бензина`,color=`Субъект РФ`)) 
# dev.off()

colnames(fas)[6] <- 'Дата'
colnames(fas)[8] <- 'year'
colnames(fas)[9] <- 'week'

fas$month <- as.numeric(format(as.Date(fas$Дата), "%m"))

bulk$month <- as.numeric(bulk$Месяц)
colnames(bulk)[1] <- 'year'
colnames(bulk)[3] <- 'Регион'

fas <- left_join(fas, bulk, by = c('year', 'month', 'Регион'))

#cdu$`Цена за 1 т бензина` <- cdu$`Цена за 1 т бензина`/1000

fas <- fas[,-28]

fas <- fas %>% group_by(`year`, `Регион`, `Винк`, `Тип топлива`) %>% arrange(week) %>% mutate(event_predupr_lag1 = lead(event_predupr,1), 
                                                                                  event_predupr_lag2 = lead(event_predupr,2),
                                                                                  event_predupr_lag3 = lead(event_predupr,3),
                                                                                  event_predupr_lag4 = lead(event_predupr,4),
                                                                                  event_predupr_lag5 = lead(event_predupr,5),
                                                                                  event_predupr_lag6 = lead(event_predupr,6),
                                                                                  event_predupr_lag7 = lead(event_predupr,7),
                                                                                  
                                                                                  event_predupr_lead1 = lag(event_predupr,1), 
                                                                                  event_predupr_lead2 = lag(event_predupr,2),
                                                                                  event_predupr_lead3 = lag(event_predupr,3),
                                                                                  event_predupr_lead4 = lag(event_predupr,4),
                                                                                  event_predupr_lead5 = lag(event_predupr,5),
                                                                                  event_predupr_lead6 = lag(event_predupr,6),
                                                                                  event_predupr_lead7 = lag(event_predupr,7),
                                                                                  #
                                                                                  
                                                                                  event_predost_lag1 = lead(event_predost,1), 
                                                                                  event_predost_lag2 = lead(event_predost,2),
                                                                                  event_predost_lag3 = lead(event_predost,3),
                                                                                  event_predost_lag4 = lead(event_predost,4),
                                                                                  event_predost_lag5 = lead(event_predost,5),
                                                                                  event_predost_lag6 = lead(event_predost,6),
                                                                                  event_predost_lag7 = lead(event_predost,7),
                                                                                  
                                                                                  event_predost_lead1 = lag(event_predost,1), 
                                                                                  event_predost_lead2 = lag(event_predost,2),
                                                                                  event_predost_lead3 = lag(event_predost,3),
                                                                                  event_predost_lead4 = lag(event_predost,4),
                                                                                  event_predost_lead5 = lag(event_predost,5),
                                                                                  event_predost_lead6 = lag(event_predost,6),
                                                                                  event_predost_lead7 = lag(event_predost,7),
                                                                                  #
                                                                                  
                                                                                  event_delo_lag1 = lead(event_delo,1), 
                                                                                  event_delo_lag2 = lead(event_delo,2),
                                                                                  event_delo_lag3 = lead(event_delo,3),
                                                                                  event_delo_lag4 = lead(event_delo,4),
                                                                                  event_delo_lag5 = lead(event_delo,5),
                                                                                  event_delo_lag6 = lead(event_delo,6),
                                                                                  event_delo_lag7 = lead(event_delo,7),
                                                                                  
                                                                                  event_delo_lead1 = lag(event_delo,1), 
                                                                                  event_delo_lead2 = lag(event_delo,2),
                                                                                  event_delo_lead3 = lag(event_delo,3),
                                                                                  event_delo_lead4 = lag(event_delo,4),
                                                                                  event_delo_lead5 = lag(event_delo,5),
                                                                                  event_delo_lead6 = lag(event_delo,6),
                                                                                  event_delo_lead7 = lag(event_delo,7),
                                                                                  #
                                                                                  
                                                                                  event_resh_lag1 = lead(event_resh,1), 
                                                                                  event_resh_lag2 = lead(event_resh,2),
                                                                                  event_resh_lag3 = lead(event_resh,3),
                                                                                  event_resh_lag4 = lead(event_resh,4),
                                                                                  event_resh_lag5 = lead(event_resh,5),
                                                                                  event_resh_lag6 = lead(event_resh,6),
                                                                                  event_resh_lag7 = lead(event_resh,7),
                                                                                  
                                                                                  event_resh_lead1 = lag(event_resh,1), 
                                                                                  event_resh_lead2 = lag(event_resh,2),
                                                                                  event_resh_lead3 = lag(event_resh,3),
                                                                                  event_resh_lead4 = lag(event_resh,4),
                                                                                  event_resh_lead5 = lag(event_resh,5),
                                                                                  event_resh_lead6 = lag(event_resh,6),
                                                                                  event_resh_lead7 = lag(event_resh,7),
                                                                                  #
                                                                                  
                                                                                  event_predp_lag1 = lead(event_predp,1), 
                                                                                  event_predp_lag2 = lead(event_predp,2),
                                                                                  event_predp_lag3 = lead(event_predp,3),
                                                                                  event_predp_lag4 = lead(event_predp,4),
                                                                                  event_predp_lag5 = lead(event_predp,5),
                                                                                  event_predp_lag6 = lead(event_predp,6),
                                                                                  event_predp_lag7 = lead(event_predp,7),
                                                                                  
                                                                                  event_predp_lead1 = lag(event_predp,1), 
                                                                                  event_predp_lead2 = lag(event_predp,2),
                                                                                  event_predp_lead3 = lag(event_predp,3),
                                                                                  event_predp_lead4 = lag(event_predp,4),
                                                                                  event_predp_lead5 = lag(event_predp,5),
                                                                                  event_predp_lead6 = lag(event_predp,6),
                                                                                  event_predp_lead7 = lag(event_predp,7))

summary(fas)

write.csv(fas,"fas.csv")
