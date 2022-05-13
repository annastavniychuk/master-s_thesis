# Пакеты
{
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
}

# Импорт данных
{
# ЦДУ
cdu <- read_excel("~/Documents/master's_thesis/coding/data/petrol/cdu/AZS_2012-2019.xlsx")
# ФАС
ai_95 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_95.xlsx")
ai_92_93 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_92_93.xlsx")
ai_76_80 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_76_80.xlsx")
# События
news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_filtered.csv")
}


# Обработка данных ФАС
{
# склеиваем отдельные датасеты с ценами
{
fas <- rbind(ai_95, ai_92_93)
fas <- rbind(fas, ai_76_80)
}

# правим дату в нужный формат
{
fas$Дата <- as.POSIXlt(fas$Дата, format="%m/%d/%Y")
news$article_date <- as.Date(news$article_date, format ="%d.%m.%Y")
}
  
#unique(fas[c("Винк")])
#unique(news[c("company")])

# приводим название компаний в одинаковый вид
{
news$company <- str_replace(news$company, "rosneft", "ОАО 'НК 'Роснефть'")
news$company <- str_replace(news$company, "bashneft-ank-ao", "ОАО АНК 'Башнефть'")
news$company <- str_replace(news$company, "gazprom-neft", "ОАО 'Газпром нефть'")
news$company <- str_replace(news$company, "lukoil", "ОАО 'Лукойл'")
news$company <- str_replace(news$company, "tatneft-3", "ОАО 'Татнефть'")
}
  
colnames(news)[8] <- 'Дата'

# добавляем к ценам данные о новостях
fas <- left_join(fas, news, by = c('Дата'))


# fas$treatment <- as.factor(ifelse(fas$event_predupr == TRUE, '1',
#                             ifelse(fas$event_predost == TRUE, '2', 
#                             ifelse(fas$event_delo == TRUE, '3', 
#                             ifelse(fas$event_resh == TRUE, '4',
#                             ifelse(fas$event_predp == TRUE, '5', NA))))))

# контроль или тритмент группа 
{
fas$treatment <- fas$Винк==fas$company
fas$treatment<-replace(fas$treatment, is.na(fas$treatment), 0)
}

# Подчищаем технические хвосты -- форматы данных и тд
{
fas <- fas[,-c(1,8,9,10,11,12,17)]
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
}
}


# Обработка данных ЦДУ


cdu <- melt(setDT(cdu), id.vars = c("Дата","Регион", "Компания"), variable.name = "Марка")
colnames(cdu)[5] <- 'Цена'

# правим дату в нужный формат
news$article_date <- as.Date(news$article_date, format ="%d.%m.%Y")


cdu <- as.data.frame(cdu)
unique(cdu[c("Компания")])
unique(news[c("company")])

# приводим название компаний в одинаковый вид
{
  news$company <- str_replace(news$company, "rosneft", "РОСНЕФТЬ")
  news$company <- str_replace(news$company, "bashneft-ank-ao", "БАШНЕФТЬ")
  news$company <- str_replace(news$company, "gazprom-neft", "ГАЗПРОМ НЕФТЬ")
  news$company <- str_replace(news$company, "lukoil", "ЛУКОЙЛ")
  news$company <- str_replace(news$company, "tatneft-3", "ТАТНЕФТЬ")
}

colnames(news)[8] <- 'Дата'
colnames(news)[6] <- 'Компания'

# добавляем к ценам данные о новостях
cdu <- left_join(cdu, news, by = c('Дата', 'Компания'))

summary(cdu)

# Подчищаем технические хвосты -- форматы данных и тд

cdu <- cdu[,-c(6,7,8,13)]

cdu$event_predupr <- as.logical(cdu$event_predupr)
cdu$event_predost <- as.logical(cdu$event_predost)
cdu$event_delo <- as.logical(cdu$event_delo)
cdu$event_resh <- as.logical(cdu$event_resh)
cdu$event_predp <- as.logical(cdu$event_predp)

cdu$event_predupr<-replace(cdu$event_predupr, is.na(cdu$event_predupr), 0)
cdu$event_predost<-replace(cdu$event_predost, is.na(cdu$event_predost), 0)
cdu$event_delo<-replace(cdu$event_delo, is.na(cdu$event_delo), 0)
cdu$event_resh<-replace(cdu$event_resh, is.na(cdu$event_resh), 0)
cdu$event_predp<-replace(cdu$event_predp, is.na(cdu$event_predp), 0)

# cdu$event_predupr <- as.logical(cdu$event_predupr)
# cdu$event_predost <- as.logical(cdu$event_predost)
# cdu$event_delo <- as.logical(cdu$event_delo)
# cdu$event_resh <- as.logical(cdu$event_resh)
# cdu$event_predp <- as.logical(cdu$event_predp)

cdu$event_predupr <- as.factor(cdu$event_predupr)
cdu$event_predost <- as.factor(cdu$event_predost)
cdu$event_delo <- as.factor(cdu$event_delo)
cdu$event_resh <- as.factor(cdu$event_resh)
cdu$event_predp <- as.factor(cdu$event_predp)

summary(cdu)

brent <- read_excel("~/Documents/master's_thesis/coding/data/brent/prepared_brent/brent.xlsx")
brent <- brent[,c(3,8)]
colnames(brent)[1] <- 'Дата'
cdu <- left_join(cdu, brent, by = c('Дата'))
colnames(cdu)[15] <- 'Цена на нефть'

neer <- read_excel("~/Documents/master's_thesis/coding/data/rub_usd_exchange_rate/RC_F01_01_2012_T31_12_2019.xlsx")
neer <- neer[,c(2,3)]
colnames(neer)[1] <- 'Дата'
cdu <- left_join(cdu, neer, by = c('Дата'))
colnames(cdu)[16] <- 'Курс доллара'


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
library(zoo)
bulk$`Дата` <- as.yearmon(paste(bulk$Год, bulk$Месяц), "%Y %m")
library(ggplot2)

cairo_pdf(file = 'prices_cdu.pdf', width = 12, height = 5)
ggplot(bulk) + geom_line(aes(x=Дата,y=`Цена за 1 т бензина`,color=`Субъект РФ`)) 
dev.off()

cdu$Месяц <- as.numeric(format(as.Date(cdu$Дата), "%m"))
cdu$Год <- as.numeric(format(as.Date(cdu$Дата), "%Y"))
bulk$Месяц <- as.numeric(bulk$Месяц)
cdu <- left_join(cdu, bulk, by = c('Год', 'Месяц'))
cdu$`Цена за 1 т бензина` <- cdu$`Цена за 1 т бензина`/1000
cdu <- cdu[,-c(19,21)]
colnames(cdu)[1] <- 'Дата'

#cdu$Месяц <- as.yearmon(cdu$Месяц, "%m")
summary(cdu)

cdu <- cdu %>% group_by(`Год`, `Месяц`, `Регион`, `Компания`, `Марка`) %>% mutate(event_predupr_lag1 = lead(event_predupr,1), 
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

summary(cdu)


cdu$Год <- as.factor(cdu$Год)
cdu$Месяц <- as.factor(cdu$Месяц)
cdu$Регион <- as.factor(cdu$Регион)
cdu$Компания <- as.factor(cdu$Компания)
  

# write.csv(cdu,"cdu.csv")
# mod1 <- lm(Цена ~ Регион + Компания + Марка + `Цена на нефть` + Месяц + Год  +
#                  event_predupr +
#                  event_predost + 
#                  event_delo + 
#                  event_resh + 
#                  event_predp, data = cdu)


cdu$`Цена на нефть` <- cdu$`Цена на нефть`*cdu$`Курс доллара`


cdu <- cdu %>% filter(Марка == 'АИ95')

mod1 <- lm(Цена ~ Регион + Компания + `Цена на нефть` + Месяц + Дата +
                 event_predupr_lag7 + event_predupr_lag6 + event_predupr_lag5 + event_predupr_lag4 + event_predupr_lag3 + event_predupr_lag2 + event_predupr_lag1 + event_predupr + event_predupr_lead1 + event_predupr_lead2 + event_predupr_lead3 + event_predupr_lead4 + event_predupr_lead5 + event_predupr_lead6 + event_predupr_lead7,
               data = cdu)
summary(mod1)

mod2 <- lm(Цена ~ Регион + Компания + `Цена на нефть` + Месяц + Дата  +
                 event_predost_lag7 + event_predost_lag6 + event_predost_lag5 + event_predost_lag4 + event_predost_lag3 + event_predost_lag2 + event_predost_lag1 + event_predost + event_predost_lead1 + event_predost_lead2 + event_predost_lead3 + event_predost_lead4 + event_predost_lead5 + event_predost_lead6 + event_predost_lead7,
               data = cdu)
summary(mod2)

mod3 <- lm(Цена ~ Регион + Компания + `Цена на нефть` + Месяц + Дата  +
                 event_delo_lag7 + event_delo_lag6 + event_delo_lag5 + event_delo_lag4 + event_delo_lag3 + event_delo_lag2 + event_delo_lag1 + event_delo + event_delo_lead1 + event_delo_lead2 + event_delo_lead3 + event_delo_lead4 + event_delo_lead5 + event_delo_lead6 + event_delo_lead7,
               data = cdu)
summary(mod3)

mod4 <- lm(Цена ~ Регион + Компания + `Цена на нефть` + Месяц + Дата  +
                 event_resh_lag7 + event_resh_lag6 + event_resh_lag5 + event_resh_lag4 + event_resh_lag3 + event_resh_lag2 + event_resh_lag1 + event_resh + event_resh_lead1 + event_resh_lead2 + event_resh_lead3 + event_resh_lead4 + event_resh_lead5 + event_resh_lead6 + event_resh_lead7,
               data = cdu)
summary(mod4)

mod5 <- lm(Цена ~ Регион + Компания + `Цена на нефть` + Месяц + Дата +
                 event_predp_lag7 + event_predp_lag6 + event_predp_lag5 + event_predp_lag4 + event_predp_lag3 + event_predp_lag2 + event_predp_lag1 + event_predp + event_predp_lead1 + event_predp_lead2 + event_predp_lead3 + event_predp_lead4 + event_predp_lead5 + event_predp_lead6 + event_predp_lead7,
               data = cdu)
summary(mod5)

mod6 <- lm(Цена ~ Регион + Компания + `Цена на нефть` + Месяц + Дата +
                 event_predupr_lag7 + event_predupr_lag6 + event_predupr_lag5 + event_predupr_lag4 + event_predupr_lag3 + event_predupr_lag2 + event_predupr_lag1 + event_predupr + event_predupr_lead1 + event_predupr_lead2 + event_predupr_lead3 + event_predupr_lead4 + event_predupr_lead5 + event_predupr_lead6 + event_predupr_lead7 +
                 event_resh_lag7 + event_resh_lag6 + event_resh_lag5 + event_resh_lag4 + event_resh_lag3 + event_resh_lag2 + event_resh_lag1 + event_resh + event_resh_lead1 + event_resh_lead2 + event_resh_lead3 + event_resh_lead4 + event_resh_lead5 + event_resh_lead6 + event_resh_lead7 +
                 event_predp_lag7 + event_predp_lag6 + event_predp_lag5 + event_predp_lag4 + event_predp_lag3 + event_predp_lag2 + event_predp_lag1 + event_predp + event_predp_lead1 + event_predp_lead2 + event_predp_lead3 + event_predp_lead4 + event_predp_lead5 + event_predp_lead6 + event_predp_lead7,
               data = cdu)
summary(mod6)


#mod1 <- lm(Цена ~ Регион + Компания + Марка + `Цена на нефть` + Месяц + Год + `Цена за 1 т бензина` + event_predupr + event_resh + event_predp, data = cdu)
summary(mod1)




