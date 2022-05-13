# Пакеты
{
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(zoo)
library(fixest)
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
news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_upd.csv", header=T)
news <- news %>% filter(FAS == 1)
news <- news %>% filter(event_predupr == 1 | event_predost == 1 | event_delo == 1 | event_resh_predp == 1)

news$date <- as.Date(news$article_date, format="%d.%m.%Y")
}

# Обработка данных ФАС
{
# склеиваем отдельные датасеты с ценами
fas <- rbind(ai_95, ai_92_93)
fas <- rbind(fas, ai_76_80)

# правим дату в нужный формат
fas$Дата <- as.Date(fas$Дата, format="%m/%d/%Y")
  
# unique(fas[c("Винк")])
# unique(news[c("company")])

# приводим название компаний в одинаковый вид
news$company <- str_replace(news$company, "rosneft", "ОАО 'НК 'Роснефть'")
news$company <- str_replace(news$company, "bashneft-ank-ao", "ОАО АНК 'Башнефть'")
news$company <- str_replace(news$company, "gazprom-neft", "ОАО 'Газпром нефть'")
news$company <- str_replace(news$company, "lukoil", "ОАО 'Лукойл'")
news$company <- str_replace(news$company, "tatneft-3", "ОАО 'Татнефть'")

colnames(news)[15] <- 'Дата'
news <- news[, c(4,8,9,10,11,14,15)]
news$week <- strftime(news$Дата, format = "%V")
news$year <- format(news$Дата, "%Y")
colnames(news)[1] <- "Компания"

fas$week <- strftime(fas$Дата, format = "%V")
fas$year <- format(fas$Дата, "%Y")
colnames(fas)[2] <- "Компания"

# добавляем к ценам данные о новостях
fas <- left_join(fas, news, by = c('year', 'week', 'Компания'))
fas <- fas[,-c(8,16)]
colnames(fas)[6] <- 'Дата'

# Подчищаем технические хвосты -- форматы данных и тд
fas$Cредняя_Розничная_цена <- as.numeric(fas$Cредняя_Розничная_цена)

fas$event_predupr <- as.numeric(as.logical(fas$event_predupr))
fas$event_predost <- as.numeric(as.logical(fas$event_predost))
fas$event_delo <- as.numeric(as.logical(fas$event_delo))
fas$event_resh_predp <- as.numeric(as.logical(fas$event_resh_predp))

fas$event_predupr<-replace(fas$event_predupr, is.na(fas$event_predupr), 0)
fas$event_predost<-replace(fas$event_predost, is.na(fas$event_predost), 0)
fas$event_delo<-replace(fas$event_delo, is.na(fas$event_delo), 0)
fas$event_resh_predp<-replace(fas$event_resh_predp, is.na(fas$event_resh_predp), 0)

fas$event_predupr <- as.factor(fas$event_predupr)
fas$event_predost <- as.factor(fas$event_predost)
fas$event_delo <- as.factor(fas$event_delo)
fas$event_resh_predp <- as.factor(fas$event_resh_predp)
}

# Обработка данных ЦДУ
{
cdu <- melt(setDT(cdu), id.vars = c("Дата","Регион", "Компания"), variable.name = "Марка")
colnames(cdu)[5] <- 'Цена'
cdu <- as.data.frame(cdu)
cdu$Дата <- as.Date(cdu$Дата)

# приводим название компаний в одинаковый вид
# unique(cdu[c("Компания")])
# unique(news[c("Компания")])
colnames(news)[1] <- 'company'
news$company <- str_replace(news$company, "ОАО 'НК 'Роснефть'", "РОСНЕФТЬ")
news$company <- str_replace(news$company, "ОАО АНК 'Башнефть'", "БАШНЕФТЬ")
news$company <- str_replace(news$company, "ОАО 'Газпром нефть'", "ГАЗПРОМ НЕФТЬ")
news$company <- str_replace(news$company, "ОАО 'Лукойл'", "ЛУКОЙЛ")
news$company <- str_replace(news$company, "ОАО 'Татнефть'", "ТАТНЕФТЬ")

colnames(news)[7] <- 'Дата'
colnames(news)[1] <- 'Компания'

# добавляем к ценам данные о новостях
cdu <- left_join(cdu, news, by = c('Дата', 'Компания'))
cdu <- cdu[,-c(11,12)]

# Подчищаем технические хвосты -- форматы данных и тд
cdu$event_predupr <- as.logical(cdu$event_predupr)
cdu$event_predost <- as.logical(cdu$event_predost)
cdu$event_delo <- as.logical(cdu$event_delo)
cdu$event_resh_predp <- as.logical(cdu$event_resh_predp)

cdu$event_predupr<-replace(cdu$event_predupr, is.na(cdu$event_predupr), 0)
cdu$event_predost<-replace(cdu$event_predost, is.na(cdu$event_predost), 0)
cdu$event_delo<-replace(cdu$event_delo, is.na(cdu$event_delo), 0)
cdu$event_resh_predp<-replace(cdu$event_resh, is.na(cdu$event_resh_predp), 0)

cdu$event_predupr <- as.factor(cdu$event_predupr)
cdu$event_predost <- as.factor(cdu$event_predost)
cdu$event_delo <- as.factor(cdu$event_delo)
cdu$event_resh_predp <- as.factor(cdu$event_resh_predp)
}
  
# Контрольные переменные
{
brent <- read_excel("~/Documents/master's_thesis/coding/data/brent/prepared_brent/brent.xlsx")
brent <- brent[,c(3,8)]
colnames(brent)[1] <- 'Дата'
cdu <- left_join(cdu, brent, by = c('Дата'))
colnames(cdu)[11] <- 'Цена на нефть'

brent$week <- strftime(brent$Дата, format = "%V")
brent$year <- format(brent$Дата, "%Y")
fas <- left_join(fas, brent, by = c('year', 'week'))
colnames(fas)[16] <- 'Цена на нефть'
fas <- fas[,-15]

neer <- read_excel("~/Documents/master's_thesis/coding/data/rub_usd_exchange_rate/RC_F01_01_2012_T31_12_2019.xlsx")
neer <- neer[,c(2,3)]
colnames(neer)[1] <- 'Дата'
cdu <- left_join(cdu, neer, by = c('Дата'))
colnames(cdu)[12] <- 'Курс доллара'

neer$week <- strftime(neer$Дата, format = "%V")
neer$year <- format(neer$Дата, "%Y")
fas <- left_join(fas, neer, by = c('year', 'week'))
colnames(fas)[17] <- 'Курс доллара'
fas <- fas[,-16]

# compet <- fas %>% group_by(Дата.x, Регион, Винк) %>% summarise(Цена=mean(Cредняя_Розничная_цена)) %>% group_by(Дата.x, Регион) %>% summarise(compet = n())
# colnames(compet) <- 
# cdu <- left_join(cdu, compet,)

bulk <- read_excel("~/Documents/master's_thesis/coding/data/petrol/СВОД ПО ОПТОВЫМ ЦЕНАМ НПЗ ЗА 2018-2021 гг.xls")
bulk <- melt(setDT(bulk), id.vars = c("Год", "Бензин","ВИНК", "НПЗ", "Субъект РФ"), variable.name = "Месяц")
bulk <- bulk %>% group_by(`Год`, `Месяц`, `Субъект РФ`) %>% summarise(`Цена за 1 т бензина` = mean(value))

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
bulk$`Дата` <- as.yearmon(paste(bulk$Год, bulk$Месяц), "%Y %m")

# library(ggplot2)
# cairo_pdf(file = 'prices_cdu.pdf', width = 12, height = 5)
# ggplot(bulk) + geom_line(aes(x=Дата,y=`Цена за 1 т бензина`,color=`Субъект РФ`)) 
# dev.off()

cdu$Месяц <- as.numeric(format(as.Date(cdu$Дата), "%m"))
cdu$Год <- as.numeric(format(as.Date(cdu$Дата), "%Y"))
bulk$Месяц <- as.numeric(bulk$Месяц)
cdu <- left_join(cdu, bulk, by = c('Год', 'Месяц'))
cdu$`Цена за 1 т бензина` <- cdu$`Цена за 1 т бензина`/1000
cdu <- cdu[,-c(15,17)]
colnames(cdu)[1] <- 'Дата'

bulk$week <- strftime(bulk$Дата, format = "%V")
bulk$year <- format(bulk$Дата, "%Y")
fas <- left_join(fas, bulk, by = c('year', 'week'))
fas$`Цена за 1 т бензина` <- fas$`Цена за 1 т бензина`/1000
fas <- fas[,-c(21,5,8,9,19)]

colnames(fas)[2] <- 'Компания'
colnames(fas)[4] <- 'Марка'
}

# Лаги и лиды тритмента
{
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
                                                                                  
                                                                                  event_resh_predp_lag1 = lead(event_resh_predp,1), 
                                                                                  event_resh_predp_lag2 = lead(event_resh_predp,2),
                                                                                  event_resh_predp_lag3 = lead(event_resh_predp,3),
                                                                                  event_resh_predp_lag4 = lead(event_resh_predp,4),
                                                                                  event_resh_predp_lag5 = lead(event_resh_predp,5),
                                                                                  event_resh_predp_lag6 = lead(event_resh_predp,6),
                                                                                  event_resh_predp_lag7 = lead(event_resh_predp,7),
                                                                                  
                                                                                  event_resh_predp_lead1 = lag(event_resh_predp,1), 
                                                                                  event_resh_predp_lead2 = lag(event_resh_predp,2),
                                                                                  event_resh_predp_lead3 = lag(event_resh_predp,3),
                                                                                  event_resh_predp_lead4 = lag(event_resh_predp,4),
                                                                                  event_resh_predp_lead5 = lag(event_resh_predp,5),
                                                                                  event_resh_predp_lead6 = lag(event_resh_predp,6),
                                                                                  event_resh_predp_lead7 = lag(event_resh_predp,7))
# summary(cdu)

fas <- fas %>% group_by(`Год`, `Месяц`, `Регион`, `Компания`, `Марка`) %>% mutate(event_predupr_lag1 = lead(event_predupr,1), 
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
                                                                                  
                                                                                  event_resh_predp_lag1 = lead(event_resh_predp,1), 
                                                                                  event_resh_predp_lag2 = lead(event_resh_predp,2),
                                                                                  event_resh_predp_lag3 = lead(event_resh_predp,3),
                                                                                  event_resh_predp_lag4 = lead(event_resh_predp,4),
                                                                                  event_resh_predp_lag5 = lead(event_resh_predp,5),
                                                                                  event_resh_predp_lag6 = lead(event_resh_predp,6),
                                                                                  event_resh_predp_lag7 = lead(event_resh_predp,7),
                                                                                  
                                                                                  event_resh_predp_lead1 = lag(event_resh_predp,1), 
                                                                                  event_resh_predp_lead2 = lag(event_resh_predp,2),
                                                                                  event_resh_predp_lead3 = lag(event_resh_predp,3),
                                                                                  event_resh_predp_lead4 = lag(event_resh_predp,4),
                                                                                  event_resh_predp_lead5 = lag(event_resh_predp,5),
                                                                                  event_resh_predp_lead6 = lag(event_resh_predp,6),
                                                                                  event_resh_predp_lead7 = lag(event_resh_predp,7))
# summary(fas)
}

# Формат переменных для регрессии
{
cdu$Год <- as.factor(cdu$Год)
cdu$Месяц <- as.factor(cdu$Месяц)
cdu$Регион <- as.factor(cdu$Регион)
cdu$Компания <- as.factor(cdu$Компания)
cdu$`Цена на нефть` <- cdu$`Цена на нефть`*cdu$`Курс доллара`
cdu$Демпфер <- as.numeric(cdu$Дата > '2019-01-01')
cdu$nДемпфер <- 1 - cdu$Демпфер
cdu$Дата <- as.factor(cdu$Дата)

fas$Год <- as.factor(fas$Год)
fas$Месяц <- as.factor(fas$Месяц)
fas$Регион <- as.factor(fas$Регион)
fas$Компания <- as.factor(fas$Компания)
fas$`Цена на нефть` <- fas$`Цена на нефть`*fas$`Курс доллара`
colnames(fas)[5] <- 'Дата'
fas$Дата <- as.POSIXlt(fas$Дата, format = '%Y-%m-%d')
fas$Демпфер <- as.numeric(fas$Дата > '2019-01-01')
fas$nДемпфер <- 1 - fas$Демпфер
fas$Марка <- as.factor(fas$Марка)
fas$Дата <- as.factor(fas$Дата)
} 

# Темпы роста
{
cdu <- cdu %>% group_by(`Год`, `Месяц`, `Регион`, `Компания`, `Марка`) %>% mutate(lЦена = log(1+Цена/lag(Цена)))
cdu <- cdu %>% group_by(`Год`, `Месяц`, `Регион`, `Компания`, `Марка`) %>% mutate(`lЦена на нефть`  = log(1+`Цена на нефть`/lag(`Цена на нефть`)))
cdu <- cdu %>% group_by(`Год`, `Месяц`, `Регион`, `Компания`, `Марка`) %>% mutate(`lЦена за 1 т бензина`  = log(1+`Цена за 1 т бензина`/lag(`Цена за 1 т бензина`)))
}


# Предупреждения
{
model1_1 <- feols(lЦена ~ `lЦена на нефть` + `Марка` + `lЦена за 1 т бензина` + event_predupr_lag.[7:1] + event_predupr_lead.[1:7] | Регион + Компания + Дата, data = cdu, se = "twoway")
summary(model1_1)
etable(model1_1)
coefplot_model1_1 <- coefplot(model1_1, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'),
                                    #group = list(`До предупреждения` = 1:6, `После предупреждения` = 8:14),
                                    main = "Фиксированные эффекты события",
                                    zero.par = list(col = "red", lwd = 2)) 

model1_2 <- feols(lЦена ~  `lЦена на нефть` + Марка + event_predupr_lag.[7:1] + event_predupr_lead.[1:7] | Регион + Компания + Дата, data = cdu, se = "twoway")
summary(model1_2)
etable(model1_2)
coefplot_model1_2 <- coefplot(model1_2, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'),
                             #group = list(`До предупреждения` = 1:7, `После предупреждения` = 9:15), 
                             main = "Фиксированные эффекты события",
                             zero.par = list(col = "red", lwd = 2)) 

model1_3 <- feols(lЦена ~ `lЦена на нефть` + `lЦена за 1 т бензина` + Демпфер + I(Демпфер*`lЦена за 1 т бензина`) + `Марка` + event_predupr_lag.[7:1] + event_predupr_lead.[1:7] | Регион + Компания + Дата, data = cdu, se = "twoway")
summary(model1_3)
#etable(model1_3, tex = TRUE)
etable(model1_3)
coefplot_model1_3 <- coefplot(model1_3, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина', 'Демпфер'),
                                        #group = list(`До предупреждения` = 1:5, `После предупреждения` = 6:12),
                                        ylab = 'Темп роста розничной цены',
                                        main = "",
                              #horiz = 1,
                              pt.join = TRUE, 
                              ci.fill = TRUE, 
                              #lab.cex = 5,
                              #lab.fit = "tilted",
                              dict = c(event_predupr_lag71 = '-7', 
                                       event_predupr_lag61 = '-6', 
                                       event_predupr_lag51 = '-5',
                                       event_predupr_lag41 = '-4', 
                                       event_predupr_lag31 = '-3', 
                                       event_predupr_lag21 = '-2',
                                       event_predupr_lag11 = '-1', 
                                       event_predupr_lead11 = '1',
                                       event_predupr_lead21 = '2',
                                       event_predupr_lead31 = '3',
                                       event_predupr_lead41 = '4',
                                       event_predupr_lead51 = '5',
                                       event_predupr_lead61 = '6',
                                       event_predupr_lead71 = '7'),
                                        zero.par = list(col = "red", lwd = 2)) 
}

# Предостережения
{
model2_1 <- feols(lЦена ~  `lЦена на нефть` + Марка + `lЦена за 1 т бензина` + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Компания + Дата, data = cdu, se = "twoway")
summary(model2_1)
coefplot_model2_1 <- coefplot(model2_1, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'), 
                                    group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
                                    ylab = 'Цена бензина, руб./л',
                                    main = "Фиксированные эффекты события", 
                                    zero.par = list(col = "red", lwd = 2))

model2_2 <- feols(lЦена ~  `lЦена на нефть` + Марка + Демпфер + event_predost_lag.[7:1] + event_predost_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model2_2)
etable(model2_2)
coefplot_model2_2 <- coefplot(model2_2, drop = c('Демпфер','lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'), 
                             #group = list(`До предостережения` = 1:4, `После предостережения` = 5:7),
                             ylab = 'Темп роста розничной цены',
                             main = "", 
                             #horiz = 1,
                             pt.join = TRUE, 
                             ci.fill = TRUE, 
                             #lab.cex = 5,
                             #lab.fit = "tilted",
                             dict = c(event_predost_lag71 = '-7', 
                                      event_predost_lag61 = '-6', 
                                      event_predost_lag51 = '-5',
                                      event_predost_lag41 = '-4', 
                                      event_predost_lag31 = '-3', 
                                      event_predost_lag21 = '-2',
                                      event_predost_lag11 = '-1', 
                                      event_predost_lead11 = '1',
                                      event_predost_lead21 = '2',
                                      event_predost_lead31 = '3',
                                      event_predost_lead41 = '4',
                                      event_predost_lead51 = '5',
                                      event_predost_lead61 = '6',
                                      event_predost_lead71 = '7'),
                             zero.par = list(col = "red", lwd = 2))

model2_3 <- feols(lЦена ~ `lЦена на нефть` + `lЦена за 1 т бензина` + Демпфер + I(Демпфер*`lЦена за 1 т бензина`) + `Марка` + event_predost_lag.[7:1] + event_predost_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model2_3)
etable(model2_3)
coefplot_model2_3 <- coefplot(model2_3, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина', 'Демпфер'), 
                                        group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
                                        ylab = 'Цена бензина, руб./л',
                                        main = "Фиксированные эффекты события", 
                                        zero.par = list(col = "red", lwd = 2))
}

# Возбуждение дела
{
model3_1 <- feols(lЦена ~  `lЦена на нефть` + Марка + `lЦена за 1 т бензина` + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model3_1)
coefplot_delo_petrol <- coefplot(model3_1, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'), 
                                 group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
                                 ylab = 'Цена бензина, руб./л',
                                 main = "Фиксированные эффекты события", 
                                 zero.par = list(col = "red", lwd = 2))

model3_2 <- feols(lЦена ~  `lЦена на нефть` + Марка + Демпфер + event_delo_lag.[7:1] + event_delo_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model3_2)
etable(model3_2)
coefplot_model3_2 <- coefplot(model3_2, drop = c('Демпфер', 'lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'), 
                          #group = list(`До возбуждения дела` = 1:3, `После возбуждения дела` = 4:6),
                          ylab = 'Темп роста розничной цены',
                          main = "", 
                          #horiz = 1,
                          pt.join = TRUE, 
                          ci.fill = TRUE, 
                          #lab.cex = 5,
                          #lab.fit = "tilted",
                          dict = c(event_delo_lag71 = '-7', 
                                   event_delo_lag61 = '-6', 
                                   event_delo_lag51 = '-5',
                                   event_delo_lag41 = '-4', 
                                   event_delo_lag31 = '-3', 
                                   event_delo_lag21 = '-2',
                                   event_delo_lag11 = '-1', 
                                   event_delo_lead11 = '1',
                                   event_delo_lead21 = '2',
                                   event_delo_lead31 = '3',
                                   event_delo_lead41 = '4',
                                   event_delo_lead51 = '5',
                                   event_delo_lead61 = '6',
                                   event_delo_lead71 = '7'),
                          zero.par = list(col = "red", lwd = 2))

model3_3 <- feols(lЦена ~  `lЦена на нефть` + `lЦена за 1 т бензина` + Демпфер + Демпфер*`lЦена на нефть` + Демпфер*`lЦена за 1 т бензина` + Марка + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model3_3)
coefplot_delo_petrol_str <- coefplot(model3_3, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина', 'Демпфер'), 
                                     group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
                                     ylab = 'Цена бензина, руб./л',
                                     main = "Фиксированные эффекты события", 
                                     zero.par = list(col = "red", lwd = 2))
}

# Решение и предписание
{
model4_1 <- feols(lЦена ~  `lЦена на нефть` + Марка + `lЦена за 1 т бензина` + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model4_1)
coefplot_model4_1 <- coefplot(model4_1, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'), 
                                 group = list(`До вынесения решения` = 1:6, `После вынесения решения` = 8:14),
                                 ylab = 'Цена бензина, руб./л',
                                 main = "Фиксированные эффекты события", 
                                 zero.par = list(col = "red", lwd = 2))

model4_2 <- feols(lЦена ~  `lЦена на нефть` + Марка + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model4_2)
coefplot_model4_2 <- coefplot(model4_2, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина'), 
                          group = list(`До вынесения решения` = 1:7, `После вынесения решения` = 9:15),
                          ylab = 'Цена бензина, руб./л',
                          main = "Фиксированные эффекты события", 
                          zero.par = list(col = "red", lwd = 2))

model4_3 <- feols(lЦена ~ `lЦена на нефть` + `lЦена за 1 т бензина` + Демпфер + I(Демпфер*`lЦена за 1 т бензина`) + `Марка` + event_resh_predp_lag.[7:1] + event_resh_predp_lead.[1:7] | Регион + Компания + Дата,  data = cdu, se = "twoway")
summary(model4_3)
etable(model4_3)
coefplot_model4_3 <- coefplot(model4_3, drop = c('lЦена на нефть', 'Марка', 'lЦена за 1 т бензина', 'Демпфер'), 
                                     #group = list(`До вынесения решения и выдачи предписания` = 1:5, `После вынесения решения и выдачи предписания` = 6:12),
                                     ylab = 'Темп роста розничной цены',
                                     main = "", 
                              #horiz = 1,
                              pt.join = TRUE, 
                              ci.fill = TRUE, 
                              #lab.cex = 5,
                              #lab.fit = "tilted",
                              dict = c(event_resh_predp_lag71 = '-7', 
                                       event_resh_predp_lag61 = '-6', 
                                       event_resh_predp_lag51 = '-5',
                                       event_resh_predp_lag41 = '-4', 
                                       event_resh_predp_lag31 = '-3', 
                                       event_resh_predp_lag21 = '-2',
                                       event_resh_predp_lag11 = '-1', 
                                       event_resh_predp_lead11 = '1',
                                       event_resh_predp_lead21 = '2',
                                       event_resh_predp_lead31 = '3',
                                       event_resh_predp_lead41 = '4',
                                       event_resh_predp_lead51 = '5',
                                       event_resh_predp_lead61 = '6',
                                       event_resh_predp_lead71 = '7'),
                                     zero.par = list(col = "red", lwd = 2))
}

# Графики
cairo_pdf(file = 'coefplot_predupr_upd.pdf', width = 10, height = 7)
coefplot_model1_3
dev.off()

cairo_pdf(file = 'coefplot_predost_upd.pdf', width = 10, height = 7)
coefplot_model2_2
dev.off()

cairo_pdf(file = 'coefplot_delo_upd.pdf', width = 10, height = 7)
coefplot_model3_2
dev.off()

cairo_pdf(file = 'coefplot_resh_predp_upd.pdf', width = 10, height = 7)
coefplot_model4_3
dev.off()


