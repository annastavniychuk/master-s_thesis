# Пакеты
{
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(zoo)
}

# Импорт данных
{
# ЦДУ
cdu <- read_excel("~/Documents/master's_thesis/coding/data/petrol/cdu/AZS_2012-2019.xlsx")
# ФАС
# ai_95 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_95.xlsx")
# ai_92_93 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_92_93.xlsx")
# ai_76_80 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_76_80.xlsx")
# События
{
news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_upd.csv", header=T)
news <- news %>% filter(FAS == 1)
news <- news %>% filter(event_predupr == 1 | event_predost == 1 | event_delo == 1 | event_resh_predp == 1)

news$date <- as.Date(news$article_date, format="%d.%m.%Y")

news$company <- str_replace(news$company, "rosneft", "rosn")
news$company <- str_replace(news$company, "bashneft-ank-ao", "bane")
news$company <- str_replace(news$company, "gazprom-neft", "sibn")
news$company <- str_replace(news$company, "lukoil", "lkoh")
news$company <- str_replace(news$company, "tatneft-3", "tatn")

# news_predupr <- news %>% filter(event_predupr==1)
# news_predupr <- news_predupr[,c(4,15)]
# news_predost <- news %>% filter(event_predost==1)
# news_predost <- news_predost[,c(4,15)]
# news_delo <- news %>% filter(event_delo==1)
# news_delo <- news_delo[,c(4,15)]
# news_resh_predp <- news %>% filter(event_resh_predp ==1)
# news_resh_predp <- news_resh_predp[,c(4,15)]

news <- news[,c(4,8,9,10,11,14,15)]
}
}


# # Обработка данных ФАС
# {
# # склеиваем отдельные датасеты с ценами
# fas <- rbind(ai_95, ai_92_93)
# fas <- rbind(fas, ai_76_80)
# 
# 
# # правим дату в нужный формат
# fas$Дата <- as.Date(fas$Дата, format="%m/%d/%Y")
# 
# # приводим название компаний в одинаковый вид
# # unique(fas[c("Винк")])
# # unique(news[c("company")])
# fas$Винк <- str_replace(fas$Винк, "ОАО 'Газпром нефть'", "sibn")
# fas$Винк <- str_replace(fas$Винк, "ОАО 'Лукойл'", "lkoh")
# fas$Винк <- str_replace(fas$Винк, "ОАО 'НК 'Роснефть'", "rosn")
# fas$Винк <- str_replace(fas$Винк, "ОАО АНК 'Башнефть'", "bane")
# 
# # добавляем к ценам данные о новостях
# colnames(news)[1] <- 'Винк'
# colnames(news)[7] <- 'Дата'
# fas <- left_join(fas, news, by = c('Винк'))
# 
# summary(fas)
# 
# # fas$treatment <- as.factor(ifelse(fas$event_predupr == TRUE, '1',
# #                             ifelse(fas$event_predost == TRUE, '2', 
# #                             ifelse(fas$event_delo == TRUE, '3', 
# #                             ifelse(fas$event_resh == TRUE, '4',
# #                             ifelse(fas$event_predp == TRUE, '5', NA))))))
# 
# # контроль или тритмент группа 
# # {
# #   fas$treatment <- fas$Винк==fas$company
# #   fas$treatment<-replace(fas$treatment, is.na(fas$treatment), 0)
# # }
# 
# # Подчищаем технические хвосты -- форматы данных и тд
# {
#   fas <- fas[,-c(1,8,9,10,11,12,17)]
#   fas$Cредняя_Розничная_цена <- as.numeric(fas$Cредняя_Розничная_цена)
#   
#   fas$event_predupr <- as.numeric(as.logical(fas$event_predupr))
#   fas$event_predost <- as.numeric(as.logical(fas$event_predost))
#   fas$event_delo <- as.numeric(as.logical(fas$event_delo))
#   fas$event_resh <- as.numeric(as.logical(fas$event_resh))
#   fas$event_predp <- as.numeric(as.logical(fas$event_predp))
#   
#   fas$event_predupr<-replace(fas$event_predupr, is.na(fas$event_predupr), 0)
#   fas$event_predost<-replace(fas$event_predost, is.na(fas$event_predost), 0)
#   fas$event_delo<-replace(fas$event_delo, is.na(fas$event_delo), 0)
#   fas$event_resh<-replace(fas$event_resh, is.na(fas$event_resh), 0)
#   fas$event_predp<-replace(fas$event_predp, is.na(fas$event_predp), 0)
# }
# }


# Обработка данных ЦДУ
{
cdu <- melt(setDT(cdu), id.vars = c("Дата","Регион", "Компания"), variable.name = "Марка")
colnames(cdu)[5] <- 'Цена'

# приводим название компаний в одинаковый вид
cdu <- as.data.frame(cdu)
# unique(cdu[c("Компания")])
# unique(news[c("company")])

news$company <- str_replace(news$company, "rosn", "РОСНЕФТЬ")
news$company <- str_replace(news$company, "bane", "БАШНЕФТЬ")
news$company <- str_replace(news$company, "sibn", "ГАЗПРОМ НЕФТЬ")
news$company <- str_replace(news$company, "lkoh", "ЛУКОЙЛ")

colnames(news)[7] <- 'Дата'
colnames(news)[1] <- 'Компания'

# добавляем к ценам данные о новостях
cdu <- left_join(cdu, news, by = c('Дата', 'Компания'))

# summary(cdu)

# Подчищаем технические хвосты -- форматы данных и тд
cdu$event_predupr <- as.logical(cdu$event_predupr)
cdu$event_predost <- as.logical(cdu$event_predost)
cdu$event_delo <- as.logical(cdu$event_delo)
cdu$event_resh_predp <- as.logical(cdu$event_resh_predp)

cdu$event_predupr <-replace(cdu$event_predupr, is.na(cdu$event_predupr), 0)
cdu$event_predost <-replace(cdu$event_predost, is.na(cdu$event_predost), 0)
cdu$event_delo <-replace(cdu$event_delo, is.na(cdu$event_delo), 0)
cdu$event_resh_predp <-replace(cdu$event_resh, is.na(cdu$event_resh_predp), 0)

# cdu$event_predupr <- as.factor(cdu$event_predupr)
# cdu$event_predost <- as.factor(cdu$event_predost)
# cdu$event_delo <- as.factor(cdu$event_delo)
# cdu$event_resh <- as.factor(cdu$event_resh)
# cdu$event_predp <- as.factor(cdu$event_predp)
}

# Контрольные переменные
{
brent <- read_excel("~/Documents/master's_thesis/coding/data/brent/prepared_brent/brent.xlsx")
brent <- brent[,c(3,8)]
colnames(brent)[1] <- 'Дата'
cdu <- left_join(cdu, brent, by = c('Дата'))
colnames(cdu)[11] <- 'Цена на нефть'

neer <- read_excel("~/Documents/master's_thesis/coding/data/rub_usd_exchange_rate/RC_F01_01_2012_T31_12_2019.xlsx")
neer <- neer[,c(2,3)]
colnames(neer)[1] <- 'Дата'
cdu <- left_join(cdu, neer, by = c('Дата'))
colnames(cdu)[12] <- 'Курс доллара'

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
library(zoo)
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
cdu <- cdu[,-c(13,14,15,17)]
colnames(cdu)[1] <- 'Дата'

cdu$`Цена на нефть` <- cdu$`Цена на нефть`*cdu$`Курс доллара`
}

cdu$Дата <- as.Date(cdu$Дата, origin="1970-01-01")



library(panelView)
panelview(`Цена` ~ event_predupr, data = cdu, index = c("Компания","Дата"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", main = "Simulated Data: Treatment Status")

install.packages('devtools')
library('devtools')
devtools::install_github('xuyiqing/fastplm')
devtools::install_github('xuyiqing/fect')
# devtools::install_github('xuyiqing/panelview')
library(fect)
data(fect)

install.packages('fastplm')
install.packages('fect', type = 'binary')
install.packages('fect', type = 'binary')

# ## Install from Github (development version)
devtools::install_github('xuyiqing/panelview')
devtools::install_github('xuyiqing/fastplm')
devtools::install_github('xuyiqing/fect')
