# Импортируем датасеты, делаем столбец дата-время в правильном формате

# Башнефть
BANE_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/BANE_120101_220101.csv")
BANE_120101_220101$time <- as.Date(BANE_120101_220101$X.DATE., format="%d/%m/%y")
colnames(BANE_120101_220101)[8] <- 'BANE.CLOSE'

# Газпромнефть
SIBN_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/SIBN_120101_220101.csv")
SIBN_120101_220101$time <- as.Date(SIBN_120101_220101$X.DATE., format="%d/%m/%y")
colnames(SIBN_120101_220101)[8] <- 'SIBN.CLOSE'

# Лукойл
LKOH_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/LKOH_120101_220101.csv")
LKOH_120101_220101$time <- as.Date(LKOH_120101_220101$X.DATE., format="%d/%m/%y")
colnames(LKOH_120101_220101)[8] <- 'LKOH.CLOSE'

# Роснефть
ROSN_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/ROSN_120101_220101.csv")
ROSN_120101_220101$time <- as.Date(ROSN_120101_220101$X.DATE., format="%d/%m/%y")
colnames(ROSN_120101_220101)[8] <- 'ROSN.CLOSE'

# Сургутнефтегаз
SNGS_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/SNGS_120101_220101.csv")
SNGS_120101_220101$time <- as.Date(SNGS_120101_220101$X.DATE., format="%d/%m/%y")
colnames(SNGS_120101_220101)[8] <- 'SNGS.CLOSE'

# Татнефть
TATN_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/TATN_120101_220101.csv")
TATN_120101_220101$time <- as.Date(TATN_120101_220101$X.DATE., format="%d/%m/%y")
colnames(TATN_120101_220101)[8] <- 'TATN.CLOSE'

# Индекс Мосбиржи
IMOEX_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/IMOEX_120101_220101.csv")
IMOEX_120101_220101$time <- as.Date(IMOEX_120101_220101$X.DATE., format="%d/%m/%y")
colnames(IMOEX_120101_220101)[8] <- 'IMOEX.CLOSE'

# Индекс нефти и газа
MOEXOG_120101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/MOEXOG_120101_220101.csv")
MOEXOG_120101_220101$time <- as.Date(MOEXOG_120101_220101$X.DATE., format="%d/%m/%y")
colnames(MOEXOG_120101_220101)[8] <- 'MOEXOG.CLOSE'

# Мерджим данные
day <- merge(x = BANE_120101_220101, y = SIBN_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
day <- merge(x = day, y = LKOH_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
day <- merge(x = day, y = ROSN_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
day <- merge(x = day, y = SNGS_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
day <- merge(x = day, y = TATN_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
day <- merge(x = day, y = IMOEX_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
day <- merge(x = day, y = MOEXOG_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)

day <- day[,c(1,9,18,27,36,45,54,63,72)]

# Экспортируем готовый датасет

write.csv(day,"~/Documents/master's_thesis/coding/data/stocks/stocks_day.csv", row.names = FALSE)
#library("writexl")
#write_xlsx(day,"~/Documents/master's_thesis/coding/data/stocks/stocks_day.xlsx") # 1,5кк строк в эксель не влазит