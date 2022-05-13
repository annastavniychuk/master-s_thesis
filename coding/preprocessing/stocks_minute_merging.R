# Импортируем датасеты, склеиваем годичные данные, делаем столбец дата-время в правильном формате

# Башнефть

BANE_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_120101_130101.csv")
BANE_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_130101_140101.csv")
BANE_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_140101_150101.csv")
BANE_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_150101_160101.csv")
BANE_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_160101_170101.csv")
BANE_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_170101_180101.csv")
BANE_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_180101_190101.csv")
BANE_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_190101_200101.csv")
BANE_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_200101_210101.csv")
BANE_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Башнефть/BANE_210101_220101.csv")

BANE_120101_220101 <- rbind(BANE_120101_130101, BANE_130101_140101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_140101_150101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_150101_160101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_160101_170101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_170101_180101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_180101_190101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_190101_200101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_200101_210101)
BANE_120101_220101 <- rbind(BANE_120101_220101, BANE_210101_220101)

BANE_120101_220101$time <- paste(BANE_120101_220101$X.DATE., BANE_120101_220101$X.TIME.)
BANE_120101_220101$time <- as.POSIXlt(BANE_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(BANE_120101_220101)[8] <- 'BANE.CLOSE'

# Газпромнефть

SIBN_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_120101_130101.csv")
SIBN_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_130101_140101.csv")
SIBN_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_140101_150101.csv")
SIBN_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_150101_160101.csv")
SIBN_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_160101_170101.csv")
SIBN_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_170101_180101.csv")
SIBN_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_180101_190101.csv")
SIBN_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_190101_200101.csv")
SIBN_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_200101_210101.csv")
SIBN_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Газпромнефть/SIBN_210101_220101.csv")

SIBN_120101_220101 <- rbind(SIBN_120101_130101, SIBN_130101_140101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_140101_150101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_150101_160101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_160101_170101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_170101_180101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_180101_190101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_190101_200101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_200101_210101)
SIBN_120101_220101 <- rbind(SIBN_120101_220101, SIBN_210101_220101)

SIBN_120101_220101$time <- paste(SIBN_120101_220101$X.DATE., SIBN_120101_220101$X.TIME.)
SIBN_120101_220101$time <- as.POSIXlt(SIBN_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(SIBN_120101_220101)[8] <- 'SIBN.CLOSE'

# Лукойл

LKOH_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_120101_130101.csv")
LKOH_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_130101_140101.csv")
LKOH_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_140101_150101.csv")
LKOH_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_150101_160101.csv")
LKOH_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_160101_170101.csv")
LKOH_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_170101_180101.csv")
LKOH_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_180101_190101.csv")
LKOH_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_190101_200101.csv")
LKOH_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_200101_210101.csv")
LKOH_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Лукойл/LKOH_210101_220101.csv")

LKOH_120101_220101 <- rbind(LKOH_120101_130101, LKOH_130101_140101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_140101_150101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_150101_160101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_160101_170101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_170101_180101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_180101_190101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_190101_200101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_200101_210101)
LKOH_120101_220101 <- rbind(LKOH_120101_220101, LKOH_210101_220101)

LKOH_120101_220101$time <- paste(LKOH_120101_220101$X.DATE., LKOH_120101_220101$X.TIME.)
LKOH_120101_220101$time <- as.POSIXlt(LKOH_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(LKOH_120101_220101)[8] <- 'LKOH.CLOSE'

# Роснефть

ROSN_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_120101_130101.csv")
ROSN_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_130101_140101.csv")
ROSN_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_140101_150101.csv")
ROSN_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_150101_160101.csv")
ROSN_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_160101_170101.csv")
ROSN_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_170101_180101.csv")
ROSN_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_180101_190101.csv")
ROSN_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_190101_200101.csv")
ROSN_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_200101_210101.csv")
ROSN_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Роснефть/ROSN_210101_220101.csv")

ROSN_120101_220101 <- rbind(ROSN_120101_130101, ROSN_130101_140101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_140101_150101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_150101_160101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_160101_170101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_170101_180101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_180101_190101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_190101_200101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_200101_210101)
ROSN_120101_220101 <- rbind(ROSN_120101_220101, ROSN_210101_220101)

ROSN_120101_220101$time <- paste(ROSN_120101_220101$X.DATE., ROSN_120101_220101$X.TIME.)
ROSN_120101_220101$time <- as.POSIXlt(ROSN_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(ROSN_120101_220101)[8] <- 'ROSN.CLOSE'

# Сургутнефтегаз

SNGS_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_120101_130101.csv")
SNGS_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_130101_140101.csv")
SNGS_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_140101_150101.csv")
SNGS_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_150101_160101.csv")
SNGS_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_160101_170101.csv")
SNGS_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_170101_180101.csv")
SNGS_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_180101_190101.csv")
SNGS_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_190101_200101.csv")
SNGS_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_200101_210101.csv")
SNGS_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Сургутнефтегаз/SNGS_210101_220101.csv")

SNGS_120101_220101 <- rbind(SNGS_120101_130101, SNGS_130101_140101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_140101_150101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_150101_160101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_160101_170101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_170101_180101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_180101_190101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_190101_200101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_200101_210101)
SNGS_120101_220101 <- rbind(SNGS_120101_220101, SNGS_210101_220101)

SNGS_120101_220101$time <- paste(SNGS_120101_220101$X.DATE., SNGS_120101_220101$X.TIME.)
SNGS_120101_220101$time <- as.POSIXlt(SNGS_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(SNGS_120101_220101)[8] <- 'SNGS.CLOSE'

# Татнефть

TATN_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_120101_130101.csv")
TATN_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_130101_140101.csv")
TATN_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_140101_150101.csv")
TATN_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_150101_160101.csv")
TATN_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_160101_170101.csv")
TATN_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_170101_180101.csv")
TATN_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_180101_190101.csv")
TATN_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_190101_200101.csv")
TATN_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_200101_210101.csv")
TATN_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Татнефть/TATN_210101_220101.csv")

TATN_120101_220101 <- rbind(TATN_120101_130101, TATN_130101_140101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_140101_150101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_150101_160101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_160101_170101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_170101_180101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_180101_190101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_190101_200101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_200101_210101)
TATN_120101_220101 <- rbind(TATN_120101_220101, TATN_210101_220101)

TATN_120101_220101$time <- paste(TATN_120101_220101$X.DATE., TATN_120101_220101$X.TIME.)
TATN_120101_220101$time <- as.POSIXlt(TATN_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(TATN_120101_220101)[8] <- 'TATN.CLOSE'

# Индекс Мосбиржи

IMOEX_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_120101_130101.csv")
IMOEX_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_130101_140101.csv")
IMOEX_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_140101_150101.csv")
IMOEX_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_150101_160101.csv")
IMOEX_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_160101_170101.csv")
IMOEX_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_170101_180101.csv")
IMOEX_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_180101_190101.csv")
IMOEX_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_190101_200101.csv")
IMOEX_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_200101_210101.csv")
IMOEX_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс МосБиржи/IMOEX_210101_220101.csv")

IMOEX_120101_220101 <- rbind(IMOEX_120101_130101, IMOEX_130101_140101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_140101_150101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_150101_160101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_160101_170101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_170101_180101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_180101_190101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_190101_200101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_200101_210101)
IMOEX_120101_220101 <- rbind(IMOEX_120101_220101, IMOEX_210101_220101)

IMOEX_120101_220101$time <- paste(IMOEX_120101_220101$X.DATE., IMOEX_120101_220101$X.TIME.)
IMOEX_120101_220101$time <- as.POSIXlt(IMOEX_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(IMOEX_120101_220101)[8] <- 'IMOEX.CLOSE'

# Индекс нефти и газа

MOEXOG_120101_130101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_120101_130101.csv")
MOEXOG_130101_140101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_130101_140101.csv")
MOEXOG_140101_150101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_140101_150101.csv")
MOEXOG_150101_160101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_150101_160101.csv")
MOEXOG_160101_170101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_160101_170101.csv")
MOEXOG_170101_180101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_170101_180101.csv")
MOEXOG_180101_190101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_180101_190101.csv")
MOEXOG_190101_200101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_190101_200101.csv")
MOEXOG_200101_210101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_200101_210101.csv")
MOEXOG_210101_220101 <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 minute/Индекс нефти и газа/MOEXOG_210101_220101.csv")

MOEXOG_120101_220101 <- rbind(MOEXOG_120101_130101, MOEXOG_130101_140101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_140101_150101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_150101_160101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_160101_170101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_170101_180101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_180101_190101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_190101_200101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_200101_210101)
MOEXOG_120101_220101 <- rbind(MOEXOG_120101_220101, MOEXOG_210101_220101)

MOEXOG_120101_220101$time <- paste(MOEXOG_120101_220101$X.DATE., MOEXOG_120101_220101$X.TIME.)
MOEXOG_120101_220101$time <- as.POSIXlt(MOEXOG_120101_220101$time, format="%d/%m/%y %H:%M:%S")
colnames(MOEXOG_120101_220101)[8] <- 'MOEXOG.CLOSE'

# Мерджим данные
minute <- merge(x = BANE_120101_220101, y = SIBN_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
minute <- merge(x = minute, y = LKOH_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
minute <- merge(x = minute, y = ROSN_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
minute <- merge(x = minute, y = SNGS_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
minute <- merge(x = minute, y = TATN_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
minute <- merge(x = minute, y = IMOEX_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)
minute <- merge(x = minute, y = MOEXOG_120101_220101, by = c("time"), all.x = TRUE, all.y = TRUE)

minute <- minute[,c(1,9,18,27,36,45,54,63,72)]

# Экспортируем готовый датасет

write.csv(minute,"~/Documents/master's_thesis/coding/data/stocks/stocks_minute.csv", row.names = FALSE)
#library("writexl")
#write_xlsx(minute,"~/Documents/master's_thesis/coding/data/stocks/stocks_minute.xlsx") # 1,5кк строк в эксель не влазит

