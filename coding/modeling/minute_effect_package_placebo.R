# devtools::install_github("nipfpmf/eventstudies", ref="master")
library('eventstudies')
library('dplyr')
library('stringr')

# Подгружаем датасет с новостями
{
# news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_filtered.csv")
news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_upd.csv", header=T)
news <- news %>% filter(FAS == 1)
news <- news %>% filter(event_predupr == 1 | event_predost == 1 | event_delo == 1 | event_resh_predp == 1)
news$time <- paste(news$article_date, news$article_time)
news$time <- as.POSIXlt(news$time, format="%d.%m.%Y %H:%M")

news$company <- str_replace(news$company, "rosneft", "rosn")
news$company <- str_replace(news$company, "bashneft-ank-ao", "bane")
news$company <- str_replace(news$company, "gazprom-neft", "sibn")
news$company <- str_replace(news$company, "lukoil", "lkoh")
news$company <- str_replace(news$company, "tatneft-3", "tatn")

colnames(news)[15] <- 'when'
colnames(news)[4] <- 'name'

news_predupr <- news %>% filter(event_predupr==1)
news_predupr <- news_predupr[,c(4,15)]
news_predost <- news %>% filter(event_predost==1)
news_predost <- news_predost[,c(4,15)]
news_delo <- news %>% filter(event_delo==1)
news_delo <- news_delo[,c(4,15)]
# news_resh <- news %>% filter(event_resh==1)
# news_resh <- news_resh[,c(4,15)]
# news_predp <- news %>% filter(event_predp==1)
# news_predp <- news_predp[,c(4,15)]
news_resh_predp <- news %>% filter(event_resh_predp ==1)
news_resh_predp <- news_resh_predp[,c(4,15)]

set.seed(321)
delo <- sample(seq(as.POSIXlt('2012-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), as.POSIXlt('2019-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), by="min"),6)
predost <- sample(seq(as.POSIXlt('2012-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), as.POSIXlt('2019-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), by="min"),2)
predupr <- sample(seq(as.POSIXlt('2012-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), as.POSIXlt('2019-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), by="min"),9)
resh_predp <- sample(seq(as.POSIXlt('2012-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), as.POSIXlt('2019-01-01 00:00:01', format='%Y-%m-%d %H:%M:%S'), by="min"),3)

placebo_news_delo <- cbind(news_delo[1], delo)
colnames(placebo_news_delo)[2] <- 'when'
placebo_news_predost <- cbind(news_predost[1], predost)
colnames(placebo_news_predost)[2] <- 'when'
placebo_news_predupr <- cbind(news_predupr[1], predupr)
colnames(placebo_news_predupr)[2] <- 'when'
placebo_news_resh_predp <- cbind(news_resh_predp[1], resh_predp)
colnames(placebo_news_resh_predp)[2] <- 'when'
}

# Подгружаем датасет с котировками
{
stocks_minute <- read.csv("~/Documents/master's_thesis/coding/data/stocks/stocks_minute.csv")
stocks_minute$time <- as.POSIXlt(stocks_minute$time, format="%Y-%m-%d %H:%M:%S")
stocks_minute <- stocks_minute[order(stocks_minute$time),]
# stocks_minute <- stocks_minute %>% slice(which(row_number() %% 30 == 1))
stocks_minute <- stocks_minute %>% mutate(BANE.CLOSE = na.locf0(BANE.CLOSE), 
                                          SIBN.CLOSE = na.locf0(SIBN.CLOSE),
                                          LKOH.CLOSE = na.locf0(LKOH.CLOSE),
                                          ROSN.CLOSE = na.locf0(ROSN.CLOSE),
                                          SNGS.CLOSE = na.locf0(SNGS.CLOSE),
                                          TATN.CLOSE = na.locf0(TATN.CLOSE),
                                          IMOEX.CLOSE = na.locf0(IMOEX.CLOSE),
                                          MOEXOG.CLOSE = na.locf0(MOEXOG.CLOSE)) %>% 
  mutate(bane = BANE.CLOSE/lag(BANE.CLOSE)-1,
         sibn = SIBN.CLOSE/lag(SIBN.CLOSE)-1,
         lkoh = LKOH.CLOSE/lag(LKOH.CLOSE)-1,
         rosn = ROSN.CLOSE/lag(ROSN.CLOSE)-1,
         sngs = SNGS.CLOSE/lag(SNGS.CLOSE)-1,
         tatn = TATN.CLOSE/lag(TATN.CLOSE)-1,
         imoex = IMOEX.CLOSE/lag(IMOEX.CLOSE)-1,
         moexog = MOEXOG.CLOSE/lag(MOEXOG.CLOSE)-1)

stocks_minute <- stocks_minute[-c(1:5),-c(2:9)]
stocks_minute_zoo <- read.zoo(stocks_minute[,c(1:7)], format="%Y-%m-%d %H:%M:%S", tz = '')
imoex <- read.zoo(stocks_minute[,c(1,8)], format="%Y-%m-%d %H:%M:%S", tz = '')
}

# Графички
{
es_minute_predupr12 <- eventstudy(firm.returns = stocks_minute_zoo,
                 event.list = placebo_news_predupr,
                 event.window = 60*12,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(market.returns=imoex)) 
es_minute_predupr6 <- eventstudy(firm.returns = stocks_minute_zoo,
                                  event.list = placebo_news_predupr,
                                  event.window = 60*6,
                                  type = "marketModel",
                                  to.remap = TRUE,
                                  remap = "cumsum",
                                  inference = TRUE,
                                  inference.strategy = "bootstrap",
                                  model.args = list(market.returns=imoex)) 
es_minute_predupr3 <- eventstudy(firm.returns = stocks_minute_zoo,
                                  event.list = placebo_news_predupr,
                                  event.window = 60*3,
                                  type = "marketModel",
                                  to.remap = TRUE,
                                  remap = "cumsum",
                                  inference = TRUE,
                                  inference.strategy = "bootstrap",
                                  model.args = list(market.returns=imoex)) 
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_minute_predupr12)     
plot(es_minute_predupr6) 
plot(es_minute_predupr3) 
}
  
{
es_minute_predost12 <- eventstudy(firm.returns = stocks_minute_zoo,
                 event.list = placebo_news_predost,
                 event.window = 60*12,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(market.returns=imoex)) 
es_minute_predost6 <- eventstudy(firm.returns = stocks_minute_zoo,
                                event.list = placebo_news_predost,
                                event.window = 60*6,
                                type = "marketModel",
                                to.remap = TRUE,
                                remap = "cumsum",
                                inference = TRUE,
                                inference.strategy = "bootstrap",
                                model.args = list(market.returns=imoex))
es_minute_predost3 <- eventstudy(firm.returns = stocks_minute_zoo,
                                event.list = placebo_news_predost,
                                event.window = 60*3,
                                type = "marketModel",
                                to.remap = TRUE,
                                remap = "cumsum",
                                inference = TRUE,
                                inference.strategy = "bootstrap",
                                model.args = list(market.returns=imoex))
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_minute_predost12)   
plot(es_minute_predost6) 
plot(es_minute_predost3) 
}

{
es_minute_delo12 <- eventstudy(firm.returns = stocks_minute_zoo,
                 event.list = placebo_news_delo,
                 event.window = 60*12,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(market.returns=imoex)) 
es_minute_delo6 <- eventstudy(firm.returns = stocks_minute_zoo,
                             event.list = placebo_news_delo,
                             event.window = 60*6,
                             type = "marketModel",
                             to.remap = TRUE,
                             remap = "cumsum",
                             inference = TRUE,
                             inference.strategy = "bootstrap",
                             model.args = list(market.returns=imoex))
es_minute_delo3 <- eventstudy(firm.returns = stocks_minute_zoo,
                             event.list = placebo_news_delo,
                             event.window = 60*3,
                             type = "marketModel",
                             to.remap = TRUE,
                             remap = "cumsum",
                             inference = TRUE,
                             inference.strategy = "bootstrap",
                             model.args = list(market.returns=imoex))
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_minute_delo12)  
plot(es_minute_delo6) 
plot(es_minute_delo3) 
}

{
es_minute_resh_predp12 <- eventstudy(firm.returns = stocks_minute_zoo,
                 event.list = placebo_news_resh_predp,
                 event.window = 60*12,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(market.returns=imoex)) 
es_minute_resh_predp6 <- eventstudy(firm.returns = stocks_minute_zoo,
                                   event.list = placebo_news_resh_predp,
                                   event.window = 60*6,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=imoex))
es_minute_resh_predp3 <- eventstudy(firm.returns = stocks_minute_zoo,
                                   event.list = placebo_news_resh_predp,
                                   event.window = 60*3,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=imoex))
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_minute_resh_predp12)  
plot(es_minute_resh_predp6)  
plot(es_minute_resh_predp3)  
}

# es_minute_predp <- eventstudy(firm.returns = stocks_minute_zoo,
#                  event.list = news_predp,
#                  event.window = 7,
#                  type = "marketModel",
#                  to.remap = TRUE,
#                  remap = "cumsum",
#                  inference = TRUE,
#                  inference.strategy = "bootstrap",
#                  model.args = list(market.returns=imoex)) 
# par(mai=c(.8,.8,.2,.2), cex=.7)
# plot(es)  

cairo_pdf(file = "es_minute_predupr12.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_predupr12) 
dev.off()
cairo_pdf(file = "es_minute_predupr6.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_predupr6) 
dev.off()
cairo_pdf(file = "es_minute_predupr3.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_predupr3) 
dev.off()

cairo_pdf(file = "es_minute_predost12.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_predost12) 
dev.off()
cairo_pdf(file = "es_minute_predost6.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_predost6) 
dev.off()
cairo_pdf(file = "es_minute_predost3.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_predost3) 
dev.off()

cairo_pdf(file = "es_minute_delo12.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_delo12) 
dev.off()
cairo_pdf(file = "es_minute_delo6.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_delo6) 
dev.off()
cairo_pdf(file = "es_minute_delo3.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_delo3) 
dev.off()

cairo_pdf(file = "es_minute_resh_predp12.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_resh_predp12) 
dev.off()
cairo_pdf(file = "es_minute_resh_predp6.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_resh_predp6) 
dev.off()
cairo_pdf(file = "es_minute_resh_predp3.pdf", width = 10, height = 7)
par(mai=c(.8,.8,.2,.2), cex=.9)
plot(es_minute_resh_predp3) 
dev.off()
