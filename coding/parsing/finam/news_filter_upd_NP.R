{
library('dplyr')
library('readxl')
}

news <-read_excel("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_updNP.xlsx")

news <- news[,c(3:15)]
news$event_resh_predp <- news$event_resh + news$event_predp
news$event_resh_predp <- as.logical(news$event_resh_predp)
summary(news)


write.csv(news,"~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_upd.csv", row.names = FALSE)
