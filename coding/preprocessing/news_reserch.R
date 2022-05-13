{
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
}

#news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_filtered.csv")
news <- read_excel("~/Documents/master's_thesis/coding/parsing/finam/news_filtered1.xlsx")

news$article_date <- as.Date(news$article_date, format ="%d.%m.%Y")

news$year <- year(news$article_date)
news$month <- month(news$article_date)
news$week <- week(news$article_date)
news$wday <- wday(news$article_date)-1
news$hour <- as.numeric(substr(news$article_time,1,2))

cairo_pdf(file = 'hour.pdf', width = 10, height = 7)
ggplot(news, aes(x=hour, fill = event)) +
  geom_bar(stat = "count") +
  labs(x = "Время", y = "Количество событий") + 
  scale_fill_discrete(name = "Тип события", labels = c("Дело","Предостережение", "Предписание", "Предупреждение", "Решение")) +
  #theme_bw() +
  theme(legend.title=element_text(size=20, face = "bold"), 
        legend.text=element_text(size=20),
        text = element_text(size = 20))
dev.off()

cairo_pdf(file = 'wday.pdf', width = 10, height = 7)
ggplot(news, aes(x=wday, fill = event)) +
  geom_bar(stat = "count") +
  labs(x = "День недели", y = "Количество событий") + 
  scale_fill_discrete(name = "Тип события", labels = c("Дело","Предостережение", "Предписание", "Предупреждение", "Решение")) +
  theme(legend.title=element_text(size=20, face = "bold"), 
        legend.text=element_text(size=20),
        text = element_text(size = 20))
dev.off()

cairo_pdf(file = 'month.pdf', width = 10, height = 7)
ggplot(news, aes(x=month, fill = event)) +
  geom_bar(stat = "count") +
  labs(x = "Месяц", y = "Количество событий") + 
  scale_fill_discrete(name = "Тип события", labels = c("Дело","Предостережение", "Предписание", "Предупреждение", "Решение")) +
  theme(legend.title=element_text(size=20, face = "bold"), 
        legend.text=element_text(size=20),
        text = element_text(size = 20))
dev.off()

cairo_pdf(file = 'year.pdf', width = 10, height = 7)
ggplot(news, aes(x=year, fill = event)) +
  geom_bar(stat = "count") +
  labs(x = "Год", y = "Количество событий") + 
  scale_fill_discrete(name = "Тип события", labels = c("Дело","Предостережение", "Предписание", "Предупреждение", "Решение")) +
  theme(legend.title=element_text(size=20, face = "bold"), 
        legend.text=element_text(size=20),
        text = element_text(size = 20))
dev.off()

