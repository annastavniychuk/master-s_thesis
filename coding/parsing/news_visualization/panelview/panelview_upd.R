library('panelView')
library('readr')
library('dplyr')
library('ggplot2')
library('data.table')

library('devtools')
install_version("panelView", version = "1.1.5")
library('panelView')

# Импортируем данные и меняем формат дат
# data <- read_csv("finam/news_filtered.csv")
# data <- fread("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter.csv", header=T)
data <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_upd.csv")
data <- data %>% filter(FAS == 1)
data <- data %>% filter(event_predupr == 1 | event_predost == 1 | event_delo == 1 | event_resh_predp == 1)

data$preview_date <- as.Date(data$preview_date, format = "%d.%m.%Y")

# русифицируем название компаний для подписи осей
data1 <- data %>% mutate(company_rus=case_when(company == "rosneft" ~ 1,
                                                company =="bashneft-ank-ao" ~ 2,
                                                company == "gazprom-neft" ~ 3,
                                                company == "lukoil" ~ 4,
                                                company == "tatneft-3" ~ 5,
                                                TRUE ~ 0))
data1$company_rus[data1$company_rus==1] <- 'Роснефть'
data1$company_rus[data1$company_rus==2] <- 'Башнефть'
data1$company_rus[data1$company_rus==3] <- 'Газпромнефть'
data1$company_rus[data1$company_rus==4] <- 'Лукойл'
data1$company_rus[data1$company_rus==5] <- 'Татнефть'

# конвертируем one hot encoding to label encoding
data1 <- data1 %>% mutate(event=case_when(event_delo == TRUE ~ 1,
                                         event_predost == TRUE ~ 2,
                                         #event_predp == TRUE ~ 3,
                                         #event_resh == TRUE ~ 4, 
                                         event_resh_predp == TRUE ~ 3, 
                                         event_predupr == TRUE ~ 4,
                                         TRUE ~ 0))
data1$X1 <- c(1:nrow(data1))
# рисуем графичек
library(RColorBrewer)
mycol<-brewer.pal(4,"Set3")[c(1,2,3,4)]
graph <- panelView(X1 ~ event, data = data1, 
                   index = c("company_rus", "preview_date"),
                   by.timing = TRUE,
                   legend.labs = c("Дело", "Предостережение", "Решение и предписание", "Предупреждение"),
                   #main = "События с Финам.ру",
                   main = "",
                   background = 'white',
                   cex.lab = 30, 
                   #cex.axis.y=12, cex.axis.x=7,
                   cex.axis=25,
                   cex.legend = 30,
                   color = mycol,
                   #axis.lab.gap = c(1,0), 
                   xlab="Дата", ylab="Компания") 
graph
graph1 <- graph + theme(axis.text.x = element_text(angle = 90))
graph1

cairo_pdf(file = 'event_panelview_upd.pdf', width = 21, height = 12)
graph1
dev.off()
