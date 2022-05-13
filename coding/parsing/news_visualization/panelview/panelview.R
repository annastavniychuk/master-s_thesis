library('panelView')
library('readr')
library('dplyr')
library('ggplot2')

# Импортируем данные и меняем формат дат
data <- read_csv("finam/news_filtered.csv")
data$preview_date <- as.Date(data$preview_date, tryFormats = c("%d.%m.%Y"))

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
                                         event_predp == TRUE ~ 3,
                                         event_resh == TRUE ~ 4, 
                                         event_predupr == TRUE ~ 5,
                                         TRUE ~ 0))

# рисуем графичек
graph <- panelView(X1 ~ event, data = data1, 
                   index = c("company_rus", "preview_date"),
                   by.timing = TRUE,
                   legend.labs = c("Дело", "Предостережение", "Предписание", "Решение", "Предупреждение"),
                   main = "События с Финам.ру",
                   background = 'white',
                   cex.lab = 14, cex.axis.y=12, cex.axis.x=7,
                   #axis.lab.gap = c(1,0), 
                   xlab="Дата", ylab="Компания") 
graph1 <- graph + theme(axis.text.x = element_text(angle = 90))

cairo_pdf(file = 'event_panelview.pdf', width = 21, height = 12)
graph1
dev.off()
