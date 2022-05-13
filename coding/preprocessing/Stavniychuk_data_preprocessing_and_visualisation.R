################################################################################
### Влияние мер конкурентной политики на рынки нефтепродуктов в России
### Ставнийчук Анна, экономический факультет МГУ, 2020-2022
################################################################################

############################ Установка пакетов #################################
library(readxl)
library(plm)
library(dplyr)
library(ggplot2)
library(plotly)
library(maptools)
library(sp)
library(sf)
library(rmapshaper)
library(stringr)

############################## Импорт данных ###################################
data <- read_excel("~/Documents/master's_thesis/coding/data/petrol/cdu/AZS_2012-2019.xlsx")
# data <- read_excel("AZS_2012-2019.xlsx")
colnames(data) <- c('date', 'region', 'firm', 'ai95', 'ai92')
data$year <- format(as.Date(data$date, format="%d/%m/%Y"),"%Y")

#################### Группированные данные для графиков ########################
data1 <- data %>% group_by(firm, date) %>% summarise(price95=mean(ai95), price92=mean(ai92))
data11 <- data %>% group_by(date) %>% summarise(price95=mean(ai95), price92=mean(ai92))
data1 <- data1 %>% mutate(price = 0.5*(price95 + price92))

data2 <- data %>% group_by(region, date) %>% summarise(price95=mean(ai95), price92=mean(ai92))
data3 <- data %>% group_by(firm, region, date) %>% summarise(price95=mean(ai95), price92=mean(ai92))
data4 <- data %>% group_by(year, region) %>% summarise(price95=mean(ai95), price92=mean(ai92))
data4 <- data4 %>% mutate(price = 0.5*(price95 + price92))

data6 <- data %>% group_by(year, region, firm) 
data6 <- subset(data6, year == 2019)
data62 <- data6 %>% group_by(year, region)  

data63 <- data6 %>% group_by(region)  %>% summarise(n=n())

#################### Графики ########################

dynamics95 <-ggplot(data1,                            
       aes(x = date,
           y = price95,
           col = firm)) +
    labs(x = 'Дата', y = 'Цена бензина марки АИ-95, руб.') +
    geom_line() +
    scale_colour_discrete("Компания") +
    theme(text = element_text(size = 12))

# cairo_pdf(file = '1.pdf', width = 10, height = 5)
# dynamics95
# dev.off()

dynamics92 <- ggplot(data1,                           
       aes(x = date,
           y = price92,
           col = firm)) +
    labs(x = 'Дата', y = 'Цена бензина марки АИ-92, руб.') +
    geom_line() +
    scale_colour_discrete("Компания") +
    theme(text = element_text(size = 12))

# cairo_pdf(file = '2.pdf', width = 10, height = 5)
# dynamics92
# dev.off()

dynamics <-ggplot(data1,                            
                    aes(x = date,
                        y = price,
                        col = firm)) +
  labs(x = 'Дата', y = 'Усреднённая по маркам цена бензина, руб.') +
  geom_line() +
  scale_colour_discrete("Компания") +
  theme_light() +
  theme(text = element_text(size = 16))
dynamics
ggplotly(dynamics)

cairo_pdf(file = 'dynamics.pdf', width = 10, height = 5)
dynamics
dev.off()

gr1 <- ggplot(data2,     # плохая идея, все сливается                      
       aes(x = date,
           y = price95,
           col = region)) +
    labs(x = 'Дата', y = 'Цена бензина марки АИ-95, руб.') +
    geom_line()
gr1 + theme(legend.position="bottom")
ggplotly(gr1)

gr2 <- ggplot(data2,    # плохая идея, все сливается                            
       aes(x = date,
           y = price92,
           col = region)) +
    labs(x = 'Дата', y = 'Цена бензина марки АИ-92, руб.') +
    geom_line()
gr2 + theme(legend.position="bottom")
ggplotly(gr2)

# ============================================================================ #
# carto_sf = st_read("/Users/annastavnijcuk/Desktop/russia_population_mk-main/carto_pop_laea")
carto_sf = st_read("~/Documents/master's_thesis/coding/preprocessing/mk_inid/carto_pop_laea")
# шейпфайл очень детальный, сильно замедляет работу и будет не очень аккуратно выглядеть
carto <- ms_simplify(carto_sf, keep = 1)
plot(carto$geometry)

data5 <- subset(data4, year == 2019)
region_uncommon = data5[!tolower(data5$region) %in% tolower(carto$full_nm),]

data5$region = str_replace(data5$region, "Ненецкий АО", "Ненецкий автономный округ")
data5$region = str_replace(data5$region, "Республика Адыгея (Адыгея)", "Республика Адыгея")
data5$region = str_replace(data5$region, "Республика Татарстан (Татарстан)", "Республика Татарстан")
data5$region = str_replace(data5$region, "Чувашская Республика - Чувашия", "Чувашская республика")

region_uncommon_upd = data5[!tolower(data5$region) %in% tolower(carto$full_nm),]

rusdata_fuel = left_join(carto, data5, by = c("full_nm" = "region"))

plot(rusdata_fuel["price95"])
laea = st_crs("+proj=laea +lat_0=50 +lon_0=90") # создаём проекцию Lambert azimuthal equal-area projection
rusdata_pop_laea = st_transform(rusdata_fuel, laea) # можно вписать код проекции: 3576 или 5940 тоже подойдут
plot(rusdata_fuel["price95"])

map95 <- ggplot() + 
    geom_sf(data = rusdata_fuel, aes(fill = price)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Цена на бензин, руб.") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12))
map95

cairo_pdf(file = 'map_2012_95.pdf', width = 10, height = 5)
map95
dev.off()

map92 <- ggplot() + 
    geom_sf(data = rusdata_fuel, aes(fill = price92)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Цена на бензин АИ-92") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12))

cairo_pdf(file = 'map_2012_92.pdf', width = 10, height = 5)
map92
dev.off()

# ============================================================================ #
brent <- read_excel("brent.xlsx", col_types = c("text", 
                                                "text", "date", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))
colnames(brent) <- c('ticker', 'per', 'date', 'time', 'open', 'high', 'low', 'close')

NER <- read_excel("RC_F01_01_2012_T31_12_2019.xlsx", col_types = c("numeric", "date", "numeric", "text"))
colnames(NER) <- c('nominal', 'date', 'curs', 'cdx')

rub_brent <- merge(x=brent,y=NER,by="date",all.x=TRUE)
rub_brent <- na.omit(rub_brent)
rub_brent$rub_close <- rub_brent$close*rub_brent$curs

plot_data <- merge(x=data11, y=rub_brent, by='date',all.x=TRUE)
plot_data <-na.omit(plot_data)

# Построим их вместе со вспомогательной осью
brent <- plot_ly(plot_data) %>%
    add_trace(x=~date,y=~price95,name = 'Цена бензина марки АИ-95, руб./литр',type = 'scatter',mode = 'lines',connectgaps = TRUE) %>%
    add_trace(x=~date,y=~price92,name = 'Цена бензина марки АИ-92, руб./литр',type = 'scatter',mode = 'lines',connectgaps = TRUE) %>%
    add_trace(x=~date,y=~rub_close,name = 'Цена на нефть марки Brent, руб./баррель',type = 'scatter',mode = 'lines',connectgaps = TRUE,yaxis = "y2") %>%
    layout(xaxis = list(title = "Дата", tickfont = list(size = 12)),
           yaxis2 = list(side = 'right', overlaying = "y", title = 'Цена на нефть марки Brent, руб./баррель', showgrid = FALSE, zeroline = FALSE, tickfont = list(size = 12)))
brent

#cairo_pdf(file = '3.pdf', width = 10, height = 5)
#brent
#dev.off()

#orca(brant,'3.svg')

plot_ly(plot_data) %>% 
    add_trace(x=~date, y=~price95, 
              name = 'Цена бензина марки АИ-95, руб./литр',
              type = 'scatter', 
              mode = 'lines',
              yaxis = "y1") %>%
    add_trace(x=~date, y=~price92, 
              name = 'Цена бензина марки АИ-92, руб./литр',
              type = 'scatter', 
              mode = 'lines',
              yaxis = "y1") %>%
    add_trace(x=~date, y=~rub_close, 
              name = 'Цена на нефть марки Brent, руб./баррель',
              type = 'scatter', 
              mode = 'lines', 
              yaxis = "y2") %>% 
    layout(xaxis = list(title = "Дата", tickfont = list(size = 12)),
           yaxis=list(title ='Цена бензинов марок АИ-92 и АИ-95, руб./литр', tickfont = list(size = 12)),
           yaxis2 = list(overlaying = "y", side = "right", title = 'Цена на нефть марки Brent, руб./баррель', tickfont = list(size = 12))) 

ggplot(rub_brent, aes(x = date, y = rub_close)) +
    labs(x = 'Дата', y = 'Цена на нефть марки Brent, долл./баррель') +
    geom_line() +
    theme(text = element_text(size = 16))

ggplot() +                            
    labs(x = 'Дата', y = 'Цена на нефть марки Brent, долл./баррель / Цена на бензин, руб.') +
    geom_line(data=data1, aes(x = date, y = price95, col = firm)) +
    geom_line(data=rub_brent, aes(x = date, y = rub_close)) +
    theme(text = element_text(size = 16))

ggplot(data1,                            
       aes(x = date,
           y = price95,
           col = firm)) +
    labs(x = 'Дата', y = 'Цена бензина марки АИ-95, руб.') +
    geom_line() +
    scale_colour_discrete("Компания") +
    theme(text = element_text(size = 16))

# ============================================================================ #

bashneft <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/BANE_120101_220101.csv")
# bashneft <- read_excel("bashneft.xlsx", col_types = c("text", 
#                                                       "text", "date", "numeric", "numeric", 
#                                                       "numeric", "numeric", "numeric", "numeric"))
colnames(bashneft) <- c('ticker', 'per', 'date', 'time', 'open', 'high', 'low', 'close', 'vol')
bashneft$firm <- 'Башнефть'

gazpromneft <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/SIBN_120101_220101.csv")
# gazpromneft <- read_excel("gazpromneft.xlsx", col_types = c("text", 
#                                                             "text", "date", "numeric", "numeric", 
#                                                             "numeric", "numeric", "numeric", "numeric"))
colnames(gazpromneft) <- c('ticker', 'per', 'date', 'time', 'open', 'high', 'low', 'close', 'vol')
gazpromneft$firm <- 'Газпромнефть'

lukoil <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/LKOH_120101_220101.csv")
# lukoil <- read_excel("lukoil.xlsx", col_types = c("text", 
#                                                   "text", "date", "numeric", "numeric", 
#                                                   "numeric", "numeric", "numeric", "numeric"))
colnames(lukoil) <- c('ticker', 'per', 'date', 'time', 'open', 'high', 'low', 'close', 'vol')
lukoil$firm <- 'Лукойл'

rosneft <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/ROSN_120101_220101.csv")
# rosneft <- read_excel("rosneft.xlsx", col_types = c("text", 
#                                                "text", "date", "numeric", "numeric", 
#                                                "numeric", "numeric", "numeric", "numeric"))
colnames(rosneft) <- c('ticker', 'per', 'date', 'time', 'open', 'high', 'low', 'close', 'vol')
rosneft$firm <- 'Роснефть'

tatneft <- read.csv("~/Documents/master's_thesis/coding/data/stocks/finam_stocks/1 day/TATN_120101_220101.csv")
# tatneft <- read_excel("tatneft.xlsx", col_types = c("text", 
#                                                "text", "date", "numeric", "numeric", 
#                                                "numeric", "numeric", "numeric", "numeric"))
colnames(tatneft) <- c('ticker', 'per', 'date', 'time', 'open', 'high', 'low', 'close', 'vol')
tatneft$firm <- 'Татнефть'

stock <- rbind(bashneft, gazpromneft)
stock <- rbind(stock, lukoil)
stock <- rbind(stock, rosneft)
stock <- rbind(stock, tatneft)

stock$date <- as.Date(stock$date, format = "%d/%m/%y")

# stock <- read.csv("~/Documents/master's_thesis/coding/data/stocks/stocks_day.csv")
stocks <- ggplot(stock,                            
       aes(x = date,
           y = close,
           col = firm)) +
    labs(x = 'Дата', y = 'Цена акции, руб.') +
    geom_line() +
    scale_colour_discrete("Компания") +
    theme_light() +
    theme(text = element_text(size = 16))
stocks
cairo_pdf(file = '4.pdf', width = 10, height = 5)
stocks
dev.off()
