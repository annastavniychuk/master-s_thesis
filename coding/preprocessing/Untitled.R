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
ai_95 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_95.xlsx")
ai_92_93 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_92_93.xlsx")
ai_76_80 <- read_excel("~/Documents/master's_thesis/coding/data/petrol/fas/ai_76_80.xlsx")

ai_95 <- ai_95[,-c(1,8)]
ai_92_93 <- ai_92_93[,-c(1,8)]
ai_76_80 <- ai_76_80[,-c(1,8)]

colnames(ai_95) <- c('vink', 'region', 'type', 'district', 'date', 'price95')
colnames(ai_92_93) <- c('vink', 'region', 'type', 'district', 'date', 'price9293')
colnames(ai_76_80) <- c('vink', 'region', 'type', 'district', 'date', 'price7680')

data <- left_join(ai_95, ai_92_93, by = c("vink", "region", "district", "date"))
data <- left_join(data, ai_76_80, by = c("vink", "region", "district", "date"))

data <- data[,-c(3,7,9)]
data <- transform(data, 
                  vink = as.factor(vink), 
                  region = as.factor(region), 
                  district = as.factor(district), 
                  date = as.Date(date, format ="%d/%m/%Y"), 
                  price95 = as.numeric(price95), 
                  price9293 = as.numeric(price9293), 
                  price7680 = as.numeric(price7680))
data$year <- as.factor(format(as.Date(data$date, format="%d/%m/%Y"),"%Y"))

#################### Группированные данные для графиков ########################
# средняя цена марки в регионе в год
data1 <- data %>% group_by(year, region) %>% summarise(mean95=mean(price95), mean9293=mean(price9293), mean7680=mean(price7680))
# количество винков в каждом регионе по годам
data2 <- data %>% group_by(year, region, vink) %>% summarise(mean95=mean(price95), mean9293=mean(price9293), mean7680=mean(price7680)) %>% group_by(year, region) %>% summarise(n = n())

data2 <- subset(data2, year == 2019)

#################### Графики ########################
carto_sf = st_read("~/Documents/master's_thesis/coding/preprocessing/carto_pop_laea")
# шейпфайл очень детальный, сильно замедляет работу и будет не очень аккуратно выглядеть
carto <- ms_simplify(carto_sf, keep = 1)
plot(carto$geometry)

data3 <- subset(data1, year == 2019)

#region_uncommon = data3[!tolower(data3$region) %in% tolower(carto$full_nm),]
#data3$region = str_replace(data3$region, "Ненецкий АО", "Ненецкий автономный округ")
#data3$region = str_replace(data3$region, "Республика Адыгея (Адыгея)", "Республика Адыгея")
#data3$region = str_replace(data3$region, "Республика Татарстан (Татарстан)", "Республика Татарстан")
#data3$region = str_replace(data3$region, "Чувашская Республика - Чувашия", "Чувашская республика")
#region_uncommon_upd = data3[!tolower(data3$region) %in% tolower(carto$full_nm),]

rusdata_fuel = left_join(carto, data3, by = c("full_nm" = "region"))

#plot(rusdata_fuel["mean95"])
#laea = st_crs("+proj=laea +lat_0=50 +lon_0=90") # создаём проекцию Lambert azimuthal equal-area projection
#rusdata_pop_laea = st_transform(rusdata_fuel, laea) # можно вписать код проекции: 3576 или 5940 тоже подойдут
#plot(rusdata_fuel["mean95"])

map95 <- ggplot() + 
    geom_sf(data = rusdata_fuel, aes(fill = mean95)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Цена на бензин АИ-95") + # указываю палитру и легенду
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