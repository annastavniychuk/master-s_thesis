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
#data <- filter(data, vink=="Независимый")
#################### Группированные данные для графиков ########################
# средняя цена марки в регионе в год
data1 <- data %>% group_by(year, region) %>% summarise(mean95=mean(price95), mean9293=mean(price9293), mean7680=mean(price7680))
# количество винков в каждом регионе по годам
data2 <- data %>% group_by(year, region, vink) %>% summarise(mean95=mean(price95), mean9293=mean(price9293), mean7680=mean(price7680)) %>% group_by(year, region) %>% summarise(n = n())

#################### Графики ########################
# carto_sf = st_read("~/Documents/master's_thesis/coding/preprocessing/carto_pop_laea")
carto_sf = st_read("~/Documents/master's_thesis/coding/preprocessing/mk_inid/carto_pop_laea")
# шейпфайл очень детальный, сильно замедляет работу и будет не очень аккуратно выглядеть
carto <- ms_simplify(carto_sf, keep = 1)
#plot(carto$geometry)

data3_2012 <- subset(data2, year == 2012)
data3_2013 <- subset(data2, year == 2013)
data3_2014 <- subset(data2, year == 2014)
data3_2015 <- subset(data2, year == 2015)
data3_2016 <- subset(data2, year == 2016)
data3_2017 <- subset(data2, year == 2017)
data3_2018 <- subset(data2, year == 2018)
data3_2019 <- subset(data2, year == 2019)

rusdata_fuel_2013 = left_join(carto, data3_2013, by = c("full_nm" = "region"))
rusdata_fuel_2014 = left_join(carto, data3_2014, by = c("full_nm" = "region"))
rusdata_fuel_2015 = left_join(carto, data3_2015, by = c("full_nm" = "region"))
rusdata_fuel_2016 = left_join(carto, data3_2016, by = c("full_nm" = "region"))
rusdata_fuel_2017 = left_join(carto, data3_2017, by = c("full_nm" = "region"))
rusdata_fuel_2018 = left_join(carto, data3_2018, by = c("full_nm" = "region"))
rusdata_fuel_2019 = left_join(carto, data3_2019, by = c("full_nm" = "region"))

mapcomp_2013 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2013, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2013 году")
mapcomp_2014 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2014, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2014 году")
mapcomp_2015 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2015, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2015 году")
mapcomp_2016 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2016, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2016 году")
mapcomp_2017 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2017, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2017 году")
mapcomp_2018 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2018, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2018 году")
mapcomp_2019 <- ggplot() + 
    geom_sf(data = rusdata_fuel_2019, aes(fill = n)) +
    scale_fill_gradient(high = "#e34a33", 
                        low = "#fee8c8", 
                        guide = "legend", 
                        na.value = "grey80",
                        #abels=c(0.1, 1, 10, 100, 1000), 
                        #breaks=c(-1, 0, 1, 2, 3), 
                        name="Количество ВИНК") + # указываю палитру и легенду
    # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
    # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
    theme_void()+
    theme(text = element_text(size = 12)) +
    ggtitle("Конкуренция в 2019 году")

mapcomp_2013 
mapcomp_2014
mapcomp_2015
mapcomp_2016
mapcomp_2017
mapcomp_2018
mapcomp_2019

cairo_pdf(file = 'mapcomp_2013.pdf', width = 10, height = 5)
mapcomp_2013
dev.off()

cairo_pdf(file = 'mapcomp_2014.pdf', width = 10, height = 5)
mapcomp_2014
dev.off()

cairo_pdf(file = 'mapcomp_2015.pdf', width = 10, height = 5)
mapcomp_2015
dev.off()

cairo_pdf(file = 'mapcomp_2016.pdf', width = 10, height = 5)
mapcomp_2016
dev.off()

cairo_pdf(file = 'mapcomp_2017.pdf', width = 10, height = 5)
mapcomp_2017
dev.off()

cairo_pdf(file = 'mapcomp_2018.pdf', width = 10, height = 5)
mapcomp_2018
dev.off()

cairo_pdf(file = 'mapcomp_2019.pdf', width = 10, height = 5)
mapcomp_2019
dev.off()
