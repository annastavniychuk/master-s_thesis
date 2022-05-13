# мастер-класс по картам от ИНИД, Ира Королева 18 марта 2021
install.packages("rmapshaper")
install.packages("measurements")
install.packages("mapproj")
install.packages("sp")
install.packages("sf")
install.packages("dplyr")
install.packages("rgdal")
install.packages("stringr")
install.packages("ggplot2")
install.packages("readr")
install.packages("RColorBrewer")
library(rmapshaper)
library(measurements)
library(sp)
library(sf)
library(dplyr)
library(rgdal)
library(stringr)
library(ggplot2)
library(readr)
library(RColorBrewer)
# par(mar=c(1,1,1,1))
options(scipen = 999) # чтобы большие числа не отображались в экспоненциальной записи (через е)

population = read_delim("population.csv", ";", trim_ws = TRUE)

# перевод координат в десятичные градусы
population$lat = gsub('.', ' ', population$latitude, fixed = TRUE)
population$long = gsub('.', ' ', population$longitude, fixed = TRUE)
population$lat = measurements::conv_unit(population$lat, from = 'deg_min_sec', to = 'dec_deg')
population$long = measurements::conv_unit(population$long, from = 'deg_min_sec', to = 'dec_deg')
population$lat = as.numeric(population$lat)
population$long = as.numeric(population$long)

ggplot() + geom_point(data = population[population$type == "с",], aes(x = long, y = lat))

unique(population$type)
# некоторые наблюдения
# оп - остановочный пункт
# рзд.п - разъезд посёлок
# c/п - сельское поселение
# x - Кретов-Первый "х" записано латиницей
# "с" латиницей и кириллицей
# д. и д
# аал - не аул
# гп - городской посёлок
# л\п - лесной посёлок
# мкр - микрорайон
population$type = str_replace(population$type, "x", "х")
population$type = str_replace(population$type, "c", "с")
population$type = str_replace(population$type, "д.", "д")

summary(population$population)
ggplot() + geom_histogram(data = population, aes(x = population), bins = 100)
summary(population$children)
ggplot() + geom_histogram(data = population, aes(x = children), bins = 100)

# в некоторых поселениях записано больше детей, чем жителей всего
children = population[population$children > population$population,]

na = population[!complete.cases(population),] # в данных всего 1 пропуск
population[!complete.cases(population),][11] = 0 # замена NA на 0

# write.csv(population, "population_coords.csv") сохранить файл
# ================================================================================================

duplicat = population[duplicated(population[,2:15]) | duplicated(population[,2:15], fromLast = TRUE),]

moscow = population[population$settlement == "Москва",]
# Большие города разбиты на районы, некоторые строки содержат идентичную информацию, 
# и в разных регионах встречаются поселения с одинаковыми названиями, 
# поэтому в группировку надо включить регион, но и среди них есть одинаковые, 
# так что включаем и тип поселения
pop_settlement = population %>% group_by(region, settlement, type) %>% summarise(sum = sum(population), frst_long = first(long), frst_lat = first(lat))
# выкинем выбросы, чтобы увидеть остальное распределение
ggplot() + geom_histogram(data = pop_settlement[pop_settlement$sum > 0 & pop_settlement$sum < 5000,] , aes(x = sum), binwidth = 100)

write.csv(pop_settlement, "settlement_unique_pop_coords.csv")

# пустые поселения со всего датасета
pop_zero = population[population$population == 0,]

# пустые поселения среди объединённых
length(pop_settlement[pop_settlement$sum == 0,]$sum) # 16657
length(pop_settlement[pop_settlement$sum == 0,]$sum)/nrow(pop_settlement) * 100 # ~13%  - пустые поселения
max(population$population)

unique(pop_settlement$type)
type = group_by(pop_settlement, type) %>% tally() # сколько таких поселений есть
type_pop = group_by(pop_settlement, type) %>% summarise(sum_pop = sum(sum)) # сколько людей в них живёт
# type = type[order(type$n, decreasing = TRUE),] сортировка датасета по n
ggplot() + geom_col(data = type, aes(x = reorder(type, -n), y = n)) + 
  theme(axis.text.x = element_text(angle = 90))

ggplot() + geom_col(data = type_pop, aes(x = reorder(type, -sum_pop), y = sum_pop)) + 
  theme(axis.text.x = element_text(angle = 90))

# количество строк на каждый класс = кол-во поселений в каждом регионе
region_settl = group_by(pop_settlement, region) %>% tally
# Районы больших городов иногда записаны своими названиями, а иногда общим, 
# поэтому у СПб значение 3, у Севастополя 43, а у Москвы 1
ggplot() + geom_col(data = region_settl, aes(x = reorder(region, -n), y = n)) +
  theme(axis.text.x = element_text(angle = 90))


# ПОЛИГОНЫ 
# https://obulantsev.carto.com/tables/russia_geojson_wgs84/public 
# !!! тяжёлый файл, лучше не открывайте его прямо сейчас, сначала сделаем его полегче
carto_sf = st_read("/russia_geojson_wgs84_carto/")
# шейпфайл очень детальный, сильно замедляет работу и будет не очень аккуратно выглядеть
carto <- ms_simplify(carto_sf, keep = 0.04)
plot(carto$geometry)

region_uncommon = region_settl[!tolower(region_settl$region) %in% tolower(carto$full_name),]
cartonames = carto$full_name
carto$full_name = str_replace(carto$full_name, "Еврейская АО", "Еврейская автономная область")
carto$full_name = str_replace(carto$full_name, "АО", "автономный округ")
carto$full_name = str_replace(carto$full_name, "Республика Северная Осетия-Алания", "Республика Северная Осетия - Алания")
carto$full_name = str_replace(carto$full_name, "Ханты-Мансийский автономный округ — Югра", "Ханты-Мансийский автономный округ - Югра")
carto$full_name = str_replace(carto$full_name, "Республика Саха", "Республика Саха (Якутия)")

# агрегируем население по регионам и присоединяем табличку к табл. атрибутов шейпфайла
regions_pop =  group_by(pop_settlement, region) %>% summarise(sum_population = sum(sum))
regions_pop$region_low = tolower(regions_pop$region) # приводим названия в нижний регистр
carto$full_name_low = tolower(carto$full_name)

rusdata_pop = left_join(carto, regions_pop, by = c("full_name_low" = "region_low"))
rusdata_pop = select(rusdata_pop, en_name, full_name, full_name_low, region, sum_population)

# файл с площадью регионов
# http://www.statdata.ru/ploshchad/rossii
area <- read_delim("area.csv", ";", escape_double = FALSE, trim_ws = TRUE)
area$Region_low = tolower(area$Region)
area$area_sq_km = gsub("\\s", "", area$area_sq_km) # исключить пробелы из чисел
area$area_sq_km = as.numeric(area$area_sq_km)

# сверим названия - все совпадают, можно присоединять к шейпфайлу
area_uncommon = area[!area$Region_low %in% carto$full_name_low,]
rusdata_pop = left_join(rusdata_pop, area, by = c("full_name_low" = "Region_low"))

# количество поселений
region_settl$region_low = tolower(region_settl$region)
colnames(region_settl) = c("region", "num_settlements", "region_low")
rusdata_pop = left_join(rusdata_pop, region_settl, by = c("full_name_low" = "region_low"))

# плотность поселений
rusdata_pop$settl_dens = rusdata_pop$num_settlements/rusdata_pop$area_sq_km

# плотность населения
rusdata_pop$pop_dens = rusdata_pop$sum_population/rusdata_pop$area_sq_km

# распределения очень скошенные, придётся проследить за классификацией
ggplot() + geom_histogram(data = rusdata_pop, aes(x = sum_population))
ggplot() + geom_histogram(data = rusdata_pop, aes(x = area_sq_km))
ggplot() + geom_histogram(data = rusdata_pop[rusdata_pop$full_name != "Москва" & rusdata_pop$full_name != "Санкт-Петербург",], aes(x = sum_population/area_sq_km))
ggplot() + geom_histogram(data = rusdata_pop, aes(x = settl_dens))
ggplot() + geom_histogram(data = rusdata_pop, aes(x = pop_dens))

# РИСУЕМ
plot(rusdata_pop["pop_dens"])
laea = st_crs("+proj=laea +lat_0=50 +lon_0=90") # создаём проекцию Lambert azimuthal equal-area projection

pal <- brewer.pal(5, "RdPu") # создаём палитру из 5 цветов
rusdata_pop_laea = st_transform(rusdata_pop, laea) # можно вписать код проекции: 3576 или 5940 тоже подойдут
plot(rusdata_pop_laea["pop_dens"], 
     main = "Population Density in Russian Federation, 2020",
     breaks = "quantile", # классификация нужна для правильного отображения скошенных переменных
     nbreaks = 5, # делим все значения на 5 групп
     pal = pal,
     logz = TRUE)

# логарифмированная шкала
ggplot() + 
  geom_sf(data = rusdata_pop_laea, aes(fill = log10(pop_dens))) +
  scale_fill_gradient(high = "#e34a33", 
                      low = "#fee8c8", 
                      guide = "legend", 
                      labels=c(0.1, 1, 10, 100, 1000), 
                      breaks=c(-1, 0, 1, 2, 3), 
                      name="population density") + # указываю палитру и легенду
  # geom_point(data=population[population$type == "г",], aes(x = long, y = lat), size = 0.5) +
  # coord_map("azequalarea", orientation = c(90, 0, 90)) + # проекция с поворотом на 90 градусов
  theme_void()

# пример с равными интервалами
classIntervals(rusdata_pop$pop_dens, 5, style = "equal")
ggplot() + 
  geom_sf(data = rusdata_pop_laea, aes(fill = pop_dens)) +
  scale_fill_gradient(high = "#e34a33", 
                      low = "#fee8c8", 
                      guide = "legend", 
                      labels=c(966, 1933, 2900, 3867, 4834), 
                      breaks=c(966, 1933, 2900, 3867, 4834), 
                      name="population density") +
  theme_void()

# сохраняем изменённый шейпфайл
st_write(rusdata_pop_laea, dsn = "~/folder", "carto_pop_laea.shp", driver="ESRI Shapefile", layer_options = "ENCODING=UTF-8")

