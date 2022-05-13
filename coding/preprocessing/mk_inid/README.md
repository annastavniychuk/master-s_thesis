# Мастер-класс от ИНИД "Как построить карту, используя R и QGIS"
## Материалы с мастер-класса 18.03.2021

Запись можно посмотреть здесь: https://www.youtube.com/watch?v=gauae5HXZhw

##  Источники данных
Датасет по населению России: https://data-in.ru/data-catalog/datasets/160/ 
Код написан под версию от 2 марта 2021

Границы субъектов РФ: https://obulantsev.carto.com/tables/russia_geojson_wgs84/public
Формат - shapefile (SHP)

Площадь регионов: http://www.statdata.ru/ploshchad/rossii подготовленный файл называется area.csv

## Файлы, созданные в ходе работы:
settlement_unique_pop_coords.csv - обработанный датасет

Папка carto_pop_laea - шейпфайл, совмещённый с данными по численности населения (численность населения, агрегированная по субъектам федерации)

Файл с кодом: MK_code_public.R
