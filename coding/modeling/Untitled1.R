# Библиотеки
{
library('dplyr')
library('tidyr')
library('zoo')
library('coefplot')
library('lubridate')
}

# Подгружаем датасет с котировками
{
stocks_minute <- read.csv("~/Documents/master's_thesis/coding/data/stocks/stocks_minute.csv")
stocks_minute$time <- as.POSIXlt(stocks_minute$time, format="%Y-%m-%d %H:%M:%S")
stocks_minute <- stocks_minute[order(stocks_minute$time),]
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

stocks_minute <- stocks_minute[-c(1:5),]
}

# Подгружаем датасет с новостями
{
news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_filtered.csv")
news$time <- paste(news$article_date, news$article_time)
news$time <- as.POSIXlt(news$time, format="%d.%m.%Y %H:%M")

news$company <- str_replace(news$company, "rosneft", "rosn")
news$company <- str_replace(news$company, "bashneft-ank-ao", "bane")
news$company <- str_replace(news$company, "gazprom-neft", "sibn")
news$company <- str_replace(news$company, "lukoil", "lkoh")
news$company <- str_replace(news$company, "tatneft-3", "tatn")
}

# Функции
{
# Функция, которая для отдельной новости 
# и заданных estimation window и event window считает abnormal return 
# + создаёт набор дамми для последующей регрессии 
abnormal_return <- function(data, event_time, estimation_window, event_window, event_vink){
  
  estimation_data <- data %>%
    filter((time > event_time - (estimation_window*2 + event_window)) & (time < event_time -  event_window))
  
  event_data <- data %>% 
    filter((time > (event_time - event_window)) & (time < event_time + event_window))
  
  estimation <- lm(estimation_data[[event_vink]] ~ estimation_data$imoex)
  a <- coef(estimation)[[1]] # alpha
  b <- coef(estimation)[[2]] # betha
  
  event_data$r_norm <- a + b*event_data$imoex
  event_data$AR <- event_data[[event_vink]] - event_data$r_norm
  
  event_data$company <- event_vink
  
  names <- colnames(event_data)
  
  # lags
  for (i in 1:(event_window/(60*10))){
    x <- as.numeric((event_data$time >= event_time - 10*60*i) & ((event_data$time < event_time - 10*60*(i-1))))
    event_data <- cbind(event_data, x)
  }
  
  #leads
  for (i in 1:(event_window/(60*10))){
    z <- as.numeric((event_data$time < event_time + 10*60*i) & ((event_data$time >= event_time + 10*60*(i-1))))
    event_data <- cbind(event_data, z)
  }
  
  # Переименуем дамми столбцы
  c1 <- 1
  for (i in 1:(event_window/(60*10))){
    c1[i] <- paste0("tau_lag_", i)
  }
  c2 <- 1
  for (i in 1:(event_window/(60*10))){
    c2[i] <- paste0("tau_lead_", i)
  }
  colnames(event_data) <- c(names, c1, c2)

  return(event_data)
}

# Функция, которая пробегается по всем новостям и считает для каждой abnormal return
event_study <- function(AR_data, news_data){
  print(1)
  start_time <- Sys.time()
  event <- abnormal_return(data = AR_data,
                           event_time = news$time[1],
                           estimation_window = 60*60*24*30*24, # в секундах
                           event_window = 6*60*60, # в секундах
                           event_vink = news$company[1])
  end_time <- Sys.time()
  x <- end_time - start_time
  total <- x
  print(x)
  print(total)
  for (i in 2:nrow(news_data)){
    print(i)
    start_time <- Sys.time()
    event_i <- abnormal_return(data = AR_data,
                               event_time = news$time[i],
                               estimation_window = 60*60*24*30*24, # в секундах
                               event_window = 6*60*60, # в секундах
                               event_vink = news$company[i])
    event <- rbind(event, event_i)
    end_time <- Sys.time()
    x <- end_time - start_time
    total <- total + x
    print(x)
    print(total)
  }
  event$wday <- event[["time"]]$wday
  event$hour <- hour(event$time)
  return(event)
}
}

# Применяем заготовленные функции на данные
data <- event_study(stocks_minute, news)

# Поскольку дамми очень много, будем записывать их в регрессию более хитро, заранее заготовим формулу
es_formula <- function(event_window){
  nam1 <- paste("tau_lag_", (event_window*60/10):2, sep="")
  nam2 <- paste("tau_lead_", 1:(event_window*60/10), sep="")
  
  formula <- as.formula(paste("AR ~ ", paste(nam1, collapse= "+"), '+', paste(nam2, collapse= "+"), '+', 'as.factor(company)', '+', 'as.factor(wday)', '+', 'as.factor(hour)'))
  return(formula)
}

# Заготовим список коэффициентов для графика coefplot
coefplot_formula <- function(event_window){
  nam1 <- paste("tau_lag_", (event_window*60/10):1, sep="")
  nam2 <- paste("tau_lead_", 1:(event_window*60/10), sep="")

  lags <- paste(nam1, collapse= ",")
  leads <-  paste(nam2, collapse= ",")
  tau <- paste(lags, leads)
  tau <- strsplit(tau, ",")[[1]]
  return(tau)
}

# Итоговая регрессия и график
mod <- lm(es_formula(6), data=data)
coefplot(mod, 
         title = "Аномальная доходность на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(6),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')

# Заготовим подвыборки для графиков по отдельным типам событий
{
news$event_predupr <- as.logical(news$event_predupr)
news$event_predost <- as.logical(news$event_predost)
news$event_delo <- as.logical(news$event_delo)
news$event_resh <- as.logical(news$event_resh)
news$event_predp <- as.logical(news$event_predp)

news_predupr <- news %>% filter(event_predupr == TRUE)
news_predost <- news %>% filter(event_predost == TRUE)
news_delo <- news %>% filter(event_delo == TRUE)
news_resh <- news %>% filter(event_resh == TRUE)
news_predp <- news %>% filter(event_predp == TRUE)
news_before <- news %>% filter(event_predupr == TRUE | event_predost == TRUE)
news_after <- news %>% filter(event_delo == TRUE | event_resh == TRUE | event_predp == TRUE)
}

# AR в разбивке
{
data_predupr <- event_study(stocks_minute, news_predupr)
data_predost <- event_study(stocks_minute, news_predost)
data_delo <- event_study(stocks_minute, news_delo)
data_resh <- event_study(stocks_minute, news_resh)
data_predp <- event_study(stocks_minute, news_predp)
data_before <- event_study(stocks_minute, news_before)
data_after <- event_study(stocks_minute, news_after)
}

# Модельки в разбивке
{
#mod_predupr <- lm(es_formula(3), data=data_predupr)
mod_predupr <- lm(AR ~ tau_lag_18 + tau_lag_17 + tau_lag_16 + tau_lag_15 + tau_lag_14 + 
                      tau_lag_13 + tau_lag_12 + tau_lag_11 + tau_lag_10 + tau_lag_9 + 
                      tau_lag_8 + tau_lag_7 + tau_lag_6 + tau_lag_5 + tau_lag_4 + 
                      tau_lag_3 + tau_lag_2 + tau_lag_1 + tau_lead_1 + tau_lead_2 + 
                      tau_lead_3 + tau_lead_4 + tau_lead_5 + tau_lead_6 + tau_lead_7 + 
                      tau_lead_8 + tau_lead_9 + tau_lead_10 + tau_lead_11 + tau_lead_12 + 
                      tau_lead_13 + tau_lead_14 + tau_lead_15 + tau_lead_16 + tau_lead_17 + 
                      tau_lead_18 + as.factor(wday) + as.factor(hour), data=data_predupr)
mod_predost <- lm(AR ~ tau_lag_18 + tau_lag_17 + tau_lag_16 + tau_lag_15 + tau_lag_14 + 
                    tau_lag_13 + tau_lag_12 + tau_lag_11 + tau_lag_10 + tau_lag_9 + 
                    tau_lag_8 + tau_lag_7 + tau_lag_6 + tau_lag_5 + tau_lag_4 + 
                    tau_lag_3 + tau_lag_2 + tau_lag_1 + tau_lead_1 + tau_lead_2 + 
                    tau_lead_3 + tau_lead_4 + tau_lead_5 + tau_lead_6 + tau_lead_7 + 
                    tau_lead_8 + tau_lead_9 + tau_lead_10 + tau_lead_11 + tau_lead_12 + 
                    tau_lead_13 + tau_lead_14 + tau_lead_15 + tau_lead_16 + tau_lead_17 + 
                    tau_lead_18 + as.factor(wday) + as.factor(hour), data=data_predost)
mod_delo <- lm(AR ~ tau_lag_18 + tau_lag_17 + tau_lag_16 + tau_lag_15 + tau_lag_14 + 
                 tau_lag_13 + tau_lag_12 + tau_lag_11 + tau_lag_10 + tau_lag_9 + 
                 tau_lag_8 + tau_lag_7 + tau_lag_6 + tau_lag_5 + tau_lag_4 + 
                 tau_lag_3 + tau_lag_2 + tau_lag_1 + tau_lead_1 + tau_lead_2 + 
                 tau_lead_3 + tau_lead_4 + tau_lead_5 + tau_lead_6 + tau_lead_7 + 
                 tau_lead_8 + tau_lead_9 + tau_lead_10 + tau_lead_11 + tau_lead_12 + 
                 tau_lead_13 + tau_lead_14 + tau_lead_15 + tau_lead_16 + tau_lead_17 + 
                 tau_lead_18 + as.factor(wday) + as.factor(hour), data=data_delo)
mod_resh <- lm(AR ~ tau_lag_18 + tau_lag_17 + tau_lag_16 + tau_lag_15 + tau_lag_14 + 
                 tau_lag_13 + tau_lag_12 + tau_lag_11 + tau_lag_10 + tau_lag_9 + 
                 tau_lag_8 + tau_lag_7 + tau_lag_6 + tau_lag_5 + tau_lag_4 + 
                 tau_lag_3 + tau_lag_2 + tau_lag_1 + tau_lead_1 + tau_lead_2 + 
                 tau_lead_3 + tau_lead_4 + tau_lead_5 + tau_lead_6 + tau_lead_7 + 
                 tau_lead_8 + tau_lead_9 + tau_lead_10 + tau_lead_11 + tau_lead_12 + 
                 tau_lead_13 + tau_lead_14 + tau_lead_15 + tau_lead_16 + tau_lead_17 + 
                 tau_lead_18 + as.factor(wday) + as.factor(hour), data=data_resh)
mod_predp <- lm(AR ~ tau_lag_18 + tau_lag_17 + tau_lag_16 + tau_lag_15 + tau_lag_14 + 
                  tau_lag_13 + tau_lag_12 + tau_lag_11 + tau_lag_10 + tau_lag_9 + 
                  tau_lag_8 + tau_lag_7 + tau_lag_6 + tau_lag_5 + tau_lag_4 + 
                  tau_lag_3 + tau_lag_2 + tau_lag_1 + tau_lead_1 + tau_lead_2 + 
                  tau_lead_3 + tau_lead_4 + tau_lead_5 + tau_lead_6 + tau_lead_7 + 
                  tau_lead_8 + tau_lead_9 + tau_lead_10 + tau_lead_11 + tau_lead_12 + 
                  tau_lead_13 + tau_lead_14 + tau_lead_15 + tau_lead_16 + tau_lead_17 + 
                  tau_lead_18 + as.factor(wday) + as.factor(hour), data=data_predp)
mod_before <- lm(AR ~ tau_lag_18 + tau_lag_17 + tau_lag_16 + tau_lag_15 + tau_lag_14 + 
                   tau_lag_13 + tau_lag_12 + tau_lag_11 + tau_lag_10 + tau_lag_9 + 
                   tau_lag_8 + tau_lag_7 + tau_lag_6 + tau_lag_5 + tau_lag_4 + 
                   tau_lag_3 + tau_lag_2 + tau_lag_1 + tau_lead_1 + tau_lead_2 + 
                   tau_lead_3 + tau_lead_4 + tau_lead_5 + tau_lead_6 + tau_lead_7 + 
                   tau_lead_8 + tau_lead_9 + tau_lead_10 + tau_lead_11 + tau_lead_12 + 
                   tau_lead_13 + tau_lead_14 + tau_lead_15 + tau_lead_16 + tau_lead_17 + 
                   tau_lead_18 + as.factor(wday) + as.factor(hour), data=data_before)
mod_after <- lm(es_formula(3), data=data_after)
}

coefplot(mod_predupr, 
         title = "Аномальная доходность для предупреждений на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')
coefplot(mod_predost, 
         title = "Аномальная доходность для предостережений на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')
coefplot(mod_delo, 
         title = "Аномальная доходность для возбуждения дел на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')
coefplot(mod_resh, 
         title = "Аномальная доходность для вынесения решений на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')
coefplot(mod_predp, 
         title = "Аномальная доходность для выдачи предписаний на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')
coefplot(mod_before, 
         title = "Аномальная доходность до доказания вины на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')
coefplot(mod_after, 
         title = "Аномальная доходность после доказания вины на событийном окне (-3; +3) часа",
         xlab = "Аномальная доходность",
         ylab = "Десятиминутные фиксированные эффекты",
         predictors = coefplot_formula(3),
         intercept = FALSE,
         outerCI=0, 
         zeroColor='red')

#write_xlsx(data,"data.xlsx")

