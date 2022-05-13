library('dplyr')
library('tidyr')
library('zoo')

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


library("stringr")

news <- read.csv("~/Documents/master's_thesis/coding/parsing/finam/news_filtered.csv")
news$time <- paste(news$article_date, news$article_time)
news$time <- as.POSIXlt(news$time, format="%d.%m.%Y %H:%M")


  
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
  
  for (i in 1:(event_window/60)){
    x <- as.numeric(event_data$time == event_time - 60*i)
    event_data <- cbind(event_data, x)
  }
  
  #y <- as.numeric(event_data$time == event_time)
  #event_data <- cbind(event_data, y)
  
  for (i in 1:(event_window/60)){
    z <- as.numeric(event_data$time == event_time + 60*i)
    event_data <- cbind(event_data, z)
  }
  
  c1 <- 1
  for (i in 1:(event_window/60)){
    c1[i] <- paste0("tau_m", i)
  }
  #c1[(event_window/60)+1] <- 'tau_0'
  c2 <- 1
  for (i in 1:(event_window/60)){
    c2[i] <- paste0("tau_", i)
  }
  
  colnames(event_data) <- c(names, c1, c2)

  return(event_data)
}

AR <- abnormal_return(data = stocks_minute,
                      event_time = news$time[2],
                      estimation_window = 60*60*24*30, # в секундах
                      event_window = 0.5*60*60, # в секундах
                      event_vink = 'rosn')


event_study <- function(AR_data, news_data){
  for (i in 1:nrows(news_data)){
    
  }
}







xnam1 <- paste("tau_m", 1:30, sep="")
#xnam2 <- paste("tau_", 0, sep="")
xnam3 <- paste("tau_", 1:30, sep="")

#formula <- as.formula(paste("AR ~ ", paste(xnam1, collapse= "+"), '+', paste(xnam2, collapse= "+"), '+', paste(xnam3, collapse= "+")))
formula <- as.formula(paste("AR ~ ", paste(xnam1, collapse= "+"), '+', paste(xnam3, collapse= "+")))


mod1 <- lm(formula, AR)

library('coefplot')
coefplot(mod1)
summary(mod1)









