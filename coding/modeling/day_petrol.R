library(fixest)
library(dplyr)

cdu <- read.csv("~/Documents/master's_thesis/coding/preprocessing/cdu.csv", header=TRUE)

{
cdu$Дата <- as.POSIXlt(cdu$Дата, format = '%Y-%m-%d')
cdu$Демпфер <- as.numeric(cdu$Дата > '2019-01-01')

#cdu <- cdu %>% filter(Дата < '2019-01-01')
#cdu <- cdu %>% filter(Дата > '2019-01-01')
  
cdu$`Цена.на.нефть1` <- cdu$`Цена.на.нефть`*cdu$`Курс.доллара`
cdu$Регион <- as.factor(cdu$Регион)
cdu$Компания <- as.factor(cdu$Компания)
cdu$Марка <- as.factor(cdu$Марка)
cdu$Месяц <- as.factor(cdu$Месяц)
cdu$Дата <- as.factor(cdu$Дата)
}

cdu <- cdu %>% 
  group_by(Регион, Компания, Марка) %>% 
  mutate(dЦена = log(Цена/lag(Цена)), dЦена.на.нефть = log(Цена.на.нефть/lag(Цена.на.нефть)), dЦена.за.1.т.бензина = log(Цена.за.1.т.бензина/log(Цена.за.1.т.бензина)))

# cdu1 <- cdu %>% 
#   group_by(Дата) %>% 
#   summarise(Цена = mean(Цена), Цена.на.нефть = mean(Цена.на.нефть1), Цена.за.1.т.бензина = mean(Цена.за.1.т.бензина)) %>% filter(Дата > '2018-01-01')
# 
# cdu1$Дата <- as.POSIXct(cdu1$Дата)

# cdu1$dЦена <- log(cdu1$Цена/lag(cdu1$Цена))
# cdu1$dЦена.на.нефть <- log(cdu1$Цена.на.нефть/lag(cdu1$Цена.на.нефть))
# cdu1$dЦена.за.1.т.бензина <- log(cdu1$Цена.за.1.т.бензина/lag(cdu1$Цена.за.1.т.бензина))

# cdu1$dЦена <- cdu1$Цена/lag(cdu1$Цена)
# cdu1$dЦена.на.нефть <- cdu1$Цена.на.нефть/lag(cdu1$Цена.на.нефть)
# cdu1$dЦена.за.1.т.бензина <- cdu1$Цена.за.1.т.бензина/lag(cdu1$Цена.за.1.т.бензина)
# 
# ddЦена <- diff(cdu1$Цена)
# ddЦена.на.нефть <- diff(cdu1$Цена.на.нефть)
# ddЦена.за.1.т.бензина <- diff(cdu1$Цена.за.1.т.бензина)
# 
# library(ggplot2)
# 
# ggplot(cdu1) + 
#   geom_line(aes(x= Дата, y = Цена, group=1, colour = "Цена")) + 
#   geom_line(aes(x= Дата, y = Цена.на.нефть, group=2, colour = "Цена.на.нефть")) + 
#   geom_line(aes(x= Дата, y = Цена.за.1.т.бензина, group=3, colour = "Цена.за.1.т.бензина"))
# 
# ggplot(cdu1) + 
#   geom_line(aes(x= Дата, y = dЦена, group=1, colour = "Цена")) + 
#   geom_line(aes(x= Дата, y = dЦена.на.нефть, group=2, colour = "Цена.на.нефть")) + 
#   geom_line(aes(x= Дата, y = dЦена.за.1.т.бензина, group=3, colour = "Цена.за.1.т.бензина"))
# 
# cdu2 <- na.omit(cdu1)
# 
# ggplot(cdu2) + 
#   geom_line(aes(x= Дата, y = dЦена, group=1, colour = "Цена")) + 
#   geom_line(aes(x= Дата, y = dЦена.на.нефть, group=2, colour = "Цена.на.нефть")) + 
#   geom_line(aes(x= Дата, y = dЦена.за.1.т.бензина, group=3, colour = "Цена.за.1.т.бензина"))
# 
# ggplot() + 
#   geom_line(aes(y = ddЦена, group=1, colour = "Цена")) + 
#   geom_line(aes(y = ddЦена.на.нефть, group=2, colour = "Цена.на.нефть")) + 
#   geom_line(aes(y = ddЦена.за.1.т.бензина, group=3, colour = "Цена.за.1.т.бензина"))
# 
# plot(ddЦена, type = "l", col = 'red')
# plot(ddЦена.на.нефть, type = "l", col = 'blue')
# plot(ddЦена.за.1.т.бензина, type = "l", col = 'green')
# 
# cddu <- as.data.frame(cbind(ddЦена, ddЦена.на.нефть, ddЦена.за.1.т.бензина))
# 
# ggplot(cddu) + 
#   geom_line(aes(x= c(1:728), y = ddЦена, group=1, colour = "Цена")) + 
#   #geom_line(aes(x= c(1:728), y = ddЦена.на.нефть, group=2, colour = "Цена.на.нефть")) + 
#   geom_line(aes(x= c(1:728), y = ddЦена.за.1.т.бензина, group=3, colour = "Цена.за.1.т.бензина"))
# 
# ggplot(cdu2) + 
#   geom_line(aes(x= Дата, y = Цена, group=1, colour = "Цена")) + 
#   geom_line(aes(x= Дата, y = Цена.на.нефть, group=2, colour = "Цена.на.нефть")) + 
#   geom_line(aes(x= Дата, y = Цена.за.1.т.бензина, group=3, colour = "Цена.за.1.т.бензина"))
# 
# cor(cdu2$dЦена.за.1.т.бензина,cdu2$dЦена.на.нефть)
# cor(cdu2$Цена.за.1.т.бензина,cdu2$Цена.на.нефть)


model1_1 <- feols(dЦена ~  `dЦена.на.нефть` + Марка + `dЦена.за.1.т.бензина` + event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model1_1)
etable(model1_1, tex = TRUE)
coefplot_predupr_petrol <- coefplot(model1_1, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'),
                                    group = list(`До предупреждения` = 1:6, `После предупреждения` = 8:14),
                                    main = "Фиксированные эффекты события",
                                    zero.par = list(col = "red", lwd = 2)) 

model1_2 <- feols(Цена ~  `Цена.на.нефть` + Марка + event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model1_2)
etable(model1_2, tex = TRUE)
coefplot_predupr <- coefplot(model1_2, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'),
                             group = list(`До предупреждения` = 1:7, `После предупреждения` = 9:15), 
                             ylab = 'Цена бензина, руб./л',
                             main = "Фиксированные эффекты события",
                             zero.par = list(col = "red", lwd = 2)) 

cdu$nДемпфер <- 1 - cdu$Демпфер

#

model1_3 <- feols(dЦена ~  `dЦена.на.нефть` + `dЦена.за.1.т.бензина` + Демпфер + I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка + event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] | Регион + Компания + Дата, 
                  data = cdu, 
                  cluster = c("Регион", "Компания"), 
                  panel.id = c("Дата", "Компания"))
summary(model1_3)
etable(model1_3)
etable(model1_3, tex = TRUE)
coefplot_predupr_petrol_str <- coefplot(model1_3, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина', 'Демпфер'),
                                    group = list(`До предупреждения` = 1:6, `После предупреждения` = 8:14),
                                    main = "Фиксированные эффекты события",
                                    zero.par = list(col = "red", lwd = 2)) 

model2_1 <- feols(Цена ~  `Цена.на.нефть` + Марка + `Цена.за.1.т.бензина` + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model2_1)
coefplot_predost_petrol <- coefplot(model2_1, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
         group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model2_2 <- feols(Цена ~  `Цена.на.нефть` + Марка + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model2_2)
coefplot_predost <- coefplot(model2_2, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
                             group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
                             ylab = 'Цена бензина, руб./л',
                             main = "Фиксированные эффекты события", 
                             zero.par = list(col = "red", lwd = 2))

model2_3 <- feols(dЦена ~  `dЦена.на.нефть` + `dЦена.за.1.т.бензина` + Демпфер + I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model2_3)
coefplot_predost_petrol_str <- coefplot(model2_3, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина', 'Демпфер'), 
                             group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
                             ylab = 'Цена бензина, руб./л',
                             main = "Фиксированные эффекты события", 
                             zero.par = list(col = "red", lwd = 2))

model3_1 <- feols(Цена ~  `Цена.на.нефть` + Марка + `Цена.за.1.т.бензина` + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model3_1)
coefplot_delo_petrol <- coefplot(model3_1, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
         group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model3_2 <- feols(Цена ~  `Цена.на.нефть` + Марка + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model3_2)
coefplot_delo <- coefplot(model3_2, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
                          group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
                          ylab = 'Цена бензина, руб./л',
                          main = "Фиксированные эффекты события", 
                          zero.par = list(col = "red", lwd = 2))

model3_3 <- feols(dЦена ~  `dЦена.на.нефть` + `dЦена.за.1.т.бензина` + Демпфер + I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model3_3)
etable(model3_3)
coefplot_delo_petrol_str <- coefplot(model3_3, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина', 'Демпфер'), 
                          group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
                          ylab = 'Цена бензина, руб./л',
                          main = "Фиксированные эффекты события", 
                          zero.par = list(col = "red", lwd = 2))

model4_1 <- feols(Цена ~  `Цена.на.нефть` + Марка + `Цена.за.1.т.бензина` + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model4_1)
coefplot_resh_petrol <- coefplot(model4_1, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
         group = list(`До вынесения решения` = 1:6, `После вынесения решения` = 8:14),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model4_2 <- feols(Цена ~  `Цена.на.нефть` + Марка + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model4_2)
coefplot_resh <- coefplot(model4_2, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
         group = list(`До вынесения решения` = 1:7, `После вынесения решения` = 9:15),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model4_3 <- feols(dЦена ~  `dЦена.на.нефть` + `dЦена.за.1.т.бензина` + Демпфер + I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model4_3)
etable(model4_3)
coefplot_resh_petrol_str <- coefplot(model4_3, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина', 'Демпфер'), 
                                 group = list(`До вынесения решения` = 1:6, `После вынесения решения` = 8:14),
                                 ylab = 'Цена бензина, руб./л',
                                 main = "Фиксированные эффекты события", 
                                 zero.par = list(col = "red", lwd = 2))

model5_1 <- feols(Цена ~  `Цена.на.нефть` + Марка + `Цена.за.1.т.бензина` + event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model5_1)
coefplot_predp_petrol <- coefplot(model5_1, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
         group = list(`До выдачи предписания` = 1:7, `После выдачи предписания` = 9:14),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model5_2 <- feols(Цена ~  `Цена.на.нефть` + Марка + event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model5_2)
coefplot_predp <- coefplot(model5_2, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
         group = list(`До выдачи предписания` = 1:7, `После выдачи предписания` = 9:14),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model5_3 <- feols(dЦена ~  `dЦена.на.нефть` + `dЦена.за.1.т.бензина` + Демпфер + I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка + event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Компания + Дата, data = cdu)
summary(model5_3)
coefplot_predp_petrol_str <- coefplot(model5_3, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина', 'Демпфер'), 
                                  group = list(`До выдачи предписания` = 1:7, `После выдачи предписания` = 9:14),
                                  ylab = 'Цена бензина, руб./л',
                                  main = "Фиксированные эффекты события", 
                                  zero.par = list(col = "red", lwd = 2))

model_sum <- feols(dЦена ~ I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка | Регион + Компания + Дата, 
                   data = cdu, 
                   cluster = c("Регион", "Компания"), 
                   panel.id = c("Дата", "Компания"))
summary(model_sum)
etable(model_s)

model_sum_lm <- lm(dЦена ~ I((1-Демпфер)*`dЦена.на.нефть`) + Демпфер*`dЦена.за.1.т.бензина` + Марка + Регион + Компания + Дата, data = cdu)
summary(model_sum_lm)

# model6 <- feols(Цена ~  `Цена.на.нефть` + Марка + `Цена.за.1.т.бензина`+ 
#                       event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] + 
#                       event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] + 
#                       event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] + 
#                       event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] + 
#                       event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Компания + Дата, data = cdu)
# summary(model6)
# coefplot(model6, drop = c('Цена.на.нефть', 'Марка', 'Цена.за.1.т.бензина'), 
#          group = list(`До предупреждения` = 1:6, `После предупреждения` = 8:14),
#          ylab = 'Цена бензина, руб./л',
#          main = "Фиксированные эффекты события", 
#          zero.par = list(col = "red", lwd = 2))

cairo_pdf(file = 'coefplot_predupr_petrol.pdf', width = 10, height = 7)
coefplot_predupr_petrol
dev.off()

cairo_pdf(file = 'coefplot_predupr.pdf', width = 10, height = 7)
coefplot_predupr
dev.off()

cairo_pdf(file = 'coefplot_predost.pdf', width = 10, height = 7)
coefplot_predost
dev.off()

cairo_pdf(file = 'coefplot_delo.pdf', width = 10, height = 7)
coefplot_delo
dev.off()

cairo_pdf(file = 'coefplot_resh_petrol.pdf', width = 10, height = 7)
coefplot_resh_petrol
dev.off()

cairo_pdf(file = 'coefplot_resh.pdf', width = 10, height = 7)
coefplot_resh
dev.off()

cairo_pdf(file = 'coefplot_predp_petrol.pdf', width = 10, height = 7)
coefplot_predp_petrol
dev.off()

cairo_pdf(file = 'coefplot_predp.pdf', width = 10, height = 7)
coefplot_predp
dev.off()

