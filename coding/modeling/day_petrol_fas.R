library(fixest)
#library(dplyr)

fas <- read.csv("~/Documents/master's_thesis/coding/preprocessing/fas.csv", header=TRUE)

{
fas$Дата <- as.POSIXlt(fas$Дата, format = '%Y-%m-%d')
fas$Демпфер <- as.numeric(fas$Дата > '2019-01-01')
#fas1 <- fas %>% filter(Демпфер==1)
fas$`Цена.на.нефть` <- fas$`Цена.на.нефть`*fas$`Курс.доллара`
fas$Регион <- as.factor(fas$Регион)
fas$Тип.топлива <- as.factor(fas$Тип.топлива)
fas$year_month <- as.factor(paste(fas$year, fas$month))
fas$Дата <- as.factor(fas$Дата)
}

model1_1 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + `Тип.топлива` + `Цена.за.1.т.бензина` + event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model1_1)
#etable(model1_1, tex = TRUE)
etable(model1_1)
coefplot_predupr_petrol <- coefplot(model1_1, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'),
                                    group = list(`До предупреждения` = 1:6, `После предупреждения` = 8:14),
                                    main = "Фиксированные эффекты события",
                                    zero.par = list(col = "red", lwd = 2)) 

model1_2 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model1_2)
#etable(model1_2, tex = TRUE)
etable(model1_2)
coefplot_predupr <- coefplot(model1_2, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'),
                             group = list(`До предупреждения` = 1:7, `После предупреждения` = 9:15), 
                             ylab = 'Цена бензина, руб./л',
                             main = "Фиксированные эффекты события",
                             zero.par = list(col = "red", lwd = 2)) 

model1_3 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + `Цена.за.1.т.бензина` + Демпфер + Демпфер*`Цена.на.нефть` + Демпфер*`Цена.за.1.т.бензина` + `Тип.топлива` + event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model1_3)
#etable(model1_3, tex = TRUE)
etable(model1_3)
coefplot_predupr_petrol_str <- coefplot(model1_3, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина', 'Демпфер'),
                                    group = list(`До предупреждения` = 1:6, `После предупреждения` = 8:14),
                                    main = "Фиксированные эффекты события",
                                    zero.par = list(col = "red", lwd = 2)) 

model2_1 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + `Цена.за.1.т.бензина` + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model2_1)
coefplot_predost_petrol <- coefplot(model2_1, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
         group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model2_2 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model2_2)
coefplot_predost <- coefplot(model2_2, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
                             group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
                             ylab = 'Цена бензина, руб./л',
                             main = "Фиксированные эффекты события", 
                             zero.par = list(col = "red", lwd = 2))

model2_3 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + `Цена.за.1.т.бензина`+ Демпфер + Демпфер*`Цена.на.нефть` + Демпфер*`Цена.за.1.т.бензина` + Тип.топлива + event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model2_3)
coefplot_predost_petrol_str <- coefplot(model2_3, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина', 'Демпфер'), 
                             group = list(`До предостережения` = 1:6, `После предостережения` = 8:11),
                             ylab = 'Цена бензина, руб./л',
                             main = "Фиксированные эффекты события", 
                             zero.par = list(col = "red", lwd = 2))

model3_1 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + `Цена.за.1.т.бензина` + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model3_1)
coefplot_delo_petrol <- coefplot(model3_1, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
         group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model3_2 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model3_1)
coefplot_delo <- coefplot(model3_1, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
                          group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
                          ylab = 'Цена бензина, руб./л',
                          main = "Фиксированные эффекты события", 
                          zero.par = list(col = "red", lwd = 2))

model3_3 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + `Цена.за.1.т.бензина` + Демпфер + Демпфер*`Цена.на.нефть` + Демпфер*`Цена.за.1.т.бензина` + Тип.топлива + event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model3_3)
coefplot_delo_petrol_str <- coefplot(model3_3, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина', 'Демпфер'), 
                                 group = list(`До возбуждения дела` = 1:5, `После возбуждения дела` = 7:11),
                                 ylab = 'Цена бензина, руб./л',
                                 main = "Фиксированные эффекты события", 
                                 zero.par = list(col = "red", lwd = 2))

model4_1 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + `Цена.за.1.т.бензина` + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model4_1)
coefplot_resh_petrol <- coefplot(model4_1, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
         group = list(`До вынесения решения` = 1:6, `После вынесения решения` = 8:14),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model4_2 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model4_2)
coefplot_resh <- coefplot(model4_2, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
         group = list(`До вынесения решения` = 1:7, `После вынесения решения` = 9:15),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model4_3 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + `Цена.за.1.т.бензина` + Демпфер + Демпфер*`Цена.на.нефть` + Демпфер*`Цена.за.1.т.бензина` + Тип.топлива + event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model4_3)
coefplot_resh_petrol_str <- coefplot(model4_3, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина', 'Демпфер'), 
                                 group = list(`До вынесения решения` = 1:6, `После вынесения решения` = 8:14),
                                 ylab = 'Цена бензина, руб./л',
                                 main = "Фиксированные эффекты события", 
                                 zero.par = list(col = "red", lwd = 2))

model5_1 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + `Цена.за.1.т.бензина` + event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model5_1)
coefplot_predp_petrol <- coefplot(model5_1, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
         group = list(`До выдачи предписания` = 1:7, `После выдачи предписания` = 9:14),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model5_2 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + Тип.топлива + event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model5_2)
coefplot_predp <- coefplot(model5_2, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
         group = list(`До выдачи предписания` = 1:7, `После выдачи предписания` = 9:14),
         ylab = 'Цена бензина, руб./л',
         main = "Фиксированные эффекты события", 
         zero.par = list(col = "red", lwd = 2))

model5_3 <- feols(Cредняя_Розничная_цена ~  `Цена.на.нефть` + `Цена.за.1.т.бензина` + Демпфер + Демпфер*`Цена.на.нефть` + Демпфер*`Цена.за.1.т.бензина` + Тип.топлива + event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Винк + Дата, data = fas)
summary(model5_3)
coefplot_predp_petrol_str <- coefplot(model5_3, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина', 'Демпфер'), 
                                  group = list(`До выдачи предписания` = 1:7, `После выдачи предписания` = 9:14),
                                  ylab = 'Цена бензина, руб./л',
                                  main = "Фиксированные эффекты события", 
                                  zero.par = list(col = "red", lwd = 2))

# model6 <- feols(Цена ~  `Цена.на.нефть` + Тип.топлива + `Цена.за.1.т.бензина`+ 
#                       event_predupr_lag.[7:1] + event_predupr + event_predupr_lead.[1:7] + 
#                       event_predost_lag.[7:1] + event_predost + event_predost_lead.[1:7] + 
#                       event_delo_lag.[7:1] + event_delo + event_delo_lead.[1:7] + 
#                       event_resh_lag.[7:1] + event_resh + event_resh_lead.[1:7] + 
#                       event_predp_lag.[7:1] + event_predp + event_predp_lead.[1:7] | Регион + Винк + Дата, data = fas)
# summary(model6)
# coefplot(model6, drop = c('Цена.на.нефть', 'Тип.топлива', 'Цена.за.1.т.бензина'), 
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

