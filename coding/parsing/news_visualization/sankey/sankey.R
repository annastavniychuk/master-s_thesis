{
library('dplyr')
library('grf')
library('tidyverse')
library('reshape2')
library('networkD3')
library('data.table')
}

#data <- fread("finam/news_dummy_filter.csv")
# data <- fread("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter.csv", header=T)
data <- fread("~/Documents/master's_thesis/coding/parsing/finam/news_dummy_filter_upd.csv", header=T)

{
data <- data %>% mutate(event=case_when(event_delo == TRUE ~ 1,
                                          event_predost == TRUE ~ 2,
                                          event_predp == TRUE ~ 3,
                                          event_resh == TRUE ~ 4, 
                                          event_predupr == TRUE ~ 5,
                                          TRUE ~ 0))
data$event[data$event==0] <- 'Прочее'
data$event[data$event==1] <- 'Дело'
data$event[data$event==2] <- 'Предостережение'
data$event[data$event==3] <- 'Предписание'
data$event[data$event==4] <- 'Решение'
data$event[data$event==5] <- 'Предупреждение'
}

{
data <- data %>% mutate(company_rus=case_when(company == "rosneft" ~ 1,
                                               company =="bashneft-ank-ao" ~ 2,
                                               company == "gazprom-neft" ~ 3,
                                               company == "lukoil" ~ 4,
                                               company == "tatneft-3" ~ 5,
                                               TRUE ~ 0))
data$company_rus[data$company_rus==0] <- 'Прочее'
data$company_rus[data$company_rus==1] <- 'Роснефть'
data$company_rus[data$company_rus==2] <- 'Башнефть'
data$company_rus[data$company_rus==3] <- 'Газпромнефть'
data$company_rus[data$company_rus==4] <- 'Лукойл'
data$company_rus[data$company_rus==5] <- 'Татнефть'
}

{
data$FAS[data$FAS==TRUE] <- 'В новости упоминается ФАС'
data$FAS[data$FAS==FALSE] <- 'В новости не упоминается ФАС'
}

{
data$company_rus <- as.factor(data$company_rus)
data$FAS <- as.factor(data$FAS)
data$event <- as.factor(data$event)
}

links <- data[,list(value=.N, source='Все события на Финам.ру', target=company_rus), by=c('company_rus')]
links <- links[,c('value', 'target', 'source')]

links2 <- data[,list(value=.N, source=company_rus, target=FAS), by=c('company_rus', 'FAS')]
links2 <- links2[,c('value', 'target', 'source')]

links3 <- data[,list(value=.N, source=FAS, target=event), by=c('FAS', 'event')]
links3 <- links3[,c('value', 'target', 'source')]

links <- rbind(links, links2)
links <- rbind(links, links3)

nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

graph <- sankeyNetwork(Links = links, Nodes = nodes, 
                       Source = 'IDsource', Target = 'IDtarget', 
                       Value = 'value', NodeID = 'name', 
                       sinksRight = FALSE, fontSize = 18)
graph


# install.packages('htmlwidgets')
# library('htmlwidgets')
# 
# saveWidget(graph, file="event_sankey1.html")
# 
# 
# install.packages('webshot')
# library('webshot')
# 
# webshot::install_phantomjs()
# webshot("event_sankey1.html", "event_sankey1.pdf")


# cairo_pdf(file = 'event_sankey.pdf', width = 21, height = 20)
# graph
# dev.off()

