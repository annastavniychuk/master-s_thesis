library(haven)

path = file.path("C:/", "Folder", "dataset.sav")
dataset = read_sav('1584103959_xyQsN8Vb.sav')

library("writexl")
write_xlsx(dataset,"2019.xlsx")

summary(dataset)

a <- unique(dataset[c("mark_gds")])
