suppressMessages(library(readxl))
x <- read_excel("data/municipal_elections/raw/schleswig_holstein/Originals/sh_gemeindewahlen_2013.xlsx", n_max=3, .name_repair="minimal")
print(names(x)); print(x[1:2,1:12])
