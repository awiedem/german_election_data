suppressMessages(library(dplyr))
e<-bundeslaendeR::ltw_elections; print(names(e)[1:20]); print(dim(e)); print(head(as.data.frame(e[,1:14]),3))
print(names(bundeslaendeR::link_gerda)); print(head(bundeslaendeR::link_gerda,3))
