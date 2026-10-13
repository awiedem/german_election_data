suppressMessages({library(dplyr);library(data.table)})
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
w<-u[,.N,by=winner_party]
cat("distinct winner_party:",nrow(w),"\n")
for(pat in c("^(f\\.?d\\.?p\\.?)$","^gr(ü|u|Ü)ne$|^b(ü|u)ndnis 90|^die gr","^die linke|^linke|^pds|^die\\.?linke","^(cdu)$","^(spd)$")){
 cat("\n",pat,"\n"); print(w[grepl(pat,winner_party,ignore.case=TRUE)][order(-N)])}
cat("\nlong-form names\n"); print(w[grepl("Partei Deutschlands|Demokratische Union|Freie Demokratische|BÜNDNIS 90",winner_party)][order(-N)][1:15])
cat("\nlongest labels\n"); w[,nc:=nchar(winner_party)]; print(w[order(-nc)][1:8])
cat("\nstate of long form:"); print(u[grepl("Sozialdemokratische Partei Deutschlands",winner_party),.N,by=state])
cat("\nnon-party placeholders in winner_party:\n"); print(w[grepl("kein|weitere|sonstige|ohne",winner_party,ignore.case=T)][order(-N)])
print(u[grepl("^kein Wahlvorschlag$|^Weitere Personen",winner_party,ignore.case=T),.N,by=state])
cat("\nparty winner cap:"); print(u[,.(n_ST_empty=sum(winner_party==""&state=="15"),n_ST=sum(state=="15"))])
