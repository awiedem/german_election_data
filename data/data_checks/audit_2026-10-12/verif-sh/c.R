suppressMessages(library(readxl))
for(f in c("2023","2021")){p<-paste0("data/state_elections/raw/Landtagswahlen/Berlin/Berlin_",f,"_AGH_BVV.xlsx")
print(excel_sheets(p))
d<-read_excel(p,sheet="AGH_W2",col_names=FALSE,.name_repair="minimal")
cat(f,dim(d),"\n")
print(as.data.frame(t(d[1:8,])[,]) |> head(80))}
