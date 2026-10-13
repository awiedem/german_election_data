suppressMessages(library(dplyr))
d <- readRDS("data/state_elections/final/state_unharm.rds")
meta <- c("ags","election_year","election_date","state","eligible_voters","number_voters","valid_votes","invalid_votes","turnout")
pc <- setdiff(names(d)[sapply(d,is.numeric)], c(meta))
show <- function(st, yr) { x <- d[d$state==st & d$election_year==yr,]; m <- colMeans(x[,pc], na.rm=TRUE); m <- m[!is.nan(m) & m>0.005]; cat(st, yr, ":", paste(names(sort(m,decreasing=TRUE))[1:min(12,length(m))], round(sort(m,decreasing=TRUE)[1:min(12,length(m))],3), collapse="; "), "\n") }
show("14",1990); show("15",1990); show("16",1990); show("12",1990); show("14",1994);show("15",1994)
x <- d[d$state=="15" & d$election_year %in% c(2011,2016),]; cat("ST FW: freie_wahler nonNA=", sum(!is.na(x$freie_wahler)), " freiewaehler nonNA=", sum(!is.na(x$freiewaehler)), " rows=", nrow(x), "\n")
print(x |> group_by(election_year) |> summarise(fw=mean(freie_wahler,na.rm=T), fw2=mean(freiewaehler,na.rm=T)))
# who else uses freie_wahler in ST
y <- d[d$state=="15",]; print(y |> group_by(election_year) |> summarise(fw=sum(!is.na(freie_wahler)), fw2=sum(!is.na(freiewaehler))) |> as.data.frame())
