suppressMessages(library(dplyr))
chk <- function(f, key, tgt, ws){
  d <- readRDS(paste0("data/crosswalks/final/",f,".rds")); cat("\n==",f,"\n")
  for(w in ws){
    s <- d %>% group_by(across(all_of(key))) %>% summarise(s=sum(.data[[w]]), .groups="drop")
    cat(w,": groups",nrow(s)," bad(|s-1|>1e-6):",sum(abs(s$s-1)>1e-6,na.rm=T)," NA:",sum(is.na(s$s)),"range",range(s$s,na.rm=T),"\n")
    cat("  weight range",range(d[[w]],na.rm=T)," NAs",sum(is.na(d[[w]])),"\n")
    b <- s %>% filter(abs(s-1)>1e-6); if(nrow(b)) print(head(b[order(-abs(b$s-1)),],8))
  }
  cat("dups (key,target):",sum(duplicated(d[,c(key,tgt)])),"\n")
  cat("NA target:",sum(is.na(d[[tgt]])),"; nchar target:"); print(table(nchar(d[[tgt]]),useNA="a"))
  cat("nchar src:"); print(table(nchar(d[[key[1]]]),useNA="a"))
}
chk("ags_crosswalks",c("ags","year"),"ags_21",c("pop_cw","area_cw","emp_cw"))
chk("ags_1990_to_2023_crosswalk",c("ags","year"),"ags_2023",c("pop_cw","area_cw"))
chk("ags_1990_to_2025_crosswalk",c("ags","year"),"ags_25",c("pop_cw","area_cw"))
chk("crosswalk_ags_2021_2022_to_2023",c("ags","year"),"ags_2023",c("pop_cw","area_cw"))
chk("crosswalk_ags_2021_to_2023",c("ags_2021"),"ags_2023",c("w_pop","w_area"))
chk("crosswalk_ags_2023_24_to_2025",c("ags","year"),"ags_25",c("pop_cw","area_cw"))
chk("crosswalk_ags_2023_to_2025",c("ags","year"),"ags_25",c("pop_cw","area_cw"))
chk("crosswalk_ags_2024_to_2025",c("ags","year"),"ags_25",c("pop_cw","area_cw"))
chk("cty_crosswalks",c("county_code","year"),"county_code_21",c("pop_cw","area_cw","emp_cw"))
