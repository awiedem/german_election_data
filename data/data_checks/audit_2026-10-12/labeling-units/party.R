cols <- readRDS(file.path(Sys.getenv("S"),"cols.rds"))
pats <- list(gruene="^(gruene|grune|grüne|b90_gr|b90.*gr|buendnis_90.*|bundnis_90.*|gruene_comb)$", linke="^(linke.*|die_linke|pds|pds_linke|dielinke|die_linke_pds)$", cdu="^(cdu.*|csu|union)$", fw="^(freie_?w.*|fw.*)$", afd="^afd.*$", fdp="^fdp.*$", nonvoter="^(nicht|partei_der_nicht).*", other="^(other|others|sonstige.*|andere.*)$", bsw="^(bsw|bundnis_sahra.*|a_sahra)$", spd="^spd.*$", total="^(total_.*|sum_.*)$")
for (d in unique(cols$ds)) { x <- cols$col[cols$ds==d]
  s <- sapply(pats, function(p) paste(x[grepl(p,x)],collapse="/")); s <- s[nzchar(s)]
  if (length(s)) cat(sprintf("%-30s %s\n", d, paste(names(s),"=",s,collapse="; "))) }
