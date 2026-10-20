## Builds the 2006 rows of data/state_elections/metadata/st_postal_pool_members.csv
## (Sachsen-Anhalt Landtagswahl 2006, pooled Briefwahlbezirke).
##
## The source books each pooled postal district on the Kreis and labels it with
## the Verwaltungsgemeinschaft (VG) it serves; its Wahlbezirk number starts with
## that VG's Gemeindeverband key (510100 = VG Coswig (Anhalt), key 5101). The VG
## members come from the Destatis Gemeindeverzeichnis, Gebietsstand 31.12.2005
## (data/covars_municipality/raw/municipality_sizes/31122005_Auszug_GV.xlsx),
## the last edition before the election of 26.03.2006; all 1,048 Gemeinden of
## the election file are in it under the same AGS.
##
## A pool's members are the units its Wahlbezirk key and label name, restricted
## to the Gemeinde parts of the pool's Landtagswahlkreis (a Briefwahlbezirk
## counts the ballots of one Wahlkreis), without the Gemeinden that count their
## own postal votes there (8-digit AGS, Wahllokalart B). Where the key is not a
## VG (verbandsfreie Städte, labels naming several units) `spec` below lists
## the units the label names; `inferred` adds VG Laucha-Schwarzeiche, which no
## label names, to the Bad Lauchstädt pool. A Gemeinde part that no pool of its
## Wahlkreis names (its VG's pool sits in another Wahlkreis, or its VG has none:
## 12 parts) is listed in `shared_ags` of every pool of its Wahlkreis and Kreis; the
## source rows of each Wahlkreis add up exactly to the official Wahlkreis
## result, so its postal ballots are in one of those pools, but the source does
## not say which. 01b splits its electorate over them in proportion to the
## electorate each pool names, so it receives their average rate.
##
## Run from the repository root:  Rscript review/st_postal_pools_2026-10-19/st_postal_pool_members_build_2006.R
pacman::p_load(readxl, data.table, here)
here::i_am("review/st_postal_pools_2026-10-19/st_postal_pool_members_build_2006.R")

st_clean <- function(x) trimws(gsub("\\s+", " ", x))

## --- Gemeindeverzeichnis 31.12.2005, Sachsen-Anhalt ---
gv <- as.data.table(read_excel(here("data", "covars_municipality", "raw", "municipality_sizes",
                                    "31122005_Auszug_GV.xlsx"),
                               sheet = 2, col_names = FALSE, col_types = "text",
                               .name_repair = "minimal")[, 1:8])
setnames(gv, c("satz", "tk", "land", "rb", "kr", "vb", "gem", "name"))
gv <- gv[land == "15"]
vg <- gv[satz == "50", .(kreis = paste0(land, rb, kr), vb, vg_name = name)]
gm <- gv[satz == "60", .(ags = paste0(land, rb, kr, gem), kreis = paste0(land, rb, kr),
                         vb, name)]
gm <- vg[gm, on = .(kreis, vb)]
stopifnot(!anyDuplicated(gm$ags), !anyDuplicated(vg[, paste(kreis, vb)]))
## One VG membership of the election's Gemeinden changed between the GV date and
## election day: Storkau (Elbe) moved from VG Arneburg-Goldbeck to VG Tangermünde
## on 12.01.2006 (de.wikipedia "Storkau (Elbe)"; the StaLA's Statistischer Bericht
## A I 2 hj 2/06 lists it under VG Tangermünde at 31.12.2006). The other
## differences from that report fall after the election (VG Leuna-Kötzschau,
## formed 14.06.2006; Förderstedt, whose Brumby and Glöthe are still separate
## Gemeinden in the election file) or move a Gemeinde between VGs served by the
## same pool of its Wahlkreis (Nempitz, Brachstedt).
stopifnot(gm[ags == "15363115", vb] == "6315")
gm[ags == "15363115", `:=`(vb = "6313", vg_name = "Tangermünde")]

## --- 2006 source ---
raw <- read_excel(here("data", "state_elections", "raw", "Landtagswahlen", "Sachsen-Anhalt",
                       "Sachsen-Anhalt_2006_Landtagswahl.xlsx"),
                  sheet = 1, col_names = FALSE, col_types = "text", .name_repair = "minimal")
d <- as.data.table(raw[7:nrow(raw), c(1, 3, 4, 6, 7, 8, 9)])
setnames(d, c("lwk", "code", "label", "wbz", "lokal", "ev", "voters"))
d[, (names(d)) := lapply(.SD, st_clean)]
d[, `:=`(ev = suppressWarnings(as.numeric(ev)), voters = as.numeric(voters))]
gem  <- d[grepl("^15\\d{6}$", code)]
pool <- d[!grepl("^15\\d{6}$", code) & grepl("^15[0-9]*$", code)]
stopifnot(all(pool$lokal == "B"), all(nchar(pool$code) == 5),
          all(gem$code %in% gm$ags))

## Gemeinde parts per Wahlkreis (electorate from the Urne rows) and the parts
## that count their own postal votes.
own_parts <- unique(gem[lokal == "B", .(lwk, ags = code)])
parts <- gem[lokal == "U", .(ev = sum(ev)), by = .(lwk, ags = code)]
parts <- gm[, .(ags, kreis, vb, vg_name, name)][parts, on = "ags"]
parts[own_parts, own := TRUE, on = .(lwk, ags)][is.na(own), own := FALSE]
stopifnot(nrow(own_parts[!parts, on = .(lwk, ags)]) == 0)

## One pool per Kreis x Wahlbezirk x label (several source rows can share it);
## no such key recurs in another Wahlkreis.
pools <- pool[, .(pool_voters = sum(voters), rows = .N), by = .(code, wbz, label, lwk)]
stopifnot(!anyDuplicated(pools[, paste(code, wbz, label)]))

## Units each pool's label names where the Wahlbezirk key is not (only) a VG:
## "VB:<key>" a VG of the GV, "G:<name>" a Gemeinde (name before the comma).
spec <- list(
  "530001" = c("G:Alsleben (Saale)", "G:Bernburg (Saale)", "G:Gerbitz", "G:Gröna", "G:Güsten",
               "G:Ilberstedt", "G:Latdorf", "G:Neugattersleben", "G:Nienburg (Saale)",
               "G:Plötzkau", "G:Pobzig", "G:Schackstedt", "G:Wedlitz"),
  "530002" = c("G:Alsleben (Saale)", "G:Bernburg (Saale)", "G:Gerbitz", "G:Gröna", "G:Güsten",
               "G:Ilberstedt", "G:Latdorf", "G:Neugattersleben", "G:Nienburg (Saale)",
               "G:Plötzkau", "G:Pobzig", "G:Schackstedt", "G:Wedlitz"),
  "530500" = c("VB:5305", "G:Könnern"),
  "595100" = c("G:Aken (Elbe)", "VB:5905"),
  "710800" = c("VB:7108", "VB:7111"),      # Gräfenhainichen = seat of VG Tor zur Dübener Heide
  "711600" = c("VB:7116", "VB:7118"),      # Bad Schmiedeberg = seat of VG Kurregion Elbe-Heideland
  "715301" = c("G:Wittenberg"),
  "715302" = c("G:Wittenberg"),
  "600400" = c("VB:6004", "G:Benndorf"),
  "601100" = c("VB:6011", "VB:6004"),      # "ML/Gerbstedt": VG Gerbstedt's part of WK 33
  "610100" = c("VB:6101", "VB:6103", "G:Schkopau"),
  "651100" = c("VB:6511", "VB:6507", "VB:6501", "VB:6509"),
  "651001" = c("VB:6510", "G:Kabelsketal"),
  "661000" = c("VB:6610", "G:Sangerhausen"),
  "660601" = c("G:Sangerhausen"),
  "660602" = c("G:Sangerhausen"),
  "570800" = c("VB:5708", "G:Huy"),
  "575101" = c("G:Halberstadt"),           # counts its own postal votes too; this pool is its
  "570300" = c("VB:5703", "VB:5709"),
  "581200" = c("G:Elbe-Parey", "VB:5812"),
  "581400" = c("VB:5814", "G:Gladau", "G:Paplitz", "G:Tucheim"),
  "631300" = c("VB:6313", "VB:6312"),
  "625500" = c("G:Niedere Börde"),
  "705101" = c("G:Salzwedel"),
  "705102" = c("G:Salzwedel")
)

## Units no label names whose postal ballots the data place in one pool.
## VG Laucha-Schwarzeiche (Delitz am Berge, Schafstädt; key 6104) has no pool in
## Wahlkreis 41. At the Bundestagswahl of 18.09.2005 the two ran a joint postal
## board of their own (Briefwahlzugehörigkeit 04: 164 postal voters for 176
## Wahlschein holders, BTW05_Zweitstimmen_Wahlbezirke.txt). In 2006 a pool holds
## about 0.47 times its members' 2005 Wahlschein holders (73 pools without shared
## parts, p10-p90 0.43-0.54). The Bad Lauchstädt pool holds 0.73 on its own
## members (381 voters, 135 above that rate) and 0.54 with these two, whose
## Wahlschein holders predict about 83; Oberes Geiseltal (0.52), Weida-Land (0.51)
## and Querfurt's own postal district (0.51) show no excess. Schafstädt had been
## put into VG Bad Lauchstädt when VG Laucha-Schwarzeiche was dissolved on
## 22.03.2005 (restored by the Verwaltungsgericht Halle on 05.04.2005); both
## joined Bad Lauchstädt on 01.01.2008. No document names the district: this is
## an inference (Vincent's decision, 2026-10-20).
inferred <- list("611400" = "VB:6104")

base <- function(x) sub(",.*$", "", x)
res <- list()
for (i in seq_len(nrow(pools))) {
  p <- pools[i]
  pw <- parts[lwk == p$lwk & kreis == p$code]
  units <- spec[[p$wbz]]
  if (is.null(units)) {
    key <- substr(p$wbz, 1, 4)
    stopifnot(nrow(vg[kreis == p$code & vb == key]) == 1)
    units <- paste0("VB:", key)
  }
  mem <- character(0); how <- character(0)
  for (u in units) {
    typ <- sub(":.*", "", u); nm <- sub("^[A-Z]+:", "", u)
    if (typ == "VB") {
      stopifnot(nrow(vg[kreis == p$code & vb == nm]) == 1)
      hit <- pw[vb == nm & !own]
      if (!nrow(hit)) stop(sprintf("%s %s: VG %s has no Gemeinde in WK %s", p$code, p$wbz, nm, p$lwk))
      how <- c(how, sprintf("VG %s (%s)", vg[kreis == p$code & vb == nm]$vg_name, nm))
    } else {
      hit <- pw[base(name) == nm | startsWith(base(name), paste0(nm, " ")) |
                  startsWith(name, paste0(nm, ", "))]
      if (nrow(hit) != 1) stop(sprintf("%s %s: '%s' matched %d", p$code, p$wbz, nm, nrow(hit)))
      how <- c(how, hit$name)
    }
    mem <- c(mem, hit$ags)
  }
  how_inf <- character(0)
  for (u in inferred[[p$wbz]]) {
    nm <- sub("^VB:", "", u)
    stopifnot(nrow(vg[kreis == p$code & vb == nm]) == 1)
    hit <- pw[vb == nm & !own]
    if (!nrow(hit)) stop(sprintf("%s %s: inferred VG %s has no Gemeinde in WK %s", p$code, p$wbz, nm, p$lwk))
    mem <- c(mem, hit$ags)
    how_inf <- c(how_inf, sprintf("VG %s (%s)", vg[kreis == p$code & vb == nm]$vg_name, nm))
  }
  stopifnot(!anyDuplicated(mem))
  res[[i]] <- data.table(i = i, ags = mem,
                         basis = paste0(if (is.null(spec[[p$wbz]])) "Wahlbezirk key = " else "label names ",
                                        paste(how, collapse = " + "),
                                        if (length(how_inf)) paste0("; plus ", paste(how_inf, collapse = " + "),
                                          ", which has no pool: inferred from the pool's size against the",
                                          " members' 2005 Wahlschein holders")))
}
mem <- rbindlist(res)

## Gemeinde parts no pool of their Wahlkreis names share every pool of their
## Wahlkreis and Kreis.
cover <- unique(merge(mem, pools[, .(i = seq_len(.N), lwk)], by = "i")[, .(lwk, ags)])
orph <- parts[own == FALSE][!cover, on = .(lwk, ags)]
stopifnot(nrow(orph) == 12)          # 12 Gemeinde parts, listed in the HANDOVER
orph_mem <- rbindlist(lapply(seq_len(nrow(orph)), function(k) {
  o <- orph[k]
  ii <- which(pools$lwk == o$lwk & pools$code == o$kreis)
  stopifnot(length(ii) > 0)
  data.table(i = ii, ags = o$ags)
}))
named <- mem[, .(i, ags)]

## Every Gemeinde part of a Wahlkreis that has a pool is now served, and no
## pool reaches outside its Wahlkreis and Kreis or into a Gemeinde that counts
## its own postal votes there (Halberstadt excepted: the pool is labelled
## with it).
chk <- merge(rbind(named, orph_mem), pools[, .(i = seq_len(.N), lwk, code)], by = "i")
chk <- merge(chk, parts[, .(lwk, ags, kreis, own)], by = c("lwk", "ags"), all.x = TRUE)
stopifnot(!anyNA(chk$kreis), all(chk$kreis == chk$code),
          all(!chk$own | chk$ags == "15357017"))
stopifnot(nrow(parts[own == FALSE][!unique(chk[, .(lwk, ags)]), on = .(lwk, ags)]) == 0)

out <- pools[, .(election_year = "2006", pool_code = code, wahlbezirk = wbz,
                 landtagswahlkreis = lwk, pool_label = label,
                 pool_voters = as.character(pool_voters), i = seq_len(.N))]
nm_of <- setNames(gm$name, gm$ags)
basis_of <- setNames(sapply(res, function(r) r$basis[1]), sapply(res, function(r) r$i[1]))
out[, member_ags := sapply(i, function(k) paste(named[i == k]$ags, collapse = ";"))]
out[, member_names := sapply(i, function(k) paste(nm_of[named[i == k]$ags], collapse = "; "))]
out[, shared_ags := sapply(i, function(k) paste(orph_mem[i == k]$ags, collapse = ";"))]
out[, basis := sapply(i, function(k) {
  b <- basis_of[[as.character(k)]]
  o <- orph_mem[i == k]$ags
  if (length(o)) b <- paste0(b, "; shared with the other pools of the Wahlkreis and Kreis: ",
                             paste(nm_of[o], collapse = ", "), " (no pool names them)")
  paste0(b, "; GV 31.12.2005")
})]
out[, i := NULL]

pins_f <- here("data", "state_elections", "metadata", "st_postal_pool_members.csv")
pins <- fread(pins_f, colClasses = "character", encoding = "UTF-8")
pins <- rbind(out, pins[election_year != "2006"], use.names = TRUE, fill = TRUE)
pins[is.na(shared_ags), shared_ags := ""]
setcolorder(pins, c("election_year", "pool_code", "wahlbezirk", "landtagswahlkreis", "pool_label",
                    "pool_voters", "member_ags", "member_names", "shared_ags", "basis"))
setorder(pins, election_year)
fwrite(pins, pins_f)
cat(sprintf("2006: %d pools (%d source rows, %d voters), %d named member links, %d shared parts\n",
            nrow(out), sum(pools$rows), sum(pools$pool_voters), nrow(named), nrow(orph)))
print(orph[, .(lwk, kreis, ags, name, vg_name, ev)])
