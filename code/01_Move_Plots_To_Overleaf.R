##

if (Sys.info()['user'] == 'hanno') {
  to_path <- '~/Dropbox/Apps/Overleaf/ElectionPaper/figures'
} else if (Sys.info()["user"] == "vincentheddesheimer") {
  to_path <- "~/Dropbox (Princeton)/Apps/Overleaf/ElectionPaper/figures"
} else {
  to_path <- ""
}


plots <- dir('output/', full.names = T, recursive = T) %>% 
  .[str_detect(., 'pdf|jpeg|tex|png')]

file.copy(
  from = c(plots),
  to = to_path,
  overwrite = TRUE,
  recursive = FALSE,
  copy.mode = TRUE
)



cat('Plots moved successfully')

