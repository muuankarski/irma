#!/usr/bin/env Rscript
setwd("/home/aurelius/sovellukset/irma")
library(rvest)
library(dplyr)
library(janitor)
library(glue)
library(htmltools)


vuosi_nyt <- "2023|2024"
vuosi_nyt <- "2023"
irma_base <- "https://irma.suunnistusliitto.fi/irma/public/competitioncalendar/view?year=-1&areaId=-1&discipline=all&competitionOpen=ALL"
raaka <- read_html(irma_base)
taulu_raaka <- html_table(raaka, header = TRUE)[[5]]
taulu_raaka <- clean_names(taulu_raaka)
taulu_raaka$date <- as.Date(taulu_raaka$paiva, format = "%e.%m.%Y", tz = "Europe/Helsinki")

ilmo_auki <- taulu_raaka %>% 
  filter(grepl("Ilmo", toiminnot),
         grepl(vuosi_nyt, date)) %>% 
  mutate(seurat = substr(seurat, start = 1, stop = 25))

ilmo_lista <- list()
raportti_lista <- list()
for (i in 1:nrow(ilmo_auki)){
  kisa_url <- glue("https://irma.suunnistusliitto.fi/irma/public/competition/view?id={ilmo_auki$kilp_numero[i]}")
  raaka_kisa <- read_html(kisa_url)
  
  ilmo_taulu <- html_table(raaka_kisa, header = TRUE)[[4]]
  ilmo_date1 <- ilmo_taulu[grepl("1. ilmo", ilmo_taulu[[1]]),][[2]]
  ilmo_date2 <- ilmo_taulu[grepl("2. ilmo", ilmo_taulu[[1]]),][[2]]
  kisa_link <- ilmo_taulu[grepl("www", ilmo_taulu[[1]]),][[2]]
  kisa_link <- ifelse(kisa_link == "", NA, 
                      ifelse(!grepl("^http", kisa_link), 
                             paste0("https://", kisa_link), kisa_link)
  )
  kisanimi <- substr(ilmo_auki$kilpailupaivan_nimi[i], start = 1, stop = 40)
  kisa_link <- ifelse(!is.na(kisa_link), HTML(glue("<a href='{kisa_link}'>{kisanimi}</a>")), 
                      kisanimi)
  
  
  ilmo_lista[[i]] <- tibble(kilp_numero = ilmo_auki$kilp_numero[i],
         ilmo_date1 = as.Date(ilmo_date1, format = "%e.%m.%Y", tz = "Europe/Helsinki"),
         ilmo_date2 = as.Date(ilmo_date2, format = "%e.%m.%Y", tz = "Europe/Helsinki"),
         kisa = kisa_link,
         ilmoittaudu = HTML(glue("<a href='{kisa_url}'>Irma</a>"))
         )
  
  # ilmoittautumisraportit
  
  ilmoit_raportti_link <- raaka_kisa %>% html_elements("a") %>% html_attr("href") %>% .[grepl("entries",.)]
  if (length(ilmoit_raportti_link) == 0){
    next()
  } else {
    ilmoit_raportti_link <- glue("https://irma.suunnistusliitto.fi/{ilmoit_raportti_link}")
    ilmoit_raportti_raw <- read_html(ilmoit_raportti_link)
    ilmoit_raportti_tbl <- ilmoit_raportti_raw %>% html_table(header = TRUE)
    raportti_lista[[i]] <- ilmoit_raportti_tbl[[7]] %>% 
      clean_names() %>% 
      mutate(kisa = ilmo_auki$kilpailupaivan_nimi[i],
             pvm = ilmo_auki$date[i]
      )    
  }
}
ilmo_df <- do.call("bind_rows", ilmo_lista)
ilmo_data <- left_join(ilmo_auki, ilmo_df)

ilmo_raportti_df <- do.call("bind_rows", raportti_lista) %>% 
  select(kisa,pvm,x1_pv,lisenssinumero,emit,emi_tag,nimi,seura) %>% 
  rename(sarja = x1_pv,lisenssi = lisenssinumero)

saveRDS(object = ilmo_data, file = "ilmo_data.RDS")
arrow::write_parquet(ilmo_raportti_df, "./ilmo_raportti_df.parquet")

saveRDS(object = Sys.time(), file = "aikaleima.RDS")



