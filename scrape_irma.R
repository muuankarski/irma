#!/usr/bin/env Rscript
setwd("/home/aurelius/sovellukset/irma")
library(rvest)
library(dplyr)
library(janitor)
library(glue)
library(htmltools)
library(httr2)
source("./R/irma_functions.R")


all_comp <- irma_get_all_comp()
listID <- 

lstId <- list()
for (i in seq(all_comp)) {
  lstId[[i]] <- all_comp[[i]]$dayId
}
dayIds <- do.call("c", lstId)

lstIdComp <- list()
for (i in seq(dayIds)) {
  
  if (i %% 10 == 0){
    print(i)
    Sys.sleep(time = rnorm(1, mean = 2, sd = .8))
  }
  tmpId <- irma_get_comp_day_details(id = dayIds[i])
  tmpDet <- data.frame(
    id = if_null_replace_with_NA(tmpId$id),
    name = if_null_replace_with_NA(tmpId$competition$name),
    webPage = if_null_replace_with_NA(tmpId$competition$webPage),
    club = if_null_replace_with_NA(tmpId$competition$organizingClubs[[1]]$name),
    competitionId = if_null_replace_with_NA(tmpId$competition$id),
    registrationAllowed = if_null_replace_with_NA(tmpId$registrationAllowed),
    distanceType = if_null_replace_with_NA(tmpId$distanceType),
    competitionDate = if_null_replace_with_NA(tmpId$competitionDate),
    firstRegistrationPeriodClosingDate = if_null_replace_with_NA(tmpId$registrationPeriod$firstRegistrationPeriodClosingDate),
    secondRegistrationPeriodClosingDate = if_null_replace_with_NA(tmpId$registrationPeriod$secondRegistrationPeriodClosingDate),
    thirdRegistrationPeriodClosingDate = if_null_replace_with_NA(tmpId$registrationPeriod$thirdRegistrationPeriodClosingDate),
    lateRegistrationPeriodClosingDate = if_null_replace_with_NA(tmpId$registrationPeriod$lateRegistrationPeriodClosingDate)
  )
  lstIdComp[[i]] <- tmpDet
}
compDet <- do.call("bind_rows", lstIdComp) %>% 
  as_tibble() %>% 
  mutate(across(c(competitionDate,firstRegistrationPeriodClosingDate,secondRegistrationPeriodClosingDate,thirdRegistrationPeriodClosingDate),
                \(x) as.Date(x, tz = "Europe/Helsinki")+1)) %>% 
  mutate(kisa = paste0("<a href = '",ifelse(grepl("^htt", webPage), webPage, paste0("https://",webPage)),"' target = '_blank'>",name,"</a>"),
         ilmoittaudu = paste0("<a href = 'https://irma.suunnistusliitto.fi/registerForCompetition/",id,"' target = '_blank'>Ilmoittaudu</a>")
  )






compReg <- list()
for (i in 1:nrow(compDet)){
  if (i %% 10 == 0){
    print(i)
    Sys.sleep(time = rnorm(1, mean = 2, sd = .8))
  }
  tmpReg <- irma_get_registered(compId = compDet$competitionId[i])
  tmpReg$competitionId <- compDet$competitionId[i]
  compReg[[i]] <- tmpReg
}

compRegDet <- list()
for (i in seq(compReg)){
  tmpc <- compReg[[i]]
  if (length(tmpc) < 2) next()
  perReg <- list()
  for (ii in seq(tmpc)){
    if (ii == length(tmpc)) next()
    perReg[[ii]]  <- data.frame(
      class = if_null_replace_with_NA(tmpc[[ii]]$dayClasses[[1]]),
      lastName = if_null_replace_with_NA(tmpc[[ii]]$userLastName[[1]]),
      firstName = if_null_replace_with_NA(tmpc[[ii]]$userFirstName[[1]]),
      userLicenseNumber = if_null_replace_with_NA(tmpc[[ii]]$userLicenseNumber[[1]]),
      clubName = if_null_replace_with_NA(tmpc[[ii]]$clubName[[1]])
    )  
  }
  compRegDet[[i]] <- do.call("bind_rows", perReg) %>% 
    mutate(competitionId = tmpc$competitionId)
}
datReg <- do.call("bind_rows", compRegDet) %>% 
  distinct() %>% 
  as_tibble() %>%
  left_join(compDet %>% distinct(competitionId,name,competitionDate)) %>% 
  select(-competitionId) %>% 
  mutate(nimi = paste0(firstName," ",lastName)) %>% 
  rename(seura = clubName,
         lisenssi = userLicenseNumber,
         sarja = class,
         kisa = name,
         pvm = competitionDate) %>% 
  filter(pvm >= Sys.Date())


saveRDS(object = compDet, file = "ilmo_data.RDS")
arrow::write_parquet(datReg, "./ilmo_raportti_df.parquet")

saveRDS(object = Sys.time(), file = "aikaleima.RDS")
source("./run.R")


