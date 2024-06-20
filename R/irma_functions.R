irma_get_all_comp <- function(){
  # httr2::curl_translate(cmd = 'curl \'https://irma.suunnistusliitto.fi/connect/CompetitionCalendarEndpoint/view\' -X POST -H \'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:126.0) Gecko/20100101 Firefox/126.0\' -H \'Accept: application/json\' -H \'Accept-Language: en-US,en;q=0.5\' -H \'Accept-Encoding: gzip, deflate, br, zstd\' -H \'Referer: https://irma.suunnistusliitto.fi/public/competitioncalendar/list\' -H \'Content-Type: application/json\' -H \'X-CSRF-Token: 21734f6d-c59f-444d-8900-3c03c1db4634\' -H \'Origin: https://irma.suunnistusliitto.fi\' -H \'Connection: keep-alive\' -H \'Cookie: JSESSIONID=F4F56EFC514970089397DF28E7A57EF6; csrfToken=21734f6d-c59f-444d-8900-3c03c1db4634\' -H \'Sec-Fetch-Dest: empty\' -H \'Sec-Fetch-Mode: cors\' -H \'Sec-Fetch-Site: same-origin\' -H \'Priority: u=4\' --data-raw \'{"year":null,"discipline":null,"areaId":null,"calendarType":"all","competitionOpen":null}\'')
  resp <- request("https://irma.suunnistusliitto.fi/connect/CompetitionCalendarEndpoint/view") |> 
    req_method("POST") |> 
    req_headers(
      `User-Agent` = "Mozilla/5.0 (X11; Linux x86_64; rv:126.0) Gecko/20100101 Firefox/126.0",
      Accept = "application/json",
      `Accept-Language` = "en-US,en;q=0.5",
      `Accept-Encoding` = "gzip, deflate, br, zstd",
      `X-CSRF-Token` = "21734f6d-c59f-444d-8900-3c03c1db4634",
      Origin = "https://irma.suunnistusliitto.fi",
      Cookie = "JSESSIONID=F4F56EFC514970089397DF28E7A57EF6; csrfToken=21734f6d-c59f-444d-8900-3c03c1db4634",
      Priority = "u=4",
    ) |> 
    req_body_raw('{"year":null,"discipline":null,"areaId":null,"calendarType":"all","competitionOpen":null}', "application/json") |> 
    req_perform()
  
  res <- resp_body_json(resp)
  return(res)
}


irma_get_comp_details <- function(id = 26805){
  # httr2::curl_translate(cmd = 'curl \'https://irma.suunnistusliitto.fi/connect/CompetitionEndpoint/viewCompetition\' -X POST -H \'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0\' -H \'Accept: application/json\' -H \'Accept-Language: fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3\' -H \'Accept-Encoding: gzip, deflate, br\' -H \'Referer: https://irma.suunnistusliitto.fi/public/competition/view/26805\' -H \'Content-Type: application/json\' -H \'X-CSRF-Token: 2f4d9099-8026-4766-b0e4-ac1d7ef38c9f\' -H \'Origin: https://irma.suunnistusliitto.fi\' -H \'Connection: keep-alive\' -H \'Cookie: JSESSIONID=7A10BCBBE92D8521E910E70CD8943AC3; csrfToken=2f4d9099-8026-4766-b0e4-ac1d7ef38c9f\' -H \'Sec-Fetch-Dest: empty\' -H \'Sec-Fetch-Mode: cors\' -H \'Sec-Fetch-Site: same-origin\' --data-raw \'{"id":26805}\'')
  resp2 <- request("https://irma.suunnistusliitto.fi/connect/CompetitionEndpoint/viewCompetition") |> 
    req_method("POST") |> 
    req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0",
      Accept = "application/json",
      `Accept-Language` = "fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3",
      `Accept-Encoding` = "gzip, deflate, br",
      `X-CSRF-Token` = "2f4d9099-8026-4766-b0e4-ac1d7ef38c9f",
      Origin = "https://irma.suunnistusliitto.fi",
      Cookie = "JSESSIONID=7A10BCBBE92D8521E910E70CD8943AC3; csrfToken=2f4d9099-8026-4766-b0e4-ac1d7ef38c9f",
    ) |> 
    req_body_raw(paste0('{"id":',id,'}'), "application/json") |> 
    req_perform()
  
  res2 <- resp_body_json(resp2)
  return(res2)
}


irma_get_comp_day_details <- function(id = 26805){
  # httr2::curl_translate(cmd = 'curl \'https://irma.suunnistusliitto.fi/connect/CompetitionEndpoint/viewCompetitionDay\' -X POST -H \'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0\' -H \'Accept: application/json\' -H \'Accept-Language: fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3\' -H \'Accept-Encoding: gzip, deflate, br\' -H \'Referer: https://irma.suunnistusliitto.fi/public/competition/view/26805\' -H \'Content-Type: application/json\' -H \'X-CSRF-Token: 2f4d9099-8026-4766-b0e4-ac1d7ef38c9f\' -H \'Origin: https://irma.suunnistusliitto.fi\' -H \'Connection: keep-alive\' -H \'Cookie: JSESSIONID=7A10BCBBE92D8521E910E70CD8943AC3; csrfToken=2f4d9099-8026-4766-b0e4-ac1d7ef38c9f\' -H \'Sec-Fetch-Dest: empty\' -H \'Sec-Fetch-Mode: cors\' -H \'Sec-Fetch-Site: same-origin\' --data-raw \'{"id":26805}\'')
  resp <- request("https://irma.suunnistusliitto.fi/connect/CompetitionEndpoint/viewCompetitionDay") |> 
    req_method("POST") |> 
    req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0",
      Accept = "application/json",
      `Accept-Language` = "fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3",
      `Accept-Encoding` = "gzip, deflate, br",
      `X-CSRF-Token` = "2f4d9099-8026-4766-b0e4-ac1d7ef38c9f",
      Origin = "https://irma.suunnistusliitto.fi",
      Cookie = "JSESSIONID=7A10BCBBE92D8521E910E70CD8943AC3; csrfToken=2f4d9099-8026-4766-b0e4-ac1d7ef38c9f",
    ) |> 
    req_body_raw(paste0('{"id":',id,'}'), "application/json") |> 
    req_perform()
  res <- resp_body_json(resp)
  return(res)
}




irma_get_registered <- function(compId = 1209398){
  
  # httr2::curl_translate(cmd = 'curl \'https://irma.suunnistusliitto.fi/connect/CompetitionEndpoint/getEntriesForCompetition\' -X POST -H \'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0\' -H \'Accept: application/json\' -H \'Accept-Language: fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3\' -H \'Accept-Encoding: gzip, deflate, br\' -H \'Referer: https://irma.suunnistusliitto.fi/public/competition/view/26805\' -H \'Content-Type: application/json\' -H \'X-CSRF-Token: 2f4d9099-8026-4766-b0e4-ac1d7ef38c9f\' -H \'Origin: https://irma.suunnistusliitto.fi\' -H \'Connection: keep-alive\' -H \'Cookie: JSESSIONID=7A10BCBBE92D8521E910E70CD8943AC3; csrfToken=2f4d9099-8026-4766-b0e4-ac1d7ef38c9f\' -H \'Sec-Fetch-Dest: empty\' -H \'Sec-Fetch-Mode: cors\' -H \'Sec-Fetch-Site: same-origin\' --data-raw \'{"competitionId":1209398,"competitionClass":"","user":"","club":0}\'')
  resp3 <- request("https://irma.suunnistusliitto.fi/connect/CompetitionEndpoint/getEntriesForCompetition") |> 
    req_method("POST") |> 
    req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0",
      Accept = "application/json",
      `Accept-Language` = "fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3",
      `Accept-Encoding` = "gzip, deflate, br",
      `X-CSRF-Token` = "2f4d9099-8026-4766-b0e4-ac1d7ef38c9f",
      Origin = "https://irma.suunnistusliitto.fi",
      Cookie = "JSESSIONID=7A10BCBBE92D8521E910E70CD8943AC3; csrfToken=2f4d9099-8026-4766-b0e4-ac1d7ef38c9f",
    ) |> 
    req_body_raw(paste0('{"competitionId":',compId,',"competitionClass":"","user":"","club":0}'), "application/json") |> 
    req_perform()
  
  res3 <- resp_body_json(resp3)
  return(res3)
  
}

if_null_replace_with_NA <- function(x) ifelse(is.null(x), NA, x)

