
Sys.setlocale(category = "LC_ALL", locale = "fi_FI.utf-8")

library(shiny)
library(dplyr)
library(reactable)
library(bslib)
library(glue)
library(lubridate)
library(metathis)

versio <- "v0.1.0"

ui <- page_fluid(#theme = bslib::bs_theme(version = 5, bootswatch = "united"),
  tags$style(HTML("
      .bold {
  font-weight: bold;
            }")),
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$html(HTML('<script defer data-domain="shiny.vaphana.com" src="https://plausible.vaphana.com/js/script.js"></script>'))
    ),
  meta() %>%
    meta_description(description = "Irman ilmoittautumistiedot") %>%
    meta_social(
      title = "Irman ilmoittautumistiedot",
      description = "Suunnistusliiton Irma-palvelun ilmoittautumistiedot",
      url = "",
      image = "irma.png",
      image_alt = "An image for social media cards",
      twitter_creator = "@muuankarski",
      twitter_card_type = "summary_large_image",
      twitter_site = "@muuankarski"
    ),
  theme = bslib::bs_theme(bootswatch = "cosmo",version = 5,
                          # bg = "#0b3d91", fg = "white", primary = "#FCC780",
                          base_font = font_google("PT Sans"),
                          code_font = font_google("Space Mono")),
  # tags$html(HTML('<a class="sr-only sr-only-focusable" href="#maincontent">Skip to main</a>')),
  tags$style(HTML("
      .navbar-xyz {
        background-color: rgb(255, 255, 255, .9);
        border-bottom: 1px solid rgb(55, 55, 55, .4);
      }
      .leaflet-container {
    background: #FFF;
    }
      .grey-background {
      background-color: rgb(245, 245, 245, .9);
      padding-top: 10px;
      padding-right: 10px;
      padding-bottom: 10px;
      padding-left: 10px;
      }
      #map {
    margin: auto;
  }")),
  tags$html(HTML(glue::glue('
    <nav class="navbar sticky-top navbar-xyz">
      <a class="navbar-brand" role="brand" href = "#"><img src = "logo.svg" style = "height: 40px; padding-right: 0px;" alt = "logo"></a>
      <div class = "lead">Irman ilmoittautumistiedot<span style="font-size:12px; padding-left:10px;">{versio}</span></div>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-end" id="navbarResponsive">
        <ul class="navbar-nav ml-auto">
          <li class="nav-item">
            <a class="nav-link text-muted" href="https://irma.suunnistusliitto.fi">irma.suunnistusliitto.fi</a>
          </li>
          <li class="nav-item">
            <a class="nav-link text-muted" href="https://github.com/muuankarski/irma">Lähdekoodi</a>
          </li>
        </ul>
      </div>
  </nav>'))),
  
  tags$div(style = "padding-top: 30px;"),
  tags$p("Sovelluksessa näet kisat joiden ilmoittatuminen on avoinna ja voit selata niihin kisoihin jo ilmoittautuneita."),
  tags$p("Avoimet kisat ja ilmoittautuneet näkyvät alla ns. eri välilehdillä"),
  textOutput("aikaleima"),
  tags$hr(),
  fluidRow(
  tabsetPanel(type = "pills",
    
  tabPanel(title = "Avoimet kisat",
         reactableOutput("ilmo_data")
  ),
  tabPanel(title = "Selaa ilmoittautuneita",
           fluidRow(
             tags$hr(),
             tags$p("Jos haet koko nimellä, käytä muotoa: 'Sukunimi Etunimi'"),
             column(3, 
                    textInput(inputId = "hakuteksti", "Syötä lisenssi tai nimi")
             ),
             column(3, 
                    textInput(inputId = "hakuteksti_seura", "Seuran nimi")
             ),
             column(3, 
                    textInput(inputId = "hakuteksti_sarja", "Sarja")
             ),
             column(3, 
                    textInput(inputId = "hakuteksti_kisa", "Kisan nimi")
             )),
           fluidRow(
             column(3, 
                    actionButton(inputId = "tee_haku", "Hae")
             )),   
         tableOutput("hakutulos")
  )
  )
  ),

  tags$html(HTML(glue(
    '
    <div class="container">
  <footer class="d-flex flex-wrap justify-content-between align-items-center py-3 my-4 border-top">
    <div class="col-md-4 d-flex align-items-center">
      <a href="/" class="mb-3 me-2 mb-md-0 text-muted text-decoration-none lh-1">
        <svg class="bi" width="30" height="24"><use xlink:href="#bootstrap"/></svg>
      </a>
      <span class="mb-3 mb-md-0 text-muted">&copy; 2023-2024 Markus Kainu</span>
      
    </div>

    <ul class="nav col-md-4 justify-content-end list-unstyled d-flex">
      <li class="ms-3"><a class="text-muted" href="https://markuskainu.fi">markuskainu.fi</a></li>
      <code class="mb-3 mb-md-0 text-muted">{versio}</code>
    </ul>
  </footer>
</div>
    '
  )))
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  output$aikaleima <- renderText({
    leima <- readRDS("aikaleima.RDS") %>% 
      as.character() %>% 
      sub("\\..+$", "", .)
    glue("Data päivitetty: {leima}")
  })
  
    output$ilmo_data <- renderReactable({
        # generate bins based on input$bins from ui.R
  df <- readRDS("./ilmo_data.RDS") %>% 
        filter(#!is.na(ilmo_date1),
          competitionDate > Sys.Date()) %>%
        rename(kisapva = competitionDate,
               # kisa = name, 
               ilmo_date1 = firstRegistrationPeriodClosingDate,
               seurat = club
               ) %>% 
    # mutate(date2 = as.POSIXct(ilmo_date1))
        mutate(#td = round(difftime(as.POSIXct(ilmo_date1),Sys.time())),
          aikaa_jaljella_num = lubridate::as.period(lubridate::as.duration(lubridate::interval(Sys.time()+3600*2,as.POSIXct(ilmo_date1+1, tz = "Europe/Helsinki")))),
          aikaa_jaljella = sub("d", " pva", sub("H", " tuntia", sub("H .+$", "H", as.character(aikaa_jaljella_num)))),
          
          aikaa_jaljella_num = as.numeric(aikaa_jaljella_num)
          # aikaa_jaljella = sub("")
               # aikaa_jaljella = sprintf('%02d %02d:%02d:%02d', day(dur), hour(dur), minute(dur), second(dur))
               ) %>% 
    filter(!is.na(aikaa_jaljella_num)) %>% 
        select(kisapva, kisa, 
               ilmo_date1, aikaa_jaljella,
               aikaa_jaljella_num,
               ilmoittaudu,
               seurat
              ) %>% 
    rename("viimeinen ilmopva" = ilmo_date1,
           "aikaa ilmoittautua" = aikaa_jaljella) %>% 
    arrange(kisapva)
  
  reactable(df,searchable = TRUE,
            # filterable = TRUE,
            striped = FALSE,
            defaultPageSize = 100,
            minRows = 5,
            columns = list(
              aikaa_jaljella_num = colDef(show = FALSE),
              kisa = colDef(html = TRUE),
              ilmoittaudu = colDef(html = TRUE)#,
              ),
            rowStyle = function(index) {
              if (df[index, "aikaa_jaljella_num"] < 90000) {
                list(background = "rgba(255, 51, 51, 0.25)")
              }
            },
            rowClass = function(index) {
              if (df[index, "aikaa_jaljella_num"] < 90000) {
                "bold"
              }
            }
            )
        
    })
    
    tee_haku <- eventReactive(input$tee_haku, {
      
        input_hakuteksti <- input$hakuteksti
        input_hakuteksti_seura <- input$hakuteksti_seura
        input_hakuteksti_sarja <- input$hakuteksti_sarja
        input_hakuteksti_kisa <- input$hakuteksti_kisa
        # input_hakuteksti <- 1682
        if (!grepl("[0-9]",input_hakuteksti)){
          matsit <- arrow::read_parquet("./ilmo_raportti_df.parquet") %>% 
            filter(grepl(input_hakuteksti, nimi, ignore.case = TRUE) & grepl(input_hakuteksti_seura, seura, ignore.case = TRUE) & grepl(input_hakuteksti_sarja, sarja, ignore.case = TRUE) & grepl(input_hakuteksti_kisa, kisa, ignore.case = TRUE))  
        } else {
          matsit <- arrow::read_parquet("./ilmo_raportti_df.parquet") %>% 
            filter(grepl(input_hakuteksti, lisenssi, ignore.case = TRUE) & grepl(input_hakuteksti_seura, seura, ignore.case = TRUE) & grepl(input_hakuteksti_sarja, sarja, ignore.case = TRUE) & grepl(input_hakuteksti_kisa, kisa, ignore.case = TRUE))
        }
        matsit <- matsit %>% mutate(pvm = as.character(pvm)) %>% select(-lisenssi,-firstName,-lastName)
      return(matsit)
      
    })
    
    
output$hakutulos <- renderTable({
  tee_haku()
})


      
      
      
      
      

}

# Run the application 
shinyApp(ui = ui, server = server)
