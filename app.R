
Sys.setlocale(category = "LC_ALL", locale = "fi_FI.utf-8")

library(shiny)
library(dplyr)
library(reactable)
library(bslib)
library(glue)
library(lubridate)

versio <- "v0.0.1"

ui <- page_navbar(
  tags$style(HTML("
      .bold {
  font-weight: bold;
            }")),
  title = "Irman ilmoittautumistiedot",
  nav_panel("Avoimet ilmoittautumiset", 
            
            tags$p("Tässä sovelluksessa näet kisat joiden ilmoittatuminen on avoinna ja voit selata niihin kisoihin jo ilmoittautuneita."),
            card(
              card_header("Kilpailut joihin ilmoittautuminen vielä auki"),
              reactableOutput("ilmo_data")
            )),
  nav_panel("Selaa ilmoittautuneita", 
            card(card_header = "Hae ilmoittautuneita",
              tags$p("Jos haet koko nimellä, käytä muotoa: 'Sukunimi Etunimi'"),
              fluidRow(
                column(3,
                       textInput(inputId = "hakuteksti", "Syötä lisenssi tai nimi"),
                       actionButton(inputId = "tee_haku", "Hae")
                       ),
                column(3,
                       textInput(inputId = "hakuteksti_seura", "Seuran nimi")
                ),
                column(3,
                       textInput(inputId = "hakuteksti_sarja", "Sarja")
                ),
                column(3,
                       textInput(inputId = "hakuteksti_kisa", "Kisan nimi")
                )
              ),
              tableOutput("hakutulos")
            )),
  nav_spacer(),
  nav_item(tags$a("irma.suunnistusliitto.fi", href = "https://irma.suunnistusliitto.fi/")),
  tags$html(HTML(glue(
    '
    <div class="container">
  <footer class="d-flex flex-wrap justify-content-between align-items-center py-3 my-4 border-top">
    <div class="col-md-4 d-flex align-items-center">
      <a href="/" class="mb-3 me-2 mb-md-0 text-muted text-decoration-none lh-1">
        <svg class="bi" width="30" height="24"><use xlink:href="#bootstrap"/></svg>
      </a>
      <span class="mb-3 mb-md-0 text-muted">&copy; 2023 Markus Kainu</span>
      
    </div>

    <ul class="nav col-md-4 justify-content-end list-unstyled d-flex">
      <li class="ms-3"><a class="text-muted" href="https://markuskainu.fi">markuskainu.fi</a></li>
      <li class="ms-3"><a class="text-muted" href="https://markuskainu.fi">lähde Gitlabissa</a></li>
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
      as.character()
    glue("uusin päivitys: {leima}")
  })
  
    output$ilmo_data <- renderReactable({
        # generate bins based on input$bins from ui.R
  df <- readRDS("./ilmo_data.RDS") %>% 
        rename(kisapva = date) %>% 
    # mutate(date2 = as.POSIXct(ilmo_date1))
        mutate(#td = round(difftime(as.POSIXct(ilmo_date1),Sys.time())),
          aikaa_jaljella_num = lubridate::as.period(lubridate::as.duration(lubridate::interval(Sys.time()-3600*3,as.POSIXct(ilmo_date1, tz = "Europe/Helsinki")))),
          aikaa_jaljella = sub("d", " pva", sub("H", " tuntia", sub("H .+$", "H", as.character(aikaa_jaljella_num)))),
          
          aikaa_jaljella_num = as.numeric(aikaa_jaljella_num)
          # aikaa_jaljella = sub("")
               # aikaa_jaljella = sprintf('%02d %02d:%02d:%02d', day(dur), hour(dur), minute(dur), second(dur))
               ) %>% 
        select(kisapva, kisa, 
               ilmo_date1, aikaa_jaljella,
               aikaa_jaljella_num,
               ilmoittaudu,
               seurat
              ) %>% 
    rename("viimeinen ilmopva" = ilmo_date1,
           "aikaa ilmoittautua" = aikaa_jaljella)
  
  reactable(df,
            # filterable = TRUE,
            striped = FALSE,
            defaultPageSize = 30,
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
        matsit <- matsit %>% mutate(pvm = as.character(pvm)) %>% select(-emit,-emi_tag)
      return(matsit)
      
    })
    
    
output$hakutulos <- renderTable({
  tee_haku()
})


      
      
      
      
      

}

# Run the application 
shinyApp(ui = ui, server = server)
