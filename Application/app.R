

#libs----
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(data.table)
library(readxl)
library(xlsx)
library(here)
library(kableExtra)
library(DT)
library(purrr)
library(data.table)
library(tidytext)
library(dplyr)
library(lubridate)
library(anytime)
library(grid)
library(wordcloud)
library(reshape2)
library(ggraph)
library(widyr)
library(topicmodels)
library(ggthemes)
library(xlsx)
library(writexl)
library(data.table)
library(stringi)
library(openxlsx)

#source----
#
#
# source("./helper_files/stemmer.R")
# source("./helper_files/text_analysis.R")

# Define UI with an improved aesthetic
ui <- fluidPage(
  theme = shinytheme("yeti"), # Using a shinythemes theme for better aesthetics

  tags$header(
    style = "background-color: #f5f5f5; padding-top: 2px; padding-bottom: 6px; text-align: center;",
    tags$img(src = "logo.jpg", height = "30px"),
    tags$h2("Kritička analiza dezinformacija o vjerskim temama", style = "color: #333; margin-top: 5px;"),
    tags$h4("", style = "color: #555;")
  ),

  tabsetPanel(
    tabPanel("Unos podataka", # The existing content
             fluidRow(
               column(12, # This sets the width to the full available width
                      textAreaInput("text", "", rows = 10, placeholder = "Ovdje unesite tekst..."),
                      actionButton("check", "Analiziraj", class = "btn-primary"),
                      textOutput("text_feedback") %>% withSpinner() # Adding the spinner here
               )
             )
    ),
    tabPanel("Analiza", # Placeholder for Analysis tab
             h2("")
    ),
    tabPanel("Opis", # About tab content
             h2(""),
             p("Projekt „Kritička analiza dezinformacija o vjerskim temama“ Hrvatskog katoličkog sveučilišta te njegovih partnera Hrvatskog katoličkog radija i Hrvatskog društva katoličkih novinara prošao je na javnom pozivu za dodjelu bespovratnih sredstava za uspostavu sustava provjere točnosti informacija u okviru mjere „Uspostava provjere medijskih činjenica i sustava javne objave podataka“. „Uz samu provjeru dezinformacija izradit ćemo alat kojim ćemo omogućiti alat za provjeru dezinformacija, na HKS-u ćemo pokrenuti izborni kolegij posvećen samo medijima i dezinformacijama, izradit ćemo i sveučilišni udžbenik i mnogo vremena posvetiti edukacijama stručnih suradnika u tiskovnim uredima diljem Hrvatske, kao i studentima i učenicima koji moraju znati prepoznati dezinformacije u vjerskom prostoru“, objasnila je doc. dr. sc. Lana Ciboci Perša za IKA-u.
Na projektu će sudjelovati stručnjaci i znanstvenici iz područja komunikologije i novinarstva, a unutar partnerskih institucija bit će pokrenute posebne jedinice: tako će na HKR-u biti pokrenuta zasebna redakcijska jedinica koja će se baviti provjerom točnosti informacija o vjerskim temama, a na portalu Hrvatske katoličke mreže nova rubrika „Glas istine“.Ovaj projekt prvi je ovakve vrste koji će se baviti sustavnim provjeravanjem činjenica o vjerskim temama u hrvatskom medijskom prostoru.Nositelji mjere koja se provodi u okviru Nacionalnog plana oporavka i otpornosti su Ministarstvo kulture i medija i Agencija za elektroničke medije.
.Izradio Odjel za komunikologiju, HKS.")
    )
  ),

  # Footer
  tags$footer(
    style = "position: fixed; bottom: 0; width: 100%; background-color: #f5f5f5; text-align: center; padding: 7px;",
    "Built with ❤ by lux: © 2023 Projekt u suradnji HKS, HKR i HDKN za uspostavu sustava provjere točnosti informacija.“ | ",
    a(href = "https://www.unicath.hr/projekt-kriticka-analiza-dezinformacija-o-vjerskim-temama-hks-a-i-partnerskih-institucija-dobio-bespovratna-sredstva-za-uspostavu-sustava-provjere-tocnosti-informacija", "Web")
  )
)

# Define server logic
server <- function(input, output) {

  # Vectors of words to check
  katolicka_tema <- "\\b(crkv|biskup|kaptol|časn|sestr|svećenik|župnik|vjernik|kardinal|pap|sveti otac|redovnik|redovnic|kršćanstv|vjer|gosp|isus|katoličk|mis|pričest|krizm|grijeh|vjeroučitelj|vjeronauk|blagoslov|svjedočanstv|relikvij|stigm|duhovnost|velečasn|zaređenj|krunic|vjeronauk|ukazanj|stepinc|damir|stojić|željk|markić|ik|mandurić|vlad|košić|robert|bajruš|inoslav|bešker|ant|tomić|branimir|pofuk|igor|lasić|hrvoj|marjanović|bozanić|ksen|abramović|drag|pilsel|ksaver|hbk|opus de|protagor|caritas|vatikansk|ugovor|plaćanj|blagoslov|sakrament|nekretnin|imovin|hod|život|obitelj|prolif|poništenj|rimsk|ugovor|sekularizacij|sekularn|držav|klerikalizm|crkv|ruš|vlast|veličaj|ustaštv|oduzima|prav|žen|stvori|katoličk|držav|afer|zataškavanj|kaptol|šutnj|vjeronauk|škol|vatikansk|bank|klerikaln|vlast|odvojen|potič|homofobij|rodn|ideologij|klerikalizm|ravnozemljaš|gay|brak|podržava|ustaš|nacist|blagoslovi|rat|sekularn|katoličk|vlad|žen|smijenjen|konzervativc|tradicionalist|pobačaj|abortus|aktivist|aktivizm|jezuit|nazadan|zaosta|neobrazovan|privilegij|privilegiran|diskriminacij|nacionalizm|nacionalist|ekstremist|otpušten|prekrštavanj|izopćen|izbačen|bludniči|posvećenj|inkardiniran|inkardinacij|mračno dob|razotkri|prijavi|bludniči|pronevjeri|homofobija|zlodjel|progon|dogm|kontroverzni svećenik|moderni svećenik|tolerantn|policij|vjerska kontrol|crkveni medij|vjerski medij|ukidanj|homofob|pedofi|homoseksualnost|patrijarha|čudesn|ozdravljenj|čud)\\b"
  fraze <- "\\b(smijenjen|konzervativc|tradicionalist|pobačaj|abortus|aktivist|aktivizm|jezuit|nazadan|zaosta|neobrazovan|privilegij|privilegiran|diskriminacij|nacionalizm|nacionalist|ekstremist|otpušten|prekrštavanj|izopćen|izbačen|bludniči|posvećenj|inkardiniran|inkardinacij|mračno dob|razotkri|prijavi|bludniči|pronevjeri|homofobija|zlodjel|progon|dogm|kontroverzn|modern|svećenik|tolerantn|vjer|policij|kontrol|medij|medij|ukidanj|homofob|pedofi|homoseksualnost|patrijarha|čudesn|ozdravljenj|čud|pedofilij|klečavc|kaptolaš|popov|lopov|zatucani|fanatic|fašist|katoliban|crkvenjak|ekstremn)\\b"
  pravno <- "\\b(vatikansk|ugovor|plaćanj|blagoslov|sakrament|nekretnin|imovin)\\b"
  politika <- "\\b(hod|život|obitelj|prolif|poništenj|rimsk|ugovor|sekularizacij|sekularn|držav|klerikalizm|crkv|ruš|vlast|veličaj|ustaštv|oduzima|prav|žen|stvori|katoličk|držav|afer|zataškavanj|kaptol|šutnj|vjeronauk|škol|vatikansk|bank|klerikaln|vlast|odvojen|potič|homofobij|rodn|ideologij|klerikalizm|ravnozemljaš|gay|brak|podržava|ustaš|nacist|blagoslovi|rat|sekularn|katoličk|vlad|žen)\\b"
  institucije <- "\\b(ksaver|hbk|opus de|protagor|caritas)\\b"

  checkWords <- function(text, pattern) {
    matches <- gregexpr(pattern, text, perl = TRUE)
    found_words <- unique(unlist(regmatches(text, matches)))
    if (length(found_words) > 0) {
      return(paste(found_words, collapse = ", "))
    }
    return(NULL)
  }


  # Reactive value for text length
  text_length <- reactive({
    nchar(input$text)
  })

  # Reactive output for text length feedback
  output$text_feedback <- renderText({
    if (text_length() > 0 && text_length() < 50) {
      "Tekst je prekratak za analizu. Unesite duži tekst."
    } else {
      NULL
    }
  })


  observeEvent(input$check, {
    if (input$text != "") {
      # Check for words in vectors
      results <- list(
        KatoličkaTema = checkWords(input$text, katolicka_tema),
        Fraze = checkWords(input$text, fraze),
        Pravno = checkWords(input$text, pravno),
        Politika = checkWords(input$text, politika),
        Institucije = checkWords(input$text, institucije)
      )

      # Formatting the results
      output$result <- renderUI({
        result_text <- HTML("") # Initialize as HTML

        if (!is.null(results$KatoličkaTema)) {
          katolicka_tema_text <- paste("Tekst objave odgovara katoličkoj tematici.<br>Identificirane riječi su:", results$KatoličkaTema, "<br><br>")
          result_text <- HTML(paste(result_text, katolicka_tema_text, sep = ""))
        }

        areas_text <- "Identificirana područja katoličke tematike:<br><br>"
        for (area in c("Fraze", "Pravno", "Politika", "Institucije")) {
          if (!is.null(results[[area]])) {
            area_text <- paste(sprintf("%s:<br>%s<br><br>", area, results[[area]]))
            areas_text <- paste(areas_text, area_text, sep = "")
          }
        }

        if (areas_text == "Identificirana područja katoličke tematike:<br><br>") {
          areas_text <- "Nisu pronađene riječi koje upučuju na dezinformacije."
        }

        result_text <- HTML(paste(result_text, areas_text, sep = ""))
        return(result_text)
      })

    } else {
      output$result <- renderText("Unesite tekst za provjeru.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

