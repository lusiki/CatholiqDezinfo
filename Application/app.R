library(shiny)
library(shinythemes)

# Define UI with an improved aesthetic
ui <- fluidPage(
  theme = shinytheme("yeti"), # Using a shinythemes theme for better aesthetics
  titlePanel("Detekcija dezinformacija katoličke tematike"),
  fluidRow(
    column(6,
           textAreaInput("text", "Unesite tekst objave:", rows = 10, placeholder = "Ovdje unesite tekst...")
    ),
    column(6,
           actionButton("check", "Provjeri tekst", class = "btn-primary")
    )
  ),
  hr(), # Horizontal line for better separation of content
  h4("Rezultati analize:"),
  textOutput("result")
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
      output$result <- renderText({
        result_text <- ""
        if (!is.null(results$KatoličkaTema)) {
          result_text <- paste(result_text, "Tekst objave odgovara katoličkoj tematici. Identificirane riječi su:", results$KatoličkaTema, "\n\n")
        }

        result_text <- paste(result_text, "Identificirana područja katoličke tematike:\n")
        for (area in c("Fraze", "Pravno", "Politika", "Institucije")) {
          if (!is.null(results[[area]])) {
            result_text <- paste(result_text, sprintf("%s: %s\n", area, results[[area]]))
          }
        }

        if (result_text == "Identificirana područja katoličke tematike:\n") {
          result_text <- "Nisu pronađene riječi koje upučuju na dezinformacije."
        }

        return(result_text)
      })
    } else {
      output$result <- renderText("Unesite tekst za provjeru.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

