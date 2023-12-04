library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Detekcija dezinformacija katoličke tematike"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Unesite tekst objave:", rows = 10),
      actionButton("check", "Provjeri tekst")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# Define the server logic
server <- function(input, output) {

  # Vector of words to check
  words_to_check <- "\\b(crkv|biskup|kaptol|časn|sestr|svećenik|župnik|vjernik|kardinal|pap|sveti otac|redovnik|redovnic|kršćanstv|vjer|gosp|isus|katoličk|mis|pričest|krizm|grijeh|vjeroučitelj|vjeronauk|blagoslov|svjedočanstv|relikvij|stigm|duhovnost|velečasn|zaređenj|krunic|vjeronauk|ukazanj|stepinc|damir|stojić|željk|markić|ik|mandurić|vlad|košić|robert|bajruš|inoslav|bešker|ant|tomić|branimir|pofuk|igor|lasić|hrvoj|marjanović|bozanić|ksen|abramović|drag|pilsel|ksaver|hbk|opus de|protagor|caritas|vatikansk|ugovor|plaćanj|blagoslov|sakrament|nekretnin|imovin|hod|život|obitelj|prolif|poništenj|rimsk|ugovor|sekularizacij|sekularn|držav|klerikalizm|crkv|ruš|vlast|veličaj|ustaštv|oduzima|prav|žen|stvori|katoličk|držav|afer|zataškavanj|kaptol|šutnj|vjeronauk|škol|vatikansk|bank|klerikaln|vlast|odvojen|potič|homofobij|rodn|ideologij|klerikalizm|ravnozemljaš|gay|brak|podržava|ustaš|nacist|blagoslovi|rat|sekularn|katoličk|vlad|žen|smijenjen|konzervativc|tradicionalist|pobačaj|abortus|aktivist|aktivizm|jezuit|nazadan|zaosta|neobrazovan|privilegij|privilegiran|diskriminacij|nacionalizm|nacionalist|ekstremist|otpušten|prekrštavanj|izopćen|izbačen|bludniči|posvećenj|inkardiniran|inkardinacij|mračno dob|razotkri|prijavi|bludniči|pronevjeri|homofobija|zlodjel|progon|dogm|kontroverzni svećenik|moderni svećenik|tolerantn|policij|vjerska kontrol|crkveni medij|vjerski medij|ukidanj|homofob|pedofi|homoseksualnost|patrijarha|čudesn|ozdravljenj|čud)\\b"

  observeEvent(input$check, {
    if (input$text != "") {
      # Find matching words in the text
      matches <- gregexpr(words_to_check, input$text, perl = TRUE)
      found_words <- unique(unlist(regmatches(input$text, matches)))

      # Output results
      if (length(found_words) > 0) {
        output$result <- renderText(paste("Pronađene riječi:", paste(found_words, collapse = ", ")))
      } else {
        output$result <- renderText("Nisu pronađene riječi koje upučuju na dezinformacije.")
      }
    } else {
      output$result <- renderText("Unesite tekst za provjeru.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
