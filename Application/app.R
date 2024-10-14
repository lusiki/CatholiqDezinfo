# Load necessary libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(bslib)
library(readxl)
library(tidytext)
library(dplyr)
library(stringr)
library(tm)
library(Matrix)
library(stopwords)
library(data.table)
library(tokenizers)
library(purrr)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Using the "cerulean" theme for blue aesthetics

  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      .navbar { margin-bottom: 0; }
      .header-logo { height: 50px; margin-top: 5px; }
      .header-title { color: #005B96; margin-top: 15px; }
      .footer { background-color: #e3f2fd; padding: 10px; position: fixed; bottom: 0; width: 100%; }
      .btn-primary { background-color: #005B96; border-color: #004080; }
      .btn-primary:hover { background-color: #004080; border-color: #003366; }
      .spinner-border { color: #005B96; }
      .progress-bar { background-color: #005B96; }
      .analysis-text { font-size: 16px; }
      .analysis-word { color: #005B96; font-weight: bold; }
      .header { background-color: #e3f2fd; padding: 10px; text-align: center; }
      .tab-content { padding: 20px; }
      .mt-2 { margin-top: 10px; }
      .analysis-sentence { margin-bottom: 15px; }
    "))
  ),

  # Header with Logo and Title
  tags$header(
    class = "header",
    tags$img(src = "logo.jpg", class = "header-logo"),
    tags$h2("Kritička analiza dezinformacija o vjerskim temama", class = "header-title")
  ),

  # Main content area with tabs
  tabsetPanel(
    tabPanel("Unos podataka",
             fluidRow(
               column(12,
                      h3("Unesite tekst za analizu"),
                      textAreaInput("text", label = NULL, rows = 10, placeholder = "Ovdje unesite tekst..."),
                      actionButton("check", "Provjeri", class = "btn btn-primary mt-2"),
                      uiOutput("text_feedback") %>% withSpinner(type = 4, color = "#005B96"),
                      br(),
                      uiOutput("result") %>% withSpinner(type = 4, color = "#005B96")
               )
             )
    ),
    tabPanel("Analiza",
             h3("Detaljna analiza"),
             uiOutput("detailed_analysis") %>% withSpinner(type = 4, color = "#005B96")
    ),
    tabPanel("Opis",
             h3("O projektu"),
             p("Projekt „Kritička analiza dezinformacija o vjerskim temama“ Hrvatskog katoličkog sveučilišta te njegovih partnera Hrvatskog katoličkog radija i Hrvatskog društva katoličkih novinara prošao je na javnom pozivu za dodjelu bespovratnih sredstava za uspostavu sustava provjere točnosti informacija u okviru mjere „Uspostava provjere medijskih činjenica i sustava javne objave podataka“."),
             p("„Uz samu provjeru dezinformacija izradit ćemo alat kojim ćemo omogućiti alat za provjeru dezinformacija, na HKS-u ćemo pokrenuti izborni kolegij posvećen samo medijima i dezinformacijama, izradit ćemo i sveučilišni udžbenik i mnogo vremena posvetiti edukacijama stručnih suradnika u tiskovnim uredima diljem Hrvatske, kao i studentima i učenicima koji moraju znati prepoznati dezinformacije u vjerskom prostoru“, objasnila je doc. dr. sc. Lana Ciboci Perša za IKA-u."),
             p("Na projektu će sudjelovati stručnjaci i znanstvenici iz područja komunikologije i novinarstva, a unutar partnerskih institucija bit će pokrenute posebne jedinice: tako će na HKR-u biti pokrenuta zasebna redakcijska jedinica koja će se baviti provjerom točnosti informacija o vjerskim temama, a na portalu Hrvatske katoličke mreže nova rubrika „Glas istine“."),
             p("Ovaj projekt prvi je ovakve vrste koji će se baviti sustavnim provjeravanjem činjenica o vjerskim temama u hrvatskom medijskom prostoru. Nositelji mjere koja se provodi u okviru Nacionalnog plana oporavka i otpornosti su Ministarstvo kulture i medija i Agencija za elektroničke medije."),
             p("Izradio Odjel za komunikologiju, HKS.")
    )
  ),

  # Footer
  tags$footer(
    class = "footer",
    HTML("Built with ❤ by lux: &copy; 2023 Projekt u suradnji HKS, HKR i HDKN za uspostavu sustava provjere točnosti informacija. | "),
    a(href = "https://www.unicath.hr/", "Web", target = "_blank")
  )
)

# Define Server logic
server <- function(input, output, session) {
  # Load necessary libraries
  # (Libraries already loaded globally)

  # Source the stemming functions (ensure these files are in the app directory)


  # base_dir <- file.path(getwd(), "Application")
  # # Source the stemming functions (ensure these files are in the Application/Source directory)
  # source(file.path(base_dir, "Source/stemmer.R"))       # This should define the write_tokens function
  # source(file.path(base_dir, "Source/text_analysis.R")) # If required by stemmer.R
  #
  # # Read the precomputed TF-IDF data
  # tf_idf_corpus <- read_excel(file.path(base_dir, "Source/output_tf_idf.xlsx"))
  #



  source("./Source/stemmer.R")       # This should define the write_tokens function
  source("./Source/text_analysis.R") # If required by stemmer.R

  # Read the precomputed TF-IDF data
  # Adjust the path to your Excel file
  tf_idf_corpus <- read_excel("./Source/output_tf_idf.xlsx")






#
# source("C:/Users/lukas/Dropbox/HKS/Projekti/Dezinformacije/CatholiqDezinfo/Application/Source/stemmer.R")       # This should define the write_tokens function
# source("C:/Users/lukas/Dropbox/HKS/Projekti/Dezinformacije/CatholiqDezinfo/Application/Source/text_analysis.R") # If required by stemmer.R
#
# # Read the precomputed TF-IDF data
# # Adjust the path to your Excel file
# tf_idf_corpus <- read_excel("C:/Users/lukas/Dropbox/HKS/Projekti/Dezinformacije/CatholiqDezinfo/Application/Source/output_tf_idf.xlsx")
#

  # Ensure the data has the necessary columns: document_id, word, tf_idf, idf

  # Preprocess the corpus data
  # Create a document-term matrix
  dtm_corpus <- tf_idf_corpus %>%
    select(document_id, word, tf_idf) %>%
    tidyr::pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0)

  # Remove the document_id column for matrix operations
  corpus_matrix <- as.matrix(dtm_corpus[,-1])

  # Normalize the corpus TF-IDF vectors
  corpus_norms <- sqrt(rowSums(corpus_matrix^2))
  corpus_matrix_norm <- corpus_matrix / corpus_norms

  # Define stop words
  stopwords_cro <- get_stopwords(language = "hr", source = "stopwords-iso")

  # Extend stop words
  my_stop_words <- tibble(
    word = c(
      "jedan","mjera", "može", "možete", "mogu", "kad", "sada", "treba", "ima", "osoba",
      "e","prvi", "dva","dvije","drugi",
      "tri","treći","pet","kod",
      "ove","ova",  "ovo","bez", "kod",
      "evo","oko",  "om", "ek",
      "mil","tko","šest", "sedam",
      "osam",   "čim", "zbog",
      "prema", "dok","zato", "koji",
      "im", "čak","među", "tek",
      "koliko", "tko","kod","poput",
      "baš", "dakle", "osim", "svih",
      "svoju", "odnosno", "gdje",
      "kojoj", "ovi", "toga",
      "ubera", "vozača", "hrvatskoj", "usluge", "godine", "više", "taksi", "taxi", "taksija", "kaže", "rekao", "19"," aee", "ae","bit.ly", "https", "one", "the"
    ),
    lexicon = "custom"
  )

  # Full set with diacritics
  cro_sw_full_d <- tibble(word = c("a","ako","ali","baš","bez","bi","bih","bila","bili","bilo","bio","bismo","bit","biti","bolje","bude","čak","čega","čemu","često","četiri","čime","čini","će","ćemo","ćete","ću","da","dakle","dalje","dan","dana","danas","dio","do","dobro","dok","dosta","dva","dvije","eto","evo","ga","gdje","god","godina","godine","gotovo","grada","i","iako","ići","ih","ili","im","ima","imaju","imali","imam","imao","imati","inače","ipak","isto","iz","iza","između","ja","jako","je","jedan","jedna","jednog","jednom","jednostavno","jednu","jer","joj","još","ju","ka","kad","kada","kaj","kako","kao","kaže","kod","koja","koje","kojeg","kojem","koji","kojih","kojim","kojima","kojoj","kojom","koju","koliko","kraju","kroz","li","malo","manje","me","među","međutim","mene","meni","mi","milijuna","mislim","mjesto","mnogo","mogao","mogli","mogu","moj","mora","možda","može","možemo","možete","mu","na","način","nad","naime","nakon","nam","naravno","nas","ne","neće","nego","neka","neke","neki","nekog","nekoliko","neku","nema","nešto","netko","ni","nije","nikad","nisam","nisu","ništa","niti","no","njih","o","od","odmah","odnosno","oko","on","ona","onda","oni","onih","ono","opet","osim","ova","ovaj","ovdje","ove","ovim","ovo","ovog","ovom","ovu","pa","pak","par","po","pod","poput","posto","postoji","pred","preko","prema","pri","prije","protiv","prvi","puno","put","radi","reći","s","sa","sad","sada","sam","samo","sati","se","sebe","si","smo","ste","stoga","strane","su","svaki","sve","svi","svih","svoj","svoje","svoju","što","ta","tada","taj","tako","također","tamo","te","tek","teško","ti","tih","tijekom","time","tko","to","tog","toga","toj","toliko","tom","tome","treba","tu","u","uopće","upravo","uvijek","uz","vam","vas","već","vi","više","vrijeme","vrlo","za","zapravo","zar","zato","zbog","zna","znači"),
                          lexicon = "diacritics")

  # Combine stop words
  stop_corpus <- bind_rows(stopwords_cro, my_stop_words, cro_sw_full_d)

  # Catholic-related word roots
  katoliq_rijeci <- data.frame(
    root = c("crkv", "svećenik", "katoličk", "pap", "svet", "bog", "biskup", "nadbiskup", "crkven", "blagoslov",
             "kardinal", "isus", "vatikan", "žup", "vjer", "franj", "župnik", "molitv", "božj", "vjernik",
             "mis", "grijeh", "biskupij", "benedikt", "krist", "prostor", "vlast", "vlč", "marij", "žid",
             "duhovn", "vatikansk", "nadbiskupij", "moli", "katedra", "najveć", "vjersk", "svećenic",
             "samostan", "fr", "vjeruj", "župn", "blagoslovljen", "katolik", "kršćansk", "gosp", "križ",
             "svećeničk", "orgulj", "oltar", "biskupsk", "dnevn", "isusov", "kristov", "blažen", "sveti",
             "franjevačk", "papinsk", "evanđelj", "katolic", "međugorj", "vjeronauk", "obraćenj", "kler",
             "kršćanstv", "spasenj", "milosrđ", "euharistij", "svjetovn", "nadbiskupijsk", "stepinčev",
             "posvet", "procesij", "biblij", "sinodaln", "svetac", "časn", "liturgij", "otajstv", "biblijsk",
             "fratar", "ministrant")
  )

  # Dezinformation-related word roots
  dezinfo_rijeci <- data.frame(
    root = c("djec", "prav", "slučaj", "žen", "pitanj", "držav", "obitelj", "zlostavljanj", "seksualn", "srpsk",
             "problem", "rat", "javn", "nek", "društv", "političk", "grijeh", "svoj", "odnos", "želi",
             "dobi", "ivan", "osta", "vlast", "ugovor", "imovin", "medij", "moli", "stepinc", "istin",
             "zakon", "odluk", "pobačaj", "potpun", "zemljišt", "društven", "celibat", "prič", "razlog",
             "dogodi", "javnost", "organizacij", "činjenic", "tvrd", "unutar", "učini", "mora", "kaza",
             "poziv", "svećenic", "državn", "izjav", "podijel", "moć", "predsjednik", "antisemitizm",
             "nitk", "skanda", "laž", "osobn", "otvoren", "srb", "pedofilij", "pokaza", "vjeruj", "zl",
             "nalaz", "novinar", "prilik", "situacij", "čud", "borb", "nekretnin", "unatoč", "izbor",
             "zahtjev", "optužb", "optužen", "služben", "vlastit", "tešk", "završi", "zaštit", "institucij",
             "kazn", "nacionaln", "osuđen", "prirod", "zločin", "dubok", "zatvor", "soton", "vraćen",
             "djevojk", "izgubi", "izvješć", "lažn", "ukidanj", "odbi", "logor", "pedofi", "vratit",
             "homoseksualc", "napad", "privatn", "zlostavlja", "kriv", "novc", "pavelić", "komunističk",
             "počini", "političar", "pr", "račun", "savjest", "međugorj", "obiteljsk", "pandemij",
             "pokor", "ponovn", "sekularn", "slik", "stan", "sveučilišt", "vjeronauk", "homoseksualnost",
             "moraln", "premijer", "prošlost", "strašn", "gradonačelnik", "građevinsk", "istraživanj",
             "oženjen", "sudsk", "ustašk", "štet", "darova", "dokaz", "egzorcist", "financijsk", "povra",
             "reakcij", "rh", "sestr", "sredstv", "tam", "holokaust", "istočn", "kler", "kršćanstv",
             "moćn", "muk", "novost", "parov", "muenchensk", "nasilj", "odgovornost", "okren", "ustaš",
             "uživa", "dječak", "gay", "oduzet", "ogromn", "ondj", "potpor", "potvrdi", "presta",
             "pristup", "rajčevc", "sudbin", "uvjet", "arhiv", "istospoln", "tez", "vlasništv",
             "zaboravi", "židov", "antisemitsk", "djetet", "konzervativn", "laganj", "pedofilsk",
             "svjetovn", "znanstven", "argument", "bogat", "bol", "bori", "jugoslav", "loš", "makn",
             "nepravd", "neprijatelj", "postupk", "pravomoćn", "progon", "stra", "strog", "đavl",
             "autoritet", "bož", "egzorcizm", "kaznen", "negativn", "objekt", "obvez", "orijentacij",
             "smrtn", "demonsk", "jugoslavensk", "krivnj", "obran", "spoln", "sumnj", "aktivist",
             "blag", "komunizm", "milijun", "nadležn", "naveden", "odgovorn", "oslobođen", "pad",
             "prijestup", "seks", "ubijen", "afer", "aktivizm", "identitet", "ideologij", "opasnost",
             "zlostavljač", "znanost", "istospoln", "lucifer", "zataškavanj")
  )

  # Reactive expression to process and analyze the input text
  analysis_result <- eventReactive(input$check, {
    req(input$text)

    # Initialize progress bar
    withProgress(message = 'Analiziranje teksta...', value = 0, {
      # Step 1: Preprocess the input text
      incProgress(0.1, detail = "Preprocesiranje teksta...")
      input_df <- data.frame(text = input$text)

      # Tokenize the text
      tokens <- input_df %>%
        unnest_tokens(word, text)

      # Remove stop words, numbers, single letters
      incProgress(0.2, detail = "Uklanjanje stop riječi i nevažnih znakova...")
      tokens <- tokens %>%
        mutate(word = tolower(word)) %>%  # Convert to lowercase
        anti_join(stop_corpus, by = "word") %>%
        mutate(word = gsub("\\d+", NA, word)) %>%
        mutate(word = gsub("^[a-zA-Zčćđšž]$", NA, word)) %>%
        filter(!is.na(word))

      # Step 2: Stemming
      incProgress(0.3, detail = "Stemmiranje riječi...")
      # Apply the write_tokens function to stem the words
      tokens <- tokens %>%
        mutate(stem = sapply(word, write_tokens))

      # Split the strings and extract the second element
      tokens <- tokens %>%
        mutate(stem = sapply(strsplit(stem, "\t"), `[`, 2))

      # Remove any NAs that may have been introduced
      tokens <- tokens %>%
        filter(!is.na(stem))

      # Check for Catholic-related terms
      incProgress(0.4, detail = "Provjera katoličkih pojmova...")
      catholic_terms_found <- tokens %>%
        filter(stem %in% katoliq_rijeci$root) %>%
        pull(stem) %>%
        unique()

      num_catholic_terms <- length(catholic_terms_found)
      is_catholic_article <- num_catholic_terms >= 2  # At least two terms

      # Check for Dezinformation-related terms
      incProgress(0.45, detail = "Provjera dezinformacijskih pojmova...")
      dezinfo_terms_found <- tokens %>%
        filter(stem %in% dezinfo_rijeci$root) %>%
        pull(stem) %>%
        unique()

      num_dezinfo_terms <- length(dezinfo_terms_found)
      has_dezinfo_terms <- num_dezinfo_terms >= 3  # At least three terms

      # Step 2.5: Extract sentences containing dezinfo words
      incProgress(0.475, detail = "Pronalaženje rečenica s dezinformacijskim pojmovima...")
      sentences <- tokenizers::tokenize_sentences(input$text)[[1]]
      sentences_df <- tibble(sentence = sentences)

      highlight_dezinfo_words <- function(sentence) {
        words <- unlist(strsplit(sentence, "\\s+"))
        words_lower <- tolower(words)
        stems <- sapply(words_lower, write_tokens)
        stems_clean <- sapply(strsplit(stems, "\t"), `[`, 2)
        stems_clean[is.na(stems_clean)] <- ""

        words_highlighted <- words
        contains_dezinfo <- FALSE
        for (i in seq_along(words)) {
          if (stems_clean[i] %in% dezinfo_rijeci$root) {
            words_highlighted[i] <- paste0("<span class='analysis-word'>", words[i], "</span>")
            contains_dezinfo <- TRUE
          }
        }
        list(
          highlighted_sentence = paste(words_highlighted, collapse = " "),
          contains_dezinfo = contains_dezinfo
        )
      }

      sentence_analysis <- lapply(sentences, highlight_dezinfo_words)
      sentences_df$highlighted_sentence <- sapply(sentence_analysis, `[[`, "highlighted_sentence")
      sentences_df$contains_dezinfo <- sapply(sentence_analysis, `[[`, "contains_dezinfo")

      # Extract sentences that contain dezinfo words
      dezinfo_sentences <- sentences_df %>%
        filter(contains_dezinfo) %>%
        pull(highlighted_sentence)

      # Step 3: Calculate term frequency (tf)
      incProgress(0.5, detail = "Izračun frekvencije pojmova...")
      term_freq <- tokens %>%
        count(stem, sort = TRUE) %>%
        rename(tf = n)

      # Step 4: Join with corpus idf values
      incProgress(0.6, detail = "Spajanje s IDF vrijednostima korpusa...")
      term_freq <- term_freq %>%
        left_join(tf_idf_corpus %>% select(word, idf) %>% distinct(), by = c("stem" = "word"))

      # Replace NA idf values with minimal value or a small constant
      min_idf <- min(tf_idf_corpus$idf, na.rm = TRUE)
      term_freq$idf[is.na(term_freq$idf)] <- min_idf

      # Step 5: Calculate TF-IDF for the input text
      incProgress(0.7, detail = "Izračunavanje TF-IDF za uneseni tekst...")
      term_freq <- term_freq %>%
        mutate(tf_idf = tf * idf)

      # Normalize the TF-IDF vector
      tf_idf_vector <- term_freq$tf_idf
      names(tf_idf_vector) <- term_freq$stem
      vector_norm <- sqrt(sum(tf_idf_vector^2))
      tf_idf_vector_norm <- tf_idf_vector / vector_norm

      # Ensure the vectors are compatible
      incProgress(0.8, detail = "Priprema za izračun kosinusne sličnosti...")
      common_words <- intersect(colnames(corpus_matrix_norm), names(tf_idf_vector_norm))
      if (length(common_words) == 0) {
        return(list(
          is_fake_news = FALSE,
          max_similarity = 0,
          message = "Nema zajedničkih riječi između unesenog teksta i korpusa.",
          is_catholic_article = is_catholic_article,
          catholic_terms_found = catholic_terms_found,
          has_dezinfo_terms = has_dezinfo_terms,
          dezinfo_terms_found = dezinfo_terms_found,
          dezinfo_sentences = dezinfo_sentences
        ))
      }

      # Subset the vectors to common words
      tf_idf_vector_norm <- tf_idf_vector_norm[common_words]
      corpus_subset <- corpus_matrix_norm[, common_words, drop = FALSE]

      # Calculate cosine similarity
      incProgress(0.9, detail = "Izračun kosinusne sličnosti...")
      cosine_similarities <- corpus_subset %*% tf_idf_vector_norm

      # Find the maximum cosine similarity
      max_similarity <- max(cosine_similarities)

      # Set a threshold for fake news detection
      threshold <- 0.4  # Adjust this threshold based on testing

      is_fake_news <- max_similarity > threshold && has_dezinfo_terms

      incProgress(1, detail = "Analiza završena.")

      list(
        is_fake_news = is_fake_news,
        max_similarity = max_similarity,
        similar_doc_id = dtm_corpus$document_id[which.max(cosine_similarities)],
        message = NULL,
        term_freq = term_freq,
        is_catholic_article = is_catholic_article,
        catholic_terms_found = catholic_terms_found,
        has_dezinfo_terms = has_dezinfo_terms,
        dezinfo_terms_found = dezinfo_terms_found,
        dezinfo_sentences = dezinfo_sentences
      )
    })
  })

  # Output for text feedback (e.g., text length warnings)
  output$text_feedback <- renderUI({
    req(input$text)
    text_length <- nchar(input$text)
    if (text_length > 0 && text_length < 50) {
      span("Tekst je prekratak za analizu. Unesite duži tekst.", style = "color: red;")
    } else {
      NULL
    }
  })

  # Output the result
  output$result <- renderUI({
    req(analysis_result())
    result <- analysis_result()
    if (!is.null(result$message)) {
      HTML(paste("<span style='color: red;'>", result$message, "</span>"))
    } else if (result$is_fake_news) {
      HTML(paste(
        "<div class='analysis-text' style='color: red;'><strong>Upozorenje:</strong> Tekst ima visoku sličnost s poznatim lažnim vijestima.</div>",
        "<div class='analysis-text'>Sličnost:", round(result$max_similarity * 100, 2), "%</div>"
      ))
    } else if (result$is_catholic_article) {
      HTML(paste(
        "<div class='analysis-text' style='color: green;'><strong>Rezultat:</strong> Tekst je povezan s katoličkim temama.</div>",
        "<div class='analysis-text'>Broj pronađenih katoličkih pojmova:", length(result$catholic_terms_found), "</div>"
      ))
    } else {
      HTML(paste(
        "<div class='analysis-text'><strong>Rezultat:</strong> Tekst ne pokazuje značajnu sličnost s poznatim lažnim vijestima niti s katoličkim temama.</div>"
      ))
    }
  })

  # Output detailed analysis
  output$detailed_analysis <- renderUI({
    req(analysis_result())
    result <- analysis_result()

    analysis_text <- ""

    if (result$is_catholic_article) {
      catholic_terms_formatted <- paste0("<span class='analysis-word'>", result$catholic_terms_found, "</span>", collapse = ", ")
      analysis_text <- paste0(
        analysis_text,
        "<p>U vašem tekstu pronađeni su sljedeći pojmovi povezani s katoličkim temama:</p>",
        "<p>", catholic_terms_formatted, "</p>"
      )
    } else {
      analysis_text <- paste0(analysis_text, "<p>Nisu pronađeni pojmovi povezani s katoličkim temama.</p>")
    }

    if (result$has_dezinfo_terms) {
      dezinfo_terms_formatted <- paste0("<span class='analysis-word'>", result$dezinfo_terms_found, "</span>", collapse = ", ")
      analysis_text <- paste0(
        analysis_text,
        "<p>Pronađeni su i sljedeći pojmovi povezani s dezinformacijama:</p>",
        "<p>", dezinfo_terms_formatted, "</p>"
      )

      # Display sentences containing dezinfo words
      if (length(result$dezinfo_sentences) > 0) {
        analysis_text <- paste0(
          analysis_text,
          "<h4>Rečenice u kojima su pronađeni dezinformacijski pojmovi:</h4>"
        )
        for (sentence in result$dezinfo_sentences) {
          analysis_text <- paste0(
            analysis_text,
            "<p class='analysis-sentence'>", sentence, "</p>"
          )
        }
      }
    } else {
      analysis_text <- paste0(analysis_text, "<p>Nisu pronađeni pojmovi povezani s dezinformacijama.</p>")
    }

    if (result$is_fake_news) {
      analysis_text <- paste0(
        analysis_text,
        "<p><strong>Upozorenje:</strong> Tekst ima visoku sličnost s poznatim lažnim vijestima.</p>"
      )
    } else {
      analysis_text <- paste0(
        analysis_text,
        "<p>Tekst ne pokazuje značajnu sličnost s poznatim lažnim vijestima.</p>"
      )
    }

    # Display top terms contributing to the similarity
    if (!is.null(result$term_freq)) {
      # Display top terms contributing to the similarity
      top_terms <- result$term_freq %>%
        arrange(desc(tf_idf)) %>%
        head(10)

      analysis_text <- paste0(
        analysis_text,
        "<h4>Najznačajniji pojmovi u tekstu:</h4>",
        "<ul>",
        paste0("<li>", top_terms$stem, " (TF-IDF: ", round(top_terms$tf_idf, 4), ")</li>", collapse = ""),
        "</ul>"
      )
    } else {
      analysis_text <- paste0(analysis_text, "<p>Nema dostupnih podataka za detaljnu analizu.</p>")
    }

    HTML(analysis_text)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
