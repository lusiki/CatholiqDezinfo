install.packages("tm")
install.packages("textTinyR")
library(tm)
library(textTinyR)

# Define your lists as full data frames

# First List of Words and Inflections
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
           "fratar", "ministrant"))

# Second List of Words and Inflections
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
           "zlostavljač", "znanost", "istospoln", "lucifer", "zataškavanj"))

# Save to files for later use
write.csv(vectors_list1, "vectors_list1.csv")
write.csv(vectors_list2, "vectors_list2.csv")