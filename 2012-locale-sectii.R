################################################################################
#încărcare statistica sectii pre-alegeri
alegLocale2012 = read.csv("2012locale/1. statistici/_statistica_alegatori_pe_sectii_de_vot_locale_2012.csv"
                          , sep = ";", stringsAsFactors = FALSE)
################################################################################
colnames(alegLocale2012)[1] = "CIRC"
colnames(alegLocale2012)[4] = "SV"
colnames(alegLocale2012)[3] = "DEN_JUD"
colnames(alegLocale2012)[5] = "Numar.alegatori"
alegLocale2012[,3] = toupper(alegLocale2012[,3])
alegLocale2012$DEN_JUD = gsub("BACĂU", "BACAU", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("BRĂILA", "BRAILA", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("BUZĂU", "BUZAU", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("SĂLAJ", "SALAJ", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("DÎMBOVIŢA", "DÂMBOVIŢA", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("BUCUREŞTI", "MUNICIPIUL BUCUREŞTI", 
                              alegLocale2012$DEN_JUD)
alegLocale2012$Numar.alegatori = gsub("\\.","",alegLocale2012$Numar.alegatori)
alegLocale2012$Numar.alegatori = as.numeric(alegLocale2012$Numar.alegatori)
#test = merge(CJUtest[, c(1:6)], alegLocale2012, all.x = TRUE)
#
################################################################################
#încărcare denumiri secţii de votare locale 2012 şi schimbare ca la referend2012
numeSecLoc2012 = read.csv("2012locale/sv.csv", sep = ";", 
                          stringsAsFactors = FALSE)
################################################################################
# numeSecLoc2012$adresa = toupper(numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Â", "A", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Ă", "A", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Ţ", "T", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Ş", "S", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub (", STR\\. ", " STRADA: ", 
#                                   numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub (" TELEFON(.*)$", "", numeSecLoc2012$adresa)
colnames(numeSecLoc2012)[1] = "JUD"
colnames(numeSecLoc2012)[2] = "DEN_JUD"
colnames(numeSecLoc2012)[4] = "DEN_CIRC"
colnames(numeSecLoc2012)[5] = "CIRC"
colnames(numeSecLoc2012)[6] = "SV"
numeSecLoc2012$DEN_JUD = gsub("BACĂU", "BACAU", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("BRĂILA", "BRAILA", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("BUZĂU", "BUZAU", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("SĂLAJ", "SALAJ", numeSecLoc2012$DEN_JUD)