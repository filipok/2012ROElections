#Clear everything
rm(list = ls(all = TRUE))

#Get data from Dropbox (zipped files) and unzip files

#You can get the raw data files from:
#https://www.dropbox.com/s/1zt0087rd3vnqwl/2012data.zip

#Download zip archive
source("generalFunctions.R")
dl_from_dropbox("2012data.zip", "1zt0087rd3vnqwl")

#Unzip files:
unzip("2012data.zip", overwrite = FALSE)

#Load local elections precinct data:
#Some pre-elections stats
alegLocale2012 = read.csv("2012locale/1. statistici/_statistica_alegatori_pe_sectii_de_vot_locale_2012.csv"
                          , sep = ";", stringsAsFactors = FALSE)
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
#Load local elections precinct names & format them acc to the referendum table
numeSecLoc2012 = read.csv("2012locale/sv.csv", sep = ";", 
                          stringsAsFactors = FALSE)
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

#Load local elections voting data:
locale2012 = read.csv("2012locale/x_baza.csv", sep = ";", 
                      stringsAsFactors = FALSE)
locale2012$DEN_CIRC = gsub("23.aug", "23 AUGUST", locale2012$DEN_CIRC) #eroare
locale2012$CODU = as.numeric(locale2012$CODU)
tururi = split(locale2012, locale2012$TUR)
rm(locale2012)
#separate first and second round
TUR1 = tururi[[1]]
TUR2 = tururi[[2]]
TUR2$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", TUR2$DEN_JUD) #unificare spelling
rm(tururi)
#separate first round: local council, county council, mayor and county president
tipuri = split(TUR1, TUR1$TIPPV)
rm(TUR1)
PCJ = tipuri[[1]]
PRI = tipuri[[2]]
CLO = tipuri[[3]]
CJU = tipuri[[4]]
rm(tipuri)

#determinare partidul noilor primari
newPrim = PRI[PRI$MAN_1 == 1,c("JUD","DEN_JUD", "CIRC", "DEN_CIRC", "SV", 
                               "CODU")]
TUR2Prim = TUR2[TUR2$TIPPV ==1 & TUR2$MAN_1 == 1, 
                c("JUD", "DEN_JUD", "CIRC", "DEN_CIRC", "SV", "CODU")]
newPrim = rbind(newPrim, TUR2Prim)
echivalareCoduri = read.csv("2012locale/coduri-partide.csv", sep = ";", 
                            stringsAsFactors = FALSE)
newPrim = merge (newPrim, echivalareCoduri, all.x = TRUE)
newPrim = merge (newPrim, unique(numeSecLoc2012[,c(1:5)]), 
                 all.x = TRUE)
rm(TUR2Prim)

#Load Siruta2008 (municipality codes)
#File source: http://earth.unibuc.ro/download/romania-seturi-vectoriale
siruta2008 = read.csv("siruta2008/localitati2008_iso8859_2.csv", sep = ",", 
                      stringsAsFactors = FALSE)
#Load referendum data
ref2012Sv = read.csv("2012referendum/2. rezultate/sv_date.csv", sep = ";", 
                     stringsAsFactors = FALSE)
colnames(ref2012Sv)[1] = "JUD"
colnames(ref2012Sv)[2] = "DEN_JUD"
colnames(ref2012Sv)[6] = "SV"
colnames(ref2012Sv)[5] = "DEN_CIRC_R"
ref2012Sv$DEN_JUD = gsub("BACĂU", "BACAU", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("BOTOSANI", "BOTOŞANI", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("BRĂILA", "BRAILA", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("CONSTANTA", "CONSTANŢA", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("SĂLAJ", "SALAJ", ref2012Sv$DEN_JUD) #spelling
#spelling
ref2012Sv$DEN_JUD = gsub("Municipiul Bucuresti", "MUNICIPIUL BUCUREŞTI", ref2012Sv$DEN_JUD)

#Load general elections votes:
numeSecParl2012 = read.csv("2012parlamentare/sv_bec.csv", sep = ";", 
                           stringsAsFactors = FALSE)
numeSecParl2012 = numeSecParl2012[numeSecParl2012$'Nr..Crt' != "" & 
                                    numeSecParl2012$'Nr..Crt' != "TOTAL",]
colnames(numeSecParl2012)[2:8] = c("JUD", "siruta", "DEN_CIRC", "SV", 
                                   "COL_CD", "COL_SE", "Adresa.parl")
parlamentare2012 = 
  read.csv("2012parlamentare/RezultateNivelSectieParlamentare2012.csv", 
           sep = ";", stringsAsFactors = FALSE, encoding  = "UTF-8")
colnames(parlamentare2012)[1:4] = c("JUD", "DEN_JUD", "SV", "DEN_CIRC")
camere = split(parlamentare2012, parlamentare2012$numeTipColegiu)
cdep = camere[[1]]
sen = camere[[2]]
rm(parlamentare2012, camere)

#Load general elections precinct data (requires the xlsReadWrite package)
source("getGenElePrecinct.R")