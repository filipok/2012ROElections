################################################################################
locale2012 = read.csv("2012locale/x_baza.csv", sep = ";", 
                      stringsAsFactors = FALSE)
################################################################################
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