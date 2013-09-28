#Import Siruta2008

#Fişierul este de la http://earth.unibuc.ro/download/romania-seturi-vectoriale
siruta2008 = read.csv("siruta2008/localitati2008_iso8859_2.csv", sep = ",", 
                      stringsAsFactors = FALSE)
#Scurtăm codul Siruta, pentru a corespunde formatului de la ROAEP
siruta2008[4] = sapply(siruta2008[4], function(qqq) strtrim(qqq,nchar(qqq)-1))