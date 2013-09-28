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