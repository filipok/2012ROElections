#în continuare definesc câteva funcţii pentru prelucrare
#în principal, fac un reshape de la format long la wide

TransformTabelParl = function (x, indepAgreg = FALSE){
  #funcţie de transformare a datelor de la BEC în tabel de tip "wide"
  #1. creez baza tabelului cu datele de participare
  tabelTransf = x[1:19]
  tabelTransf = unique(tabelTransf)
  #2. fac tabel separat cu independenţii
  workIndep = x[x$numeCompetitor == "NULL",] 
  #3. fac tabel cu datele privind voturile partidelor (doar coloanele necesare)
  workPartide = x[x$numeCompetitor != "NULL", 
                  c("JUD", "DEN_CIRC", "SV", "numeCompetitor", 
                    "VoturiCandidat")]
  workPartide = reshape(workPartide, v.names = "VoturiCandidat", 
                        idvar = c("JUD", "DEN_CIRC", "SV"), 
                        timevar = "numeCompetitor", direction = "wide")
  #4. unesc tabel cu date  participare cu tabel cu voturi partide
  tabelTransf = merge(tabelTransf, workPartide, all.x = TRUE, sort = FALSE)
  rm(workPartide)
  #5. acum trebuie să adăugăm una sau mai multe coloane pentru independenţi
  #avem două cazuri, cu sau fără agregare independenţi
  if(indepAgreg == FALSE) {
    #numele coloanelor de independenţi: DEN_JUD + COLEGIU + NUME_CANDIDAT
    indepLabels = apply(workIndep, 1, function(qqq) paste(qqq[2],"_", qqq[5], 
                                                          "_", qqq[20], 
                                                          sep = ""))  
    workIndep = cbind(workIndep, indepLabels)
    workIndep = workIndep[, c("JUD", "SV", "VoturiCandidat", "indepLabels")]
    workIndep = reshape(workIndep, v.names = "VoturiCandidat", 
                        idvar = c("JUD", "SV"), timevar = "indepLabels", 
                        direction = "wide")
    #printăm număr independenţi, aşa, de verificare
    indepLabels = unique(indepLabels)
    print(paste("Avem", length(indepLabels), "independenti."))
  }
  else {
    #dacă vrem să agregăm independenţii
    workIndep = aggregate(workIndep$VoturiCandidat, 
                          by = list(workIndep$JUD, workIndep$SV), sum)
    colnames(workIndep) = c("JUD", "SV","INDEPENDENTI")
  }
  #unim tabelul de independenţi la tabelul general
  tabelTransf = merge(tabelTransf, workIndep, sort = TRUE, all.x = TRUE)
  rm(workIndep)
  #6. finalizare
  #eliminăm "VoturiCandidat." din numele coloanelor
  colnames(tabelTransf) = gsub("VoturiCandidat.", "", colnames(tabelTransf))
  #înlocuim NA cu zero
  tabelTransf[is.na(tabelTransf)] = 0
  #adăugăm adresa secţiei
  tabelTransf = merge (tabelTransf, numeSecParl2012[,c(2, 5, 8)], 
                       all.x = TRUE)
  tabelTransf
}

CumulatCirc = function(x){
  circ = aggregate(subset(x, select = -c(JUD, SV, DEN_CIRC, DEN_JUD,
                                         numarColegiu, numeTipColegiu, 
                                         Adresa.parl)), 
                   by = list(x$JUD, x$DEN_JUD, x$DEN_CIRC), sum)
  colnames(circ)[1:3] = c("JUD", "DEN_JUD", "DEN_CIRC")
  circ = circ[ order(as.numeric(circ[,1]), circ[,3]), ]
  circ
}

CumulatColegiu = function(x){
  colegiu = aggregate(subset(x, select = -c(JUD, SV, DEN_CIRC, DEN_JUD,
                                            numarColegiu, numeTipColegiu, 
                                            Adresa.parl)), 
                      by = list(x$JUD, x$DEN_JUD, x$numarColegiu, 
                                x$numeTipColegiu), sum)
  colnames(colegiu)[1:4] = c("JUD", "DEN_JUD", "numarColegiu", 
                             "numeTipColegiu")
  colegiu = colegiu[ order(as.numeric(colegiu[,1]), colegiu[,3]), ]
  colegiu
}

CumulatJudet = function(x){
  judet = aggregate(subset(x, select = -c(JUD, SV, DEN_CIRC, DEN_JUD,
                                          numarColegiu, numeTipColegiu, 
                                          Adresa.parl)), 
                    by = list(x$JUD, x$DEN_JUD), sum)
  colnames(judet)[1:2] = c("JUD", "DEN_JUD")
  judet
}