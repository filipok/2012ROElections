#În acest fişier sunt funcţiile de comparare a secţiilor de la parlamentare,
#referendum şi locale 2012. Sunt apelate în 2012-work-sectiidevotare.R.

testadresa <- function(lista1, lista2, dista){
  #funcţie de testare pe baza adresei
  for(n in 1:nrow(lista1)){ #pt fiecare secţie din localitate
    adrcurent <- lista1[n,8] #adresa secţiei curente
    if(nchar(adrcurent) !=0){ #unele adrese nu sunt deloc şi dădea eroare
      indgasit <- agrep(adrcurent, lista2[,7], 
                            value = FALSE, max.distance = dista)
      adrgasit <- lista2[indgasit,7]
      #iau doar răspunsurile unice (şi nenule) şi testez dif. nr. alegători 
      if(length(adrgasit) == 1){ 
        if(abs((lista2[indgasit, 9] / lista1[n,10]) - 1) < 0.25){
          #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
          lista1[n, 12] <- adrgasit
          lista1[n, 11] <- lista2[,6][indgasit]
          lista1[n, 13] <- lista2[,9][indgasit]
          #Secţia din a.ref alocată deja se scoate din listă
          lista2 <- lista2[-indgasit,]
        }
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}


testnraleg <- function(lista1, lista2, proc){
  #funcţie de testare pe baza numărului de alegători
  if(nrow(lista1) < 20){
    for(n in 1:nrow(lista1)){
      if(is.na(lista1[n, 11])){
        nrcurent <- lista1[n, 10]
        vectornrref <- lista2[,9]
        vectornrref <- abs((vectornrref/nrcurent)-1)
        if (sum(vectornrref < proc) == 1){ #condiţie suplimentară, să fie unul singur
          #Alocăm valoarea ce corespunde înregistrării cu diferenţă minimă - min(vectornrref)
          lista1[n,11:13] <- lista2[,c(6,7,9)][which(vectornrref == min(vectornrref)),]
          #Secţia din a.ref alocată deja se scoate din listă
          lista2 <- lista2[-which(vectornrref == min(vectornrref)),]
        }
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

testcomplex <- function(lista1, lista2, dista, proc){
  #Căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-1.5%
  for(n in 1:nrow(lista1)){
    if(is.na(lista1[n, 11])){
      #Mai întâi caut secţii cu nume asemănător
      adrcurent <- lista1[n,8] #adresa secţiei curente
      if(nchar(adrcurent) !=0){ #unele adrese nu sunt deloc şi dădea eroare
        indgasit <- agrep(adrcurent, lista2[,7], 
                              value = FALSE, max.distance = dista)
        adrgasit <- lista2[indgasit,7]
        #Acum avem un vector în indgasit
        #Iau doar răspunsurile nenule şi identice
        # şi găsesc înregistrarea unică cu nr. de secţie corespunzător
        if(length(adrgasit) > 0 & length(unique(adrgasit)) == 1){
          #print(paste("am trecut de IF-ul 1 cu length(unique) == 1 şi valoarea e",length(unique(adrgasit))))
          nrcurenta <- lista1[n,5] #numar secţie parl
          #Următorul IF verifică dacă numărul secţiei de la parlamentare se
          #regăseşte în vectorul de secţii găsite acum pentru referendum
          #De exemplu, dacă secţia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367), este ok.
          if(nrcurenta %in% lista2[,6][indgasit]){
            #Iar următorul IF verifică dacă numărul de alegători e stabil
            #de la parlamentare la referendum
            if(abs((lista2[lista2$SV.ref == 
                             nrcurenta, 9] / lista1[n,10])- 1) < proc){
              #Acum scriem echivalenţele
              #vectorul adrgasit e cu chestii identice şi aplicăm (unique)
              lista1[n, 12] <- unique(adrgasit) 
              #fiind acelaşi număr de secţie, scriem nr. de la parlamentare
              lista1[n, 11] <- lista1[n, 5] 
              #Scriem numărul de alegători, luat din înregistrarea cu 
              #codul siruta şi numărul de secţie corespunzătoare
              lista1[n, 13] <- lista2[lista2$SV.ref == lista1[n, 5],9] 
              #Secţia din a.ref alocată deja se scoate din listă
              lista2 <- lista2[lista2$SV.ref != lista1[n, 5],]
            }
          }
        }
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

testcomplex2 <- function(lista1, lista2, dista, proc){
  #Căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-1.5% 
  #Diferenţa faţă de funcţia testcomplex e că nu e musai ca adresele găsite să
  #fie identice (mai sunt erori de ortografie).
  for(n in 1:nrow(lista1)){
    if(is.na(lista1[n, 11])){
      #Mai întâi caut secţii cu nume asemănător
      adrcurent <- lista1[n,8] #adresa secţiei curente
      if(nchar(adrcurent) !=0){ #unele adrese nu sunt deloc şi dădea eroare
        indgasit <- agrep(adrcurent, lista2[,7], 
                              value = FALSE, max.distance = dista)
        adrgasit <- lista2[indgasit,7]
        #Acum avem un vector în indgasit
        #Iau doar răspunsurile nenule, dar nu neapărat identice
        if(length(adrgasit) > 0){
          #print(paste("am trecut de IF-ul 1 cu length(unique) == 1 şi valoarea e",length(unique(adrgasit))))
          nrcurenta <- lista1[n,5] #numar secţie parl
          #Următorul IF verifică dacă numărul secţiei de la parlamentare se
          #regăseşte în vectorul de secţii găsite acum pentru referendum
          #De exemplu, dacă secţia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367), este ok.
          if(nrcurenta %in% lista2[,6][indgasit]){
            #Iar următorul IF verifică dacă numărul de alegători e stabil
            #de la parlamentare la referendum
            if(abs((lista2[lista2$SV.ref == 
                             nrcurenta, 9] / lista1[n,10])- 1) < proc){
              #Acum scriem echivalenţele
              #fiind acelaşi număr de secţie, scriem nr. de la parlamentare
              lista1[n, 11] <- lista1[n, 5] 
              #Scriem numărul de alegători, luat din înregistrarea cu 
              #codul siruta şi numărul de secţie corespunzătoare
              lista1[n, 13] <- lista2[lista2$SV.ref == lista1[n, 5],9] 
              #adresa gasita
              lista1[n, 12] <- lista2[lista2$SV.ref == lista1[n, 5],7]
              #Secţia din a.ref alocată deja se scoate din listă
              lista2 <- lista2[lista2$SV.ref != lista1[n, 5],]
            }
          }
        }
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

testdecalat <- function(lista1, lista2, dista, proc){
  #Caut grupuri de secţii cu aceeaşi adresă, dar cu numere diferite, decalate
  for(n in 1:nrow(lista1)){
    #testez că nu avem deja datele din lista2 şi că adresa nu e nulă (dă eroare)
    if(is.na(lista1[n, 11]) & nchar(lista1[n,8]) !=0){
      #Caut secţii cu nume asemănător
      adrcurent <- lista1[n,8] #adresa secţiei curente
      indgasit <- agrep(adrcurent, lista2[,7], 
                            value = FALSE, max.distance = dista)
      adrgasit <- lista2[indgasit,7]
      #Am vectori cu adrese şi indici adrese, caut înregistrarea unică, dacă e
      if(length(adrgasit) > 0 & length(unique(adrgasit)) == 1){
        nrcurenta <- lista1[n,5] #numar secţie parl
        #Verificăm că avem acelaşi număr de secţii pentru adresa din lista1 şi
        #cea din lista 2
        sectiipar <- nrow(lista1[lista1$adresa.par == adrcurent & is.na(lista1$SV.ref.echiv),])
        sectiiref <- nrow(lista2[lista2$adresa.ref == unique(adrgasit),])
        if(sectiipar == sectiiref & sectiipar != 0){ #pot fi egale cu zero?
          #Avem acelaşi număr de secţii; vom presupune că ordinea e aceeaşi.
          temporar <- as.data.frame(matrix(rep(NA, 3*sectiipar),sectiipar))
          for(z in 1:sectiipar){
            #testăm fiecare pereche de secţii lista1-lista2 în privinţa
            #numărului de alegători
            numar1 <- lista1[lista1$adresa.par == adrcurent & is.na(lista1$SV.ref.echiv),][z, 10]
            numar2 <- lista2[lista2$adresa.ref == unique(adrgasit),][z, 9]
            if(abs((numar2 / numar1) - 1) < proc){
              #Putem introduce modificările. Adresa
              temporar[z, 2] <- unique(adrgasit)
              #Numărul secţiei
              temporar[z, 1] <- lista2[lista2$adresa.ref == unique(adrgasit),][z, 6]
              #Numărul de alegători 
              temporar[z, 3] <- numar2
            }
          }
          #Acum copiem df temporar
          lista1[lista1$adresa.par == adrcurent & is.na(lista1$SV.ref.echiv), 11:13] <- temporar
          #Secţiile din lista2 alocate deja se scot din listă
          #Atenţie, nu toate cu adresa gasita, ci doar alea selectate în FOR-ul de mai sus!
          lista2[lista2$adresa.ref == unique(adrgasit),][!is.na(temporar$V1),] <- NA
          lista2 <- lista2[!is.na(lista2$JUD),]            
        }
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

testdecalat2 <- function(lista1, lista2, dista, proc){
  #Caut grupuri de secţii cu aceeaşi adresă, dar cu numere diferite, decalate
  #Diferenţa faţă de testdecalat() e că nu e musai ca adresele găsite să fie
  #unice (mai sunt typos).
  for(n in 1:nrow(lista1)){
    #testez că nu avem deja datele din lista2 şi că adresa nu e nulă (dă eroare)
    if(is.na(lista1[n, 11]) & nchar(lista1[n,8]) !=0){
      #Caut secţii cu nume asemănător
      adrcurent <- lista1[n,8] #adresa secţiei curente
      indgasit <- agrep(adrcurent, lista2[,7], 
                            value = FALSE, max.distance = dista)
      adrgasit <- lista2[indgasit,7]
      #Am vectori cu adrese şi indici adrese
      if(length(adrgasit) > 0){
        nrcurenta <- lista1[n,5] #numar secţie parl
        #Verificăm că avem acelaşi număr de secţii pentru adresa din lista1 şi
        #cea din lista 2
        sectiipar <- nrow(lista1[lista1$adresa.par == adrcurent & is.na(lista1$SV.ref.echiv),])
        sectiiref <- nrow(lista2[lista2$adresa.ref %in% unique(adrgasit),])
        if(sectiipar == sectiiref & sectiipar != 0){ #pot fi egale cu zero?
          #Avem acelaşi număr de secţii; vom presupune că ordinea e aceeaşi.
          temporar <- as.data.frame(matrix(rep(NA, 3*sectiipar),sectiipar))
          for(z in 1:sectiipar){
            #testăm fiecare pereche de secţii lista1-lista2 în privinţa
            #numărului de alegători
            numar1 <- lista1[lista1$adresa.par == adrcurent & is.na(lista1$SV.ref.echiv),][z, 10]
            numar2 <- lista2[lista2$adresa.ref %in% unique(adrgasit),][z, 9]
            if(abs((numar2 / numar1) - 1) < proc){
              #Putem introduce modificările. Adresa
              temporar[z, 2] <- adrgasit[z]
              #Numărul secţiei
              temporar[z, 1] <- lista2[lista2$adresa.ref %in% unique(adrgasit),][z, 6]
              #Numărul de alegători 
              temporar[z, 3] <- numar2
            }
          }
          #Acum copiem df temporar
          lista1[lista1$adresa.par == adrcurent & is.na(lista1$SV.ref.echiv), 11:13] <- temporar
          #Secţiile din lista2 alocate deja se scot din listă
          #Atenţie, nu toate cu adresa gasita, ci doar alea selectate în FOR-ul de mai sus!
          lista2[lista2$adresa.ref %in% unique(adrgasit),][!is.na(temporar$V1),] <- NA
          lista2 <- lista2[!is.na(lista2$JUD),]            
        }
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

nrsecidentic <- function(lista1, lista2, maxim){
  #funcţie pentru numere de secţie identice verificate manual
  #maxim e nr. de secţie maxim pentru care se face echivalare
  for(n in 1:nrow(lista1)){
    if(lista1[n,5] <= maxim){
      lista1[n, 12] <- lista2[lista2$SV.ref == lista1[n,5],7] #adresa
      lista1[n, 11] <- lista2[lista2$SV.ref == lista1[n,5],6] #SV
      lista1[n, 13] <- lista2[lista2$SV.ref == lista1[n,5],9] #nr.aleg
      #Secţia din a.ref alocată deja se scoate din listă
      lista2 <- lista2[lista2$SV.ref != lista1[n,5],]
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

corectare <- function(sir, s1old, s1new){
  #mutare sectie s2 de la s1old la s1new
  if(is.na(lista1[lista1$SV.par == s1new,11]) & 
       !is.na(lista1[lista1$SV.par == s1old,11])){
    lista1[lista1$SV.par == s1new,11:13] <- lista1[lista1$SV.par == s1old,11:13]
    lista1[lista1$SV.par == s1old,11:13] <- NA
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

completare <- function(sir, s1, s2){
  #completare manuală secţie s2 în s1
  if(is.na(lista1[lista1$SV.par == s1new, 11]) & 
       !is.na(lista2[lista2$SV.ref == s2, 6])){
    lista1[lista1$SV.par == s1new, 11:13] <- lista2[lista2$SV.ref == s2, c(6:7, 9)]
    lista2 <- lista2[lista2$SV.ref != s2,]
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}