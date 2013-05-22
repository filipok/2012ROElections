library("data.table")

testadresaDT <- function(lista1, lista2, distanta){
  #funcţie de testare pe baza adresei pentru data.tables
  for(n in 1:nrow(lista1)){ #pt fiecare secţie din localitate
    adresa.curenta <- lista1[n,adresa.par] #adresa secţiei curente
    if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
      indice.gasit <- agrep(adresa.curenta, lista2[,adresa.ref], 
                            value = FALSE, max.distance = distanta)
      adresa.gasita <- lista2[indice.gasit, adresa.ref]
      if(length(adresa.gasita) == 1){ #iau doar răspunsurile unice (şi nenule)
        #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
        set(lista1, n, 12L, adresa.gasita)
        set(lista1, n, 11L, lista2[,SV.ref][indice.gasit])
        set(lista1, n, 13L, lista2[,ta][indice.gasit])
        #Secţia din a.ref alocată deja se scoate din listă
        lista2 <- subset(lista2, SV.ref != lista1[n, SV.ref.echiv])
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

testadresaMT <- function(lista1, lista2, distanta){
  #funcţie de testare pe baza adresei pentru matrici
  lista1 <- as.matrix(lista1)
  lista2 <- as.matrix(lista2)
  for(n in 1:nrow(lista1)){ #pt fiecare secţie din localitate
    adresa.curenta <- lista1[n,8] #adresa secţiei curente
    if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
      indice.gasit <- agrep(adresa.curenta, lista2[,7], 
                            value = FALSE, max.distance = distanta)
      adresa.gasita <- lista2[indice.gasit,7]
      if(length(adresa.gasita) == 1){ #iau doar răspunsurile unice (şi nenule)
        #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
        lista1[n, 12] <- adresa.gasita
        lista1[n, 11] <- lista2[,6][indice.gasit]
        lista1[n, 13] <- lista2[,9][indice.gasit]
        #Secţia din a.ref alocată deja se scoate din listă
        lista2 <- lista2[-indice.gasit, , drop = FALSE]
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}

testadresaSA <- function(dubla, distanta){
  #funcţie de testare pe baza adresei
  lista1 <- dubla[[1]]
  lista2 <- dubla[[2]]
  for(n in 1:nrow(lista1)){ #pt fiecare secţie din localitate
    adresa.curenta <- lista1[n,8] #adresa secţiei curente
    if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
      indice.gasit <- agrep(adresa.curenta, lista2[,7], 
                            value = FALSE, max.distance = distanta)
      adresa.gasita <- lista2[indice.gasit,7]
      if(length(adresa.gasita) == 1){ #iau doar răspunsurile unice (şi nenule)
        #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
        lista1[n, 12] <- adresa.gasita
        lista1[n, 11] <- lista2[,6][indice.gasit]
        lista1[n, 13] <- lista2[,9][indice.gasit]
        #Secţia din a.ref alocată deja se scoate din listă
        lista2 <- lista2[-indice.gasit,]
      }
    }
  }
  dubla <- vector ("list", 2)
  dubla[[1]] <- lista1
  dubla[[2]] <- lista2
  dubla
}