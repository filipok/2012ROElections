rm(list = ls(all = TRUE))
source("2012-functiigenerale.R")
#Apel cod pentru locale, referendum şi parlamentare
source("2012-locale.R")
source("2012-referendum.R")
source("2012-parlamentare.R")

#Un data frame pentru fiecare listă de secţii de votare, incl. nr. de alegători
a.loc <- merge(nume.sec.loc.2012, 
               aleg.loc.2012[c("DEN_JUD", "CIRC", 
                               "SV", "Numar.alegatori")], 
               sort = FALSE)
a.par <- nume.sec.parl.2012
a.ref <- ref.2012.sv[1:9]

#Şterg datele care nu-mi folosesc pe moment
rm(CJU, CLO, PCJ, PRI, TUR2, aleg.loc.2012, cdep, cdep.circ, cdep.colegiu, 
   cdep.judet, cdep.sv, echivalare.coduri, new.prim, nume.sec.loc.2012,
   nume.sec.parl.2012, ref.2012.circ, ref.2012.circ.pro, ref.2012.jud,
   ref.2012.jud.pro, ref.2012.nat, ref.2012.sv, ref.2012.sv.nat, 
   ref.2012.sv.pro, sen, sen.circ, sen.colegiu, sen.judet, sen.sv, AbsolutCirc, 
   AbsolutJudet, AbsolutSv, CumulatCirc, CumulatColegiu, CumulatJudet, Procente,
   Procente.Ref, SimpLocale2012, TransformTabel, TransformTabelParl)

Ghilimele = function(x){
  x <- gsub("„", "=",x)
  x <- gsub("”", "=",x)
  x <- gsub("“", "=",x)
  x <- gsub(",,", "=",x)
  x <- gsub("’’", "=",x)
  x <- gsub("\"", "=",x)
  x  
}

Prelucrari = function(x){
  x <- chartr("ăâîşţĂÂÎŞŢ", "aaistaaist", x) #elimin diacriticele
  x <- toupper(x) #majuscule
  x <- gsub("^( +)", "", x)#spaţii de la începutul adresei
  x <- gsub(" ( +)", " ", x)#spaţii duble
  x <- Ghilimele(x)
  x <- gsub("([0-9A-Z])=([0-9A-Z])", "\\1 =\\2", x) #ghilimele fără spaţii înainte
}

#Prelucrari adresa
a.par$Adresa.parl <- Prelucrari(a.par$Adresa.parl)
a.loc$adresa <- Prelucrari(a.loc$adresa)
a.ref$adresa <- Prelucrari(a.ref$adresa)

#de individualizat coloanele SV şi adresa
colnames(a.par)[5] <- "SV.par"
colnames(a.loc)[3] <- "SV.loc"
colnames(a.ref)[6] <- "SV.ref"
colnames(a.loc)[7] <- "adresa.loc"
colnames(a.par)[8] <- "adresa.par"
colnames(a.ref)[7] <- "adresa.ref"

#Prelucrări DEN_CIRC
a.par$DEN_CIRC <- Prelucrari(a.par$DEN_CIRC)
a.ref$DEN_CIRC_R <- Prelucrari(a.ref$DEN_CIRC_R)
a.loc$DEN_CIRC <- Prelucrari(a.loc$DEN_CIRC)

#Modificări coduri Siruta
a.ref[a.ref$siruta == 81193, "siruta"] <- 81184
a.ref[a.ref$siruta == 78766, "siruta"] <- 78748
a.ref[a.ref$siruta == 78720, "siruta"] <- 78711
a.ref[a.ref$siruta == 999, "siruta"] <- 275

#scoatem judeţul 43 din parlamentare şi referendum
a.par <- a.par[a.par$JUD != 43,]
a.ref <- a.ref[a.ref$JUD != 43,]

#Urmează verificările propriu-zise
#Folosesc ca bază parlamentarele, am cele mai multe secţii în RO

timp1<- Sys.time()
baza <- a.par
arefwork <- a.ref
coloane <- matrix(rep(NA, 18456 * 3), ncol = 3)
baza <- cbind(baza, coloane)
rm(coloane)
colnames(baza)[11:13] <- c("SV.ref.echiv", "Adresa.ref.echiv","Aleg.ref.echiv")
baza[,11] <- as.integer(NA)
baza[,12] <- as.character(NA)
baza[,13] <- as.integer(NA)

rm(statss)
#coeficienţii pentru căutare
distanta <- 0.3
distanta2 <- 0.4
distanta3 <- 0.5
procent <- 0.05

#bucată de activat dacă folosim data.table
# baza <- as.data.table(baza)
# arefwork <- as.data.table(arefwork)

#splitare date pe siruta
listapar <- split(baza, as.numeric(baza$siruta))
listaref <- split(arefwork, arefwork$siruta)

timp3<- Sys.time()
pb <- txtProgressBar(min = 0, max = length(unique(a.par$siruta)), style = 3)
source("2012-work-functiicomparare.R") #încărcăm funcţiile apelate în loop
for(i in 1:length(unique(a.par$siruta))){ #pt fiecare localitate facem câteva teste
  setTxtProgressBar(pb, i)
  #1. Acum testăm în funcţie de adresă
  listoi <- testadresa(listapar[[i]], listaref[[i]], distanta)
  #2. Acum testăm în funcţie de numărul de alegători
  listoi <- testnraleg(listoi[[1]], listoi[[2]], procent)
  #3. Acum testăm iar în funcţie de adresă,cu un coeficient de distanţă mai mare
  listoi <- testadresa(listoi[[1]], listoi[[2]], distanta2)
  #4. Acum căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-5%
  listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta, procent)
  #5. Mai rulez încă o dată, cu distanta2=0.4
  listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta2, procent)
  #6.aici încerc să identific grupurile de secţii cu aceeaşi adresă, dar care
  #nu au acelaşi număr, fiind decalate
  listoi <- testdecalat(listoi[[1]], listoi[[2]], distanta, procent)
  #7.Mai rulez încă o dată punctul #6 cu distanta = 0.2
  listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.2, procent)
  #8. Căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/5%
  #(diferenţa faţă de cealaltă funcţie testcomplex e că nu cere ca adresele
  #găsite să fie identice, existând mici typos uneori)
  listoi <- testcomplex2(listoi[[1]], listoi[[2]], distanta, procent)
  
  #9. Căutăm secţii cu adresă asemănătoare, nr. identic, alegători +/-10%
  listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta, 0.1)
  #10.Identific grupurile de secţii cu aceeaşi adresă, dar care
  #nu au acelaşi număr, fiind decalate; acum procent = 10%
  listoi <- testdecalat(listoi[[1]], listoi[[2]], distanta, 0.1)
  #11.Identific grupurile de secţii cu aceeaşi adresă, dar care nu au acelaşi
  #număr, fiind decalate; adresele găsite nu e musai să fie identice (typos)
  listoi <- testdecalat2(listoi[[1]], listoi[[2]], distanta, procent)
  #12. Teste speciale pentru codurile siruta cu cele mai multe secţii negăsite
  if(listapar[[i]][1,3] == 54975){
    listoi <- testdecalat2(listoi[[1]], listoi[[2]], distanta, 0.3)
  }
  if(listapar[[i]][1,3] %in% c(95060, 155243, 60419, 179169)){
    listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta, 0.3)
  }
  if(listapar[[i]][1,3] %in% c(40198, 9262, 143450, 114319, 42682, 20297, 106318)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], distanta, 0.3)
  }
  if(listapar[[i]][1,3] %in% c(130534, 13169, 167473)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.5, procent)
  }
  if(listapar[[i]][1,3] %in% c(197196, 20563, 106318, 114319, 159614, 60847, 
                               139704, 1017, 130534)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.5, 0.5)
  }
  if(listapar[[i]][1, 3] %in% c(13169)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.7, procent)
  }
    
  #13. Test special pentru cele cu mai puţin de 10 negăsite, unde numărul de
  #secţii negăsite de la lista1 e acelaşi cu cel de la lista2
  if(nrow(listoi[[2]]) < 11 & nrow(listoi[[2]]) == nrow(listoi[[1]][is.na(listoi[[1]]$Adresa.ref.echiv),])){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.5, 0.3)
  }
  
  #14. Teste speciale pentru cele cu 2 sau 3 negăsite; distanţă mică, fiindcă au
  #nume asemănătoare
  if(nrow(listoi[[2]]) %in% c(2, 3) & nrow(listoi[[2]]) == nrow(listoi[[1]][is.na(listoi[[1]]$Adresa.ref.echiv),])){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.1, procent)
  }
  if(nrow(listoi[[2]]) %in% c(2, 3) & nrow(listoi[[2]]) == nrow(listoi[[1]][is.na(listoi[[1]]$Adresa.ref.echiv),])){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.05, procent)
  }
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
}

close(pb)
timp4<- Sys.time()


baza <- do.call("rbind", listapar)
arefwork <- do.call("rbind", listaref)
rm(listapar, listaref)


#completări manuale acolo unde lipsesc 1 secţie la parlamentare şi 1 secţie la 
#referendum; matricele sunt introduse byrow; la corma primul număr este codul 
#siruta, al doilea este secţia de unde este scoasă înregistrarea, iar al treilea
#este secţia unde este introdusă înregistrarea; la compa, prima cifră e siruta, 
#a doua este numărul secţiei de la parlamentare, iar a treia este numărul
#secţiei de la referendum
corma <- matrix(c(103764, 154, 152,
                  119527, 519, 522,
                  122187, 235, 236,
                  122187, 238, 235,
                  125016, 474, 473,
                  136642, 112, 113,
                  136642, 111, 112,
                  139358, 327, 329,
                  14165, 208, 210,
                  17254,376, 378,
                  18475, 434, 435,
                  19392, 482, 484,
                  39417, 384, 388,
                  40492, 272, 273,
                  45101, 152, 154,
                  50102, 402, 401
), ncol = 3, byrow = TRUE)
compa <- matrix(c(103764, 154, 153,
                  108473, 349, 322,
                  109504, 426, 395,
                  115389, 256, 244,
                  119527, 519, 495,
                  120922, 311, 313,
                  122187, 238, 241,
                  122908, 301, 303,
                  123175, 325, 327,
                  123790, 374, 374,
                  125016, 474, 474,
                  131407, 237, 237,
                  131988, 291, 290,
                  133722, 448, 445,
                  135725, 582, 577,
                  136107, 607, 602,
                  136642, 111, 115,
                  137764, 206, 207,
                  139358, 327, 328,
                  14165, 208, 208,
                  144795, 289, 289,
                  147465, 231, 231,
                  149227, 159, 159,
                  149539, 388, 386,
                  149682, 399, 398,
                  150935, 497, 497,
                  150980, 506, 506,
                  151022, 510, 510,
                  151736, 187, 186,
                  153623, 239, 238,
                  15554, 288, 286,
                  15652, 293, 291,
                  158243, 512,507,
                  158314, 289, 288,
                  160172, 106, 102,
                  161829, 100, 100,
                  162194, 153, 153,
                  163734, 264, 264,
                  16908, 349, 349,
                  169306, 196, 198,
                  17254, 376, 375,
                  174290, 422, 426,
                  18475, 434, 430,
                  19392, 482, 477,
                  20876, 134, 132,
                  26289, 626, 602,
                  27383, 246, 246,
                  27971, 287, 285,
                  28665, 336, 334,
                  2915, 121, 120,
                  30229, 467, 465,
                  30568, 499, 497,
                  31422, 585, 585,
                  39417, 384, 381,
                  40492, 272, 272,
                  45101, 152, 152,
                  49545, 364, 365,
                  50102, 402, 403,
                  51010, 87, 86,
                  5103, 286, 285,
                  60534, 343, 367,
                  61871, 444, 466,
                  6592, 353, 352,
                  71956, 317, 317,
                  75203, 184, 184,
                  83133, 55, 50,
                  87843, 262, 256,
                  91535, 494, 491,
                  95355, 269, 269,
                  98051, 514, 514,
                  98435, 540, 540,
                  99370, 625, 624
), ncol = 3, byrow = TRUE)
for(i in 1:nrow(corma)){
  print(i)
  baza <- corectare(baza, corma[i,1],corma[i,2],corma[i,3])
}
for(i in 1:nrow(compa)){
  listoi <- completare(baza, arefwork, compa[i,1], compa[i, 2], compa[i, 3])
  baza <- listoi[[1]]
  arefwork <-listoi[[2]]
}

#completări manuale acolo unde lipsesc câte 2 secţii
corma <- matrix(c(121652, 195, 196,
                  137069, 144, 147,
                  142426, 252, 253,
                  163002, 206, 208,
                  21418, 269, 267,
                  51118, 138, 139,
                  75766, 247, 245,
                  76255, 295, 297,
                  85840, 246, 248), ncol = 3, byrow = TRUE)
compa <- matrix(c(105570, 215, 209,
                  105570, 216, 210,
                  10827, 261, 253,
                  10827, 262, 254,
                  108669, 374, 344,
                  108669, 375, 345,
                  108963, 156, 132,
                  108963, 158, 134,
                  10943, 274, 266,
                  10943, 275, 267,
                  114453, 523, 499,
                  114453, 524, 500,
                  116796, 367, 349,
                  116796, 368, 350,
                  121466, 177, 179,
                  121466, 179, 181,
                  121652, 195, 197,
                  121652, 199, 201,
                  121796, 210, 212,
                  121796, 211, 213,
                  122347, 250, 253,
                  122347, 251, 254,
                  122702, 290, 292,
                  122702, 291, 293,
                  124411, 421, 421,
                  124411, 422, 422,
                  124885, 463, 463,
                  124885, 465, 465,
                  125962, 136, 136,
                  125962, 137, 137,
                  126978, 193, 193,
                  126978, 194, 194,
                  127224, 217, 217,
                  127224, 218, 218,
                  127251, 221, 221,
                  127251, 222, 222,
                  127322, 227, 227,
                  127322, 228, 228,
                  127493, 241, 241,
                  127493, 242, 242,
                  128524, 285, 285,
                  128524, 286, 286,
                  128588, 291, 291,
                  128588, 292, 292,
                  129460, 332, 332,
                  129460, 333, 333,
                  130981, 187, 185,
                  130981, 188, 186,
                  132075, 295, 293,
                  132075, 298, 296,
                  133278, 405, 402,
                  133278, 406, 403,
                  133330, 420, 417,
                  133330, 421, 418,
                  135949, 598, 593,
                  135949, 603, 598,
                  137069, 144, 145,
                  137069, 146, 147,
                  137675, 197, 199,
                  137675, 199, 200,
                  139811, 52, 41,
                  139811, 55, 44,
                  140627, 116, 104,
                  140627, 118, 106,
                  142426, 251, 239,
                  142426, 252, 240,
                  147633, 127, 127,
                  147633, 128, 128,
                  14851, 245, 244,
                  14851, 246, 245,
                  148612, 144, 144,
                  148612, 145, 145,
                  149290, 360, 359,
                  149290, 361, 360,
                  149316, 365, 364,
                  149316, 366, 365,
                  149414, 380, 379,
                  149414, 382, 381,
                  151978, 89, 89,
                  151978, 90, 90,
                  153151, 194, 193,
                  153151, 196, 195,
                  156106, 342, 340,
                  156106, 345, 343,
                  158528, 524, 519,
                  158528, 525, 520,
                  158779, 547, 540,
                  158779, 548, 541,
                  158859, 556, 549,
                  158859, 557, 550,
                  159785, 77, 73,
                  159785, 80, 76,
                  159847, 81, 77,
                  159847, 83, 79,
                  160261, 110, 106,
                  160261, 112, 108,
                  160476, 125, 121,
                  160476, 126, 122,
                  160831, 150, 146,
                  160831, 151, 147,
                  161794, 54, 54,
                  161794, 84, 84,
                  161945, 35, 35,
                  161945, 36, 36,
                  163002, 206, 206,
                  163002, 207, 207,
                  16454, 328, 326,
                  16454, 329, 327,
                  174254, 418, 422,
                  174254, 419, 423,
                  175055, 91, 90,
                  175055, 93, 92,
                  178117, 303, 301,
                  178117, 304, 302,
                  180064, 106, 102,
                  180064, 107, 103,
                  18741, 451, 447,
                  18741, 452, 448,
                  19114, 466, 462,
                  19114, 467, 463,
                  19560, 486, 481,
                  19560, 487, 482,
                  21418, 268, 255,
                  21418, 269, 256,
                  21855, 304, 290,
                  21855, 305, 291,
                  24631, 518, 498,
                  24631, 519, 499,
                  28610, 333, 331,
                  28610, 334, 332,
                  28816, 348, 346,
                  28816, 349, 347,
                  30069, 454, 452,
                  30069, 455, 453,
                  31011, 535, 533,
                  31011, 536, 534,
                  35731, 48, 57,
                  35731, 59, 59,
                  36569, 180, 179,
                  36569, 181, 180,
                  37280, 127, 127,
                  37280, 128, 128,
                  37397, 248, 247,
                  37397, 249, 248,
                  37547, 258, 257,
                  37547, 261, 260,
                  38679, 341, 338,
                  38679, 342, 339,
                  38893, 352, 350,
                  38893, 354, 351,
                  39328, 376, 373,
                  39328, 377, 374,
                  39391, 382, 379,
                  39391, 383, 380,
                  40278, 210, 210,
                  40278, 211, 211,
                  40526, 283, 283,
                  40526, 284, 284,
                  44391, 266, 264,
                  44391, 267, 265,
                  51118, 137, 136,
                  51118, 138, 137,
                  51387, 167, 166,
                  51387, 168, 167,
                  5700, 316, 315,
                  5700, 317, 316,
                  61372, 396, 419,
                  61372, 397, 420,
                  6547, 350, 349,
                  6547, 351, 350,
                  66474, 159, 159,
                  66474, 160, 160,
                  67595, 255, 254,
                  67595, 256, 255,
                  75258, 352, 350,
                  75258, 353, 351,
                  75766, 247, 247,
                  75766, 248, 248,
                  75864, 255, 255,
                  75864, 257, 257,
                  76255, 295, 295,
                  76255, 296, 296,
                  8158, 418, 417,
                  8158, 420, 419,
                  83525, 77, 72,
                  83525, 79, 74,
                  85341, 210, 203,
                  85341, 211, 204,
                  85840, 246, 234,
                  85840, 247, 235,
                  87139, 249, 243,
                  87139, 250, 244,
                  87219, 185, 177,
                  87219, 186, 178,
                  87424, 194, 186,
                  87424, 195, 187,
                  97189, 428, 428,
                  97189, 429, 429,
                  97811, 496, 496,
                  97811, 497,497,
                  9798, 180, 172,
                  9798, 181, 173,
                  99165, 609, 609,
                  99165, 612,611), ncol = 3, byrow = TRUE)
for(i in 1:nrow(corma)){
  print(i)
  baza <- corectare(baza, corma[i,1],corma[i,2],corma[i,3])
}
for(i in 1:nrow(compa)){
  listoi <- completare(baza, arefwork, compa[i,1], compa[i, 2], compa[i, 3])
  baza <- listoi[[1]]
  arefwork <-listoi[[2]]
}

# siru <- 99165; vezi(baza[baza$siruta == siru,])
# vezi(arefwork[arefwork$siruta == siru,])

#analiză distribuţie secţii găsite
numar.sectii <- table(baza[, 3])
numar.sectii <- as.data.frame(numar.sectii)
gasite <- !is.na(baza[, 11])
gasite <- cbind(baza[, 3], gasite)
gasite <- as.data.frame(gasite)
gasite$gasite <- as.numeric(gasite$gasite)-1
agregare <- aggregate(gasite$gasite, by = list(gasite$V1), sum)
colnames(agregare) <- c("siruta", "sectii.gasite")
colnames(numar.sectii) <- c("siruta", "existpar")
statss <- merge(numar.sectii, agregare)
rm(numar.sectii, agregare, gasite)
statss$procentaj <- statss$sectii.gasite/statss$existpar
statss$nepar <- statss$existpar - statss$sectii.gasite
statss <- merge(statss, table(arefwork[,4]), by.x = "siruta", by.y = "Var1")
colnames(statss)[6] <- "neref"
#View(statss[order(-statss$nepar),])
vezi(with(statss, table(nepar, neref)))

print(paste("mai am", sum(statss$nepar), "secţii negăsite"))
timp2 <- Sys.time()

print(paste("a durat", timp2 - timp1, "minute, din care loop-ul", timp4 - timp3))
beep(10)


vezi(statss[statss$nepar == 1 & statss$neref == 1,])
vezi(statss[statss$nepar == 2 & statss$neref == 2,])
vezi(statss[statss$nepar == 3 & statss$neref == 3,])
vezi(statss[statss$nepar == 4 & statss$neref == 4,])

vezi(statss[statss$nepar == 2 & statss$neref == 1,])
vezi(statss[statss$nepar == 1 & statss$neref == 2,])

#rezultate proaste avem, în mod previzibil, în localităţile cu multe secţii
# #dar sunt şi secţii mici fără rezultate bune
# plot(statss$existpar, statss$procentaj)
# plot(log10(statss$existpar)+1, statss$procentaj)
# #Alea unde a găsit una din două!
# View(statss[statss$existpar == 2 & statss$procentaj == 0.5,])
# #Distribuţia localităţilor după nr. de secţii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribuţia secţiilor negăsite
vezi(statss[order(-statss$nepar),])

siru <- 108473; vezi(baza[baza$siruta == siru,])
vezi(arefwork[arefwork$siruta == siru,])
# hist(log10(statss$nepar))

#rm(contor, count, n, numar, s, adresa.curenta, adresa.gasita, indice.gasit, baza)
