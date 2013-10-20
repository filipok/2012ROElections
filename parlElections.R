#Source the file below:
#source("loadDump.R")
#or if you prefer the raw data:
#source("get_data.R")

#Load the required functions
source("parlFunctions.R")

#date pe secţie de votare
senSv = TransformTabelParl(sen)
cdepSv = TransformTabelParl(cdep)
#date pe localitate
senCirc = CumulatCirc(senSv)
cdepCirc = CumulatCirc(cdepSv)
#date pe colegiu
senColegiu = CumulatColegiu(senSv)
cdepColegiu = CumulatColegiu(cdepSv)
#date pe judeţ
senJudet = CumulatJudet(senSv)
cdepJudet = CumulatJudet(cdepSv)

#Facem un JOIN între cdepSv/senSv şi judete pentru codul Siruta
cdepSv = merge(cdepSv, romania, all.x = TRUE)
senSv = merge(senSv, romania, all.x = TRUE)

#Scurtăm codul Siruta, pentru a corespunde formatului de la ROAEP
siruta2008[4] = sapply(siruta2008[4], function(qqq) strtrim(qqq,nchar(qqq)-1))


#Facem JOIN-ul final între cdepSv/senSv şi siruta2008 pentru lat/long
colnames(siruta2008)[4] = "SirutaComp" #pregătire nume coloană pentru merge
cdepSv = merge(cdepSv, siruta2008[c("X", "Y", "SirutaComp")], all.x = TRUE, 
                 sort = FALSE)
senSv = merge(senSv, siruta2008[c("X", "Y", "SirutaComp")], all.x = TRUE, 
                 sort = FALSE)
#Verificări
#print(table(cdepSv[is.na(cdepSv$X) & cdepSv$JUD != 42 & 
#                      cdepSv$JUD != 43,]["DEN_JUD"]))
#print(table(senSv[is.na(senSv$X) & cdepSv$JUD != 42 & 
#                       senSv$JUD != 43,]["DEN_JUD"]))
#pdf("2012-parlamentare.pdf")
#symbols(cdepSv$X, cdepSv$Y, 
#        circles=sqrt(cdepSv$NrAlegatoriPrezentatiLaUrne)/2000, inches=FALSE,
#        main="Romania", xlab="longitudine", ylab="latitudine", 
#        bg = rgb(0,0,0,0.2), col = rgb(0,0,0,0.2))
#dev.off()
#
#pdf("2012-parlamentare-judete.pdf")
#for(judet in unique(cdepSv$DEN_JUD[cdepSv$DEN_JUD != "Strainatate (CE. 43)"])){
#  attach(cdepSv[cdepSv$DEN_JUD == judet,])
#  symbols(X, Y, circles=sqrt(NrAlegatoriPrezentatiLaUrne)/2000, inches=FALSE,
#          main=paste("Judeţul", judet), 
#          xlab="", ylab="", bg = rgb(0,0,0,0.2), col = rgb(0,0,0,0.2))
#  text(X, Y, labels = SV, cex = 0.7)
#    detach()
#}
#dev.off()


#Uncomment to export general(Parliament) election data to the CSV folder
#La nivel de secţie de votare, cu cod SIRUTA şi lat/long
#write.table(x=cdepSv, file="CSV/2012cdepsv.csv", row.names = FALSE)
#write.table(x=senSv, file="CSV/2012sensv.csv", row.names = FALSE)
#La nivel de oraş/comună
#write.table(x=cdepCirc, file="CSV/2012cdepcirc.csv", row.names = FALSE)
#write.table(x=senCirc, file="CSV/2012sencirc.csv", row.names = FALSE)
#La nivel de colegiu
#write.table(x=cdepColegiu, file="CSV/2012cdepcolegiu.csv", row.names = FALSE)
#write.table(x=senColegiu, file="CSV/2012sencolegiu.csv", row.names = FALSE)
#La nivel de judeţ
#write.table(x=cdepJudet, file="CSV/2012cdepjudet.csv", row.names = FALSE)
#write.table(x=senJudet, file="CSV/2012senjudet.csv", row.names = FALSE)
