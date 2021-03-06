rm(list = ls(all = TRUE))
#Source first the files below:
source("loadDump.R")
#or if you prefer the raw data:
#source("get_data.R")

source("localFunctions.R")

cjuSv = Procente(SimpLocale2012(AbsolutSv(CJU, TRUE)))
cjuCirc = Procente(SimpLocale2012(CumulatCirc(CJU)))
priCirc = Procente(SimpLocale2012(AbsolutCirc(PRI, TRUE)))
priCirc2 = TUR2[TUR2$TIPPV == 1,]
priCirc2 = Procente(SimpLocale2012(AbsolutCirc(priCirc2, 
                                                  TRUE)))
priCircAll = rbind(priCirc, priCirc2)


source("refElections.R")

#coduri siruta aiurea
ref2012CircPro[ref2012CircPro$DEN_CIRC_R == "PEŞTIŞANI","siruta"] = 81184
ref2012CircPro[ref2012CircPro$DEN_CIRC_R == "BĂLĂNEŞTI","siruta"]= 78748
ref2012CircPro[ref2012CircPro$DEN_CIRC_R == "BAIA DE FIER","siruta"] = 78711


#facem merge pentru a introduce câştigătorii primăriilor:
referendum = merge(ref2012CircPro, newPrim, all.x = TRUE)
referendum$CODU = NULL


#facem încă un merge cu rezultatele la primărie la localele din 2012
referendum = merge(referendum, priCircAll, all.x = TRUE)

#...şi facem un merge
siruta2008$RANG[siruta2008$NAME == "Breazu"] = "V" #eroare în fişier siruta
#mai sunt erori, cu ranguri "" şi "0" în loc de "V"
referendum = merge(referendum, siruta2008[siruta2008$RANG != "V" & 
  siruta2008$RANG != "" & siruta2008$RANG != "0", 
                                     c("X", "Y", "SIRUTA_SUP")], 
              by.x = "siruta", by.y = "SIRUTA_SUP", all.x = TRUE)
#mai sunt unele puse aiurea la rang V, deşi ar trebui IV
qqq = siruta2008[siruta2008$RANG == "V" & siruta2008$NAME == siruta2008$NAME_SUP,]
backupReferendum = referendum
#print(nrow(referendum[is.na(referendum$X),]))
for(i in 1:nrow(backupReferendum[is.na(backupReferendum$X),])){
  if (nrow(qqq[qqq$SIRUTA_SUP == backupReferendum[is.na(backupReferendum$X),][i,1], c(1, 2)])  ==  1){
    #print(i)
    referendum[referendum$siruta == backupReferendum[is.na(backupReferendum$X),][i,1],c(46, 47)] = 
      qqq[qqq$SIRUTA_SUP == backupReferendum[is.na(backupReferendum$X),][i,1], c(1, 2)]
  }
}
rm(qqq, backupReferendum)
#pus manual coordonatele geogragfice la unele care nu precizau care e satul reşedinţă al comunei (rang IV vs V)
#sau care aveau probleme cu diacriticele
referendum[referendum$siruta == 13891,c(46,47)] = siruta2008[siruta2008$SIRUTA == 13917, c(1,2)]
referendum[referendum$siruta == 32179,c(46,47)] = siruta2008[siruta2008$SIRUTA == 28326, c(1,2)]
referendum[referendum$siruta == 42464,c(46,47)] = siruta2008[siruta2008$SIRUTA == 42361, c(1,2)]
referendum[referendum$siruta == 60455,c(46,47)] = siruta2008[siruta2008$SIRUTA == 60464, c(1,2)]
referendum[referendum$siruta == 63300,c(46,47)] = siruta2008[siruta2008$SIRUTA == 61149, c(1,2)]
referendum[referendum$siruta == 86487,c(46,47)] = siruta2008[siruta2008$SIRUTA == 85216, c(1,2)]
referendum[referendum$siruta == 100308,c(46,47)] = siruta2008[siruta2008$SIRUTA == 99334, c(1,2)]
referendum[referendum$siruta == 100326,c(46,47)] = siruta2008[siruta2008$SIRUTA == 99584, c(1,2)]
referendum[referendum$siruta == 100335,c(46,47)] = siruta2008[siruta2008$SIRUTA == 97857, c(1,2)]
referendum[referendum$siruta == 130348,c(46,47)] = siruta2008[siruta2008$SIRUTA == 127581, c(1,2)]
referendum[referendum$siruta == 130366,c(46,47)] = siruta2008[siruta2008$SIRUTA == 127929, c(1,2)]
referendum[referendum$siruta == 136241,c(46,47)] = siruta2008[siruta2008$SIRUTA == 134247, c(1,2)]
referendum[referendum$siruta == 151512,c(46,47)] = siruta2008[siruta2008$SIRUTA == 146851, c(1,2)]
referendum[referendum$siruta == 151576,c(46,47)] = siruta2008[siruta2008$SIRUTA == 148346, c(1,2)]
referendum[referendum$siruta == 159482,c(46,47)] = siruta2008[siruta2008$SIRUTA == 156758, c(1,2)]
referendum[referendum$siruta == 178910,c(46,47)] = siruta2008[siruta2008$SIRUTA == 178171, c(1,2)]
referendum[referendum$siruta == 179141,c(46,47)] = siruta2008[siruta2008$SIRUTA == 179132, c(1,2)] + c(-0.05, 0.05)
referendum[referendum$siruta == 179150,c(46,47)] = siruta2008[siruta2008$SIRUTA == 179132, c(1,2)] + c(0, 0.05)
referendum[referendum$siruta == 179169,c(46,47)] = siruta2008[siruta2008$SIRUTA == 179132, c(1,2)] + c(0.05, 0.05)
referendum[referendum$siruta == 179178,c(46,47)] = siruta2008[siruta2008$SIRUTA == 179132, c(1,2)] + c(-0.05, -0.05)
referendum[referendum$siruta == 179187,c(46,47)] = siruta2008[siruta2008$SIRUTA == 179132, c(1,2)] + c(0, 0.05)
referendum[referendum$siruta == 179196,c(46,47)] = siruta2008[siruta2008$SIRUTA == 179132, c(1,2)] + c(0.05, -0.05)
referendum[referendum$siruta == 179720,c(46,47)] = siruta2008[siruta2008$SIRUTA == 33756, c(1,2)]
referendum[referendum$siruta == 179917,c(46,47)] = siruta2008[siruta2008$SIRUTA == 66296, c(1,2)]
referendum[referendum$siruta == 179953,c(46,47)] = siruta2008[siruta2008$SIRUTA == 34217, c(1,2)]
referendum[referendum$siruta == 180091,c(46,47)] = siruta2008[siruta2008$SIRUTA == 138404, c(1,2)]

#culori partide
culori = c("blue", "red", "green", "white", "brown", "pink", "white", "white",
            "white", "lightgreen", "black", "yellow", "black", "white", "white")
names(culori) = unique(referendum$abrevi) #for some reason, referendum$abrevi has some NA values now

#primarii locale 2012 pe toată România
pdf("2012-romania.pdf")
symbols(referendum$X, referendum$Y, circles=sqrt(referendum$TAP)/2000, inches=FALSE,
main="Romania", 
xlab="longitudine", ylab="latitudine", bg = rgb(0,0,0,0.2), col = rgb(0,0,0,0.2))
dev.off()

pdf("2012-romania-color.pdf")
symbols(referendum$X, referendum$Y, circles=sqrt(referendum$TAP)/2000, inches=FALSE,
        main="Romania", 
        xlab="longitudine", ylab="latitudine", bg = culori[referendum$abrevi], col = rgb(0,0,0,0.2))
dev.off()

#primarii locale 2012, judeţ cu judeţ
pdf("2012-judete-color.pdf")
for(judet in unique(referendum$DEN_JUD)){
  attach(referendum[referendum$DEN_JUD == judet,])
  symbols(X, Y, circles=sqrt(TAP)/2000, inches=FALSE,
          main=paste("Judeţul", judet), 
          xlab="", ylab="", bg = culori[abrevi], col = rgb(0,0,0,0.2))
  text(X, Y, labels = format(TAPU_P_Ref, digits = 2), cex = 0.7)
  detach()
}
dev.off()

#referendum 2012, participare la vot reprezentată de culoare
pdf("2012-participare-referendum-color.pdf")
referendum2 = referendum
referendum2$TAPU_P_Ref[referendum2$TAPU_P_Ref > 0.5] = 1
referendum2$TAPU_P_Ref[referendum2$TAPU_P_Ref <= 0.5] = 0
pal = colorRamp(c("white", "blue"))
symbols(referendum2$X, referendum2$Y, circles=sqrt(referendum2$TAP)/2000, inches=FALSE,
        main="Romania", 
        xlab="longitudine", ylab="latitudine", bg = rgb(1, 1-referendum2$TAPU_P_Ref, 1-referendum2$TAPU_P_Ref), col = rgb(0,0,0,0.2))
rm(referendum2)
dev.off()

#boxplot "Prezenţa la referendum în funcţie de culoare primari (USL versus PDL)
pdf("2012-prezenta.pdf")
attach(referendum[referendum$abrevi %in% c("allUSL","allPDL"),]) 
boxplot(TAPU_P_Ref ~ abrevi * DEN_JUD, varwidth = TRUE, las = 2, 
        col = c("blue", "red"), ylim = c(0, 1.25), 
        main = "Prezenţa la referendum \nîn funcţie de culoarea primarilor 
        (USL versus PDL)")
detach()
dev.off()

#boxplot "Prezenţa la referendum în funcţie de culoare primari (USL versus PDL)
# în caz de alegeri strânse
pdf("2012-prezenta-stranse.pdf")
referendum2  = referendum[(referendum$allPDL/referendum$allUSL) > 0.8 
                  & (referendum$allPDL/referendum$allUSL) < 1.2 
                  &  referendum$abrevi %in% c("allUSL", "allPDL"),]
boxplot(TAPU_P_Ref ~ abrevi * DEN_JUD, data = referendum2, varwidth = TRUE, 
        las = 2, col = c("blue", "red"), ylim = c(0, 1.25), notch = FALSE,
        main = "Prezenţa la referendum \nîn funcţie de culoarea primarilor 
        (USL versus PDL) în caz de alegeri strânse")
rm(referendum2)
dev.off()

#coplot Participare la vot la locale vs referendum în funcţie de culoare primari
pdf("2012-participare-locale-ref.pdf")
coplot(TAPU_P_Ref ~ TAPU_P | abrevi, 
       data = referendum[referendum$abrevi %in% c("allUSL", "allPDL"),], 
       xlab = "Participarea la vot la locale vs referendum în funcţie de culoarea
       primarilor")
dev.off()


pdf("2012-plot-prezenta.pdf")
attach(referendum[referendum$abrevi == "allUSL",])
plot(TAPU_P, TAPU_P_Ref, col = "red")
detach()
attach(referendum[referendum$abrevi == "allPDL",])
points(TAPU_P, TAPU_P_Ref, col = "blue")
detach()
dev.off()

#densitate
judet = "OLT";
plot(density(referendum$TAPU_P_Ref[referendum$DEN_JUD %in% judet]))
#vs
hist(referendum$allUSL[referendum$DEN_JUD %in% judet])
#cumulative distribution
n = length(referendum$allUSL[referendum$DEN_JUD %in% judet]); 
plot(sort(referendum$allUSL[referendum$DEN_JUD %in% judet]), (1:n)/n, 
     type = "s", 
     ylim = c(0, 1))
#sau
plot(ecdf(referendum$allUSL[referendum$DEN_JUD %in% judet]))
