#clear everything
#rm(list = ls(all = TRUE))
#
source("2012-locale-voturi.R")

source("2012-locale-sectii.R")

#
################################################################################
#determinare partidul noilor primari
################################################################################
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
