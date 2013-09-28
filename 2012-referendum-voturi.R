################################################################################
ref2012Sv = read.csv("2012referendum/2. rezultate/sv_date.csv", sep = ";", 
                     stringsAsFactors = FALSE)
################################################################################
colnames(ref2012Sv)[1] = "JUD"
colnames(ref2012Sv)[2] = "DEN_JUD"
colnames(ref2012Sv)[6] = "SV"
colnames(ref2012Sv)[5] = "DEN_CIRC_R"
ref2012Sv$DEN_JUD = gsub("BACĂU", "BACAU", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("BOTOSANI", "BOTOŞANI", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("BRĂILA", "BRAILA", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("CONSTANTA", "CONSTANŢA", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("SĂLAJ", "SALAJ", ref2012Sv$DEN_JUD) #spelling
ref2012Sv$DEN_JUD = gsub("Municipiul Bucuresti", "MUNICIPIUL BUCUREŞTI", ref2012Sv$DEN_JUD) #spelling