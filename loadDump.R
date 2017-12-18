#Clear everything
rm(list = ls(all = TRUE))

#Download and unzip
source("generalFunctions.R")
download.file("https://s3-eu-west-1.amazonaws.com/2012roelections-r/datadump.zip", "datadump.zip")
unzip("datadump.zip", exdir="datadump", overwrite = FALSE)

#Load local elections data
TUR2 = dget("datadump/TUR2.R")
PRI = dget("datadump/PRI.R")
PCJ = dget("datadump/PCJ.R")
CLO = dget("datadump/CLO.R")
CJU = dget("datadump/CJU.R")
alegLocale2012 = dget("datadump/alegLocale2012.R")
numeSecLoc2012 = dget("datadump/numeSecLoc2012.R")

newPrim = dget("datadump/newPrim.R")
echivalareCoduri = dget("datadump/echivalareCoduri.R")

#Load Siruta data
siruta2008 = dget("datadump/siruta2008.R")

#Load general elections data
cdep = dget("datadump/cdep.R")
sen = dget("datadump/sen.R")
numeSecParl2012 = dget("datadump/numeSecParl2012.R")

#Load general election precinct data
romania = dget("datadump/romania.R")

#Load referendum data
ref2012Sv = dget("datadump/ref2012Sv.R")