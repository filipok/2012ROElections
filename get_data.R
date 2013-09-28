#get data from Dropbox (zipped files) and unzip files

#You can get the raw data files from:
#https://www.dropbox.com/s/1zt0087rd3vnqwl/2012data.zip

#or you can use this code
#source: (http://thebiobucket.blogspot.ro/2013/04/download-files-from-dropbox.htmlhelp())
dl_from_dropbox <- function(x, key) {
  require(RCurl)
  bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                      ssl.verifypeer = FALSE)
  con <- file(x, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(x, "read into", getwd())))                        
}
dl_from_dropbox("2012data.zip", "1zt0087rd3vnqwl")

#Unzip files:
unzip("2012data.zip", overwrite = FALSE)