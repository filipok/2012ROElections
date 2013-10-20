library("R2HTML")

vezi = function (x) { 
  #de la Georgian
  file.remove( 'test.html');
  HTML(x, file='test.html', row.names=TRUE, innerBorder=1);
  system(' "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe" "C:\\Users\\Filip\\Dropbox\\R_Work\\2012AlegeriRomania\\test.html"  ', wait=FALSE )
}
beep = function(n = 3){
  #source: http://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}

#source: (http://thebiobucket.blogspot.ro/2013/04/download-files-from-dropbox.html
dl_from_dropbox <- function(x, key) {
  require(RCurl)
  bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                      ssl.verifypeer = FALSE)
  con <- file(x, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(x, "read into", getwd())))                        
}