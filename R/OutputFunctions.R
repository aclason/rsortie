#' Extract output files
#'
#' @description
#' `extractFiles()` reads the directory and extract all files from any .gz.tar files that are present.
#' It also assumes that all the files in a single .gz.tar file were in the same directory. It looks at the
#' first file in the .gz.tar, determines how many subdirectories are present, and then strips all those subdirectories.
#' The extracted files will be placed in a new extracted directory. If you want to put them
#' somewhere else, then change the variable extractDir
#'
#' @param itype [double()] '1' = extract only the given file, otherwise extract all files in the directory
#' @param exname [character()] The directory that contains the tar file(s) to be extracted
#' @param onename [character()] The single tar file to extract (only used if itype = 1)
#' @param extime [double()] Extract time?
#'
#' @return
#' The routine returns a list of the extracted files.
#' @export
#'
#' @examples
#'extractFiles(itype,exname,onename,extime)
#'
extractFiles <- function(itype,exname,onename,extime) {  #used for .gz.tar files - e.g., trees
  #itype: 1=extract only the given file, otherwise extract all files in the directory.
  #exname: the directory that contains the tar file(s) to be extracted
  #onename: the single tar file to extract (only used if itype=1)

  #This program will then read the directory and extract all files from any tar files that are present
  #It also assumes that all the files in a single tar file were in the same directory. It looks at the
  #first file in the tar, determines how many subdirectories are present, and then strips all those subdirectories.
  #The extracted files will be placed in a new extracted directory. If you want to put them
  # somewhere else, then change the variable extractDir
  #The routine returns a list of the extracted files.
  write("", file="rungzip.bat")
  write("", file="runtar.bat")
  outdir <- exname
  extractDir <- paste0(outdir,"extracted")  #directory that will contain the extracted files
  if (dir.exists(extractDir) & (itype != 1)) {
    print("Target directory exists, so files will not be extracted.")
    return(NULL)
  }

  dir.create(extractDir,showWarnings=FALSE)  #make the directory if it doesn't already exist

  #Get the list of files for tar extract
  if(itype != 1) {  #extract all the tar files in the directory
    FileList <- list.files(outdir,pattern="*.tar")
  }else{
    FileList <- onename #list.files(outdir,pattern=onename)
  }

  #untar the list of files
  for (ix in 1:length(FileList)) {
    #first get a list of the files and find out how many directory levels down they are. Just check the first file.
    ndir <- stringr::str_count(untar(paste0(outdir,FileList[ix]), compressed = TRUE, list=TRUE),pattern="/")
    #untar(paste0(outdir,FileList[ix]),exdir=extractDir, compressed = TRUE, extras=paste0("--strip-components ",ndir[1]))
    #untar(paste0(outdir,FileList[ix]),exdir=extractDir, compressed = TRUE)
    cmd <-paste0("tar -xf \"",outdir,FileList[ix],"\""," --strip-components=",ndir," -C ",extractDir)
    write(cmd, file="runtar.bat", append=FALSE)
    system("runtar.bat")
  }

  #Get the list of .gz files to extract
  if (itype != 1) {
    FileList2 <- list.files(extractDir,pattern="*.gz",recursive=TRUE)
  } else {
    # First get all the .gz files in the directory
    FileList2 <- list.files(extractDir,pattern="*.gz",recursive=TRUE)
    #keep the ones with file name selected with itype 1
    FileList4 <- c()
    for(ix2 in 1:length(FileList)){
      FLsub <- list.files(extractDir,pattern=gsub(pattern = "*.gz.tar", "",
                                                  onename[ix2]),recursive=TRUE)
      FileList4 <-c(FileList4,FLsub)
    }
    FileList2 <- FileList4
  }
  #Extract the list of files - this doesn't seem to work or maybe not needed??
  for (ix3 in 1:length(FileList2)) {
    cmd<-paste0("gzip -d \"",extractDir,"/",FileList2[ix3],"\"")
    write(cmd, file="rungzip.bat", append=TRUE)
    system("rungzip.bat")
  }


  #I actually don't know if we need this part that returns a list of file names
  if(itype != 1){
    FileList3 <- list.files(extractDir,pattern="*.xml",recursive=TRUE,full.names=TRUE)
  } else {
    FL1 <- gsub(pattern = "*.gz.tar", "",onename)
    FL2 <- c()
    for(ix4 in 1:length(onename)){
      FL3 <- list.files(extractDir,pattern=paste0(FL1[ix4],"*"),recursive=TRUE,full.names=TRUE)
      FL2 <- c(FL2,FL3)
    }
    FileList3 <- FL2
  }

  return(FileList3)
}


#' Read plot file
#'
#' @description
#' `readPlotFile()` reads .out output files.
#'
#' @param outdir [character()] Output directory
#'
#' @return
#' @export
#'
#' @examples
#'readPlotFile(outdir)
#'
readPlotFile <- function(outdir) {
  dt <- data.table()
  FileList <- list.files(outdir,pattern="*.out")
  dt <- fread(paste0(outdir,FileList[1]), sep="\t", header=T,na.strings = "--", skip=5)
  #dt_table <- rbind(dt_table,dt)
  #return(dt_table)
  return(dt)

}
