#' Extract output files
#'
#' @description
#' `extractFiles()` reads the directory and extract all files from any .gz.tar files that are present.
#' It also assumes that all the files in a single .gz.tar file were in the same directory. It looks at the
#' first file in the .gz.tar, determines how many subdirectories are present, and then strips all those subdirectories.
#' The extracted files will be placed in a new extracted directory. If you want to put them
#' somewhere else, then change the variable extractDir
#'
#' @param itype [double()] '1' = extract only the given file(s), 0 = extract all files in the directory
#' @param exname [character()] The directory that contains the tar file(s) to be extracted
#' @param tarnames [character()] The single tar file to extract (only used if itype = 1)
#' @param extime [double()] Extract time? Not working
#'
#' @return
#' The routine returns a list of the extracted files.
#' @export
#'
#' @examples
#'extractFiles(itype,exname,tarnames,extime)
#'
extractFiles <- function(itype,exname,tarnames,extime) {  #used for .gz.tar files
  #itype: 0 = extract all files in the directory, 1 = extract only the given file(s)
  #exname: the directory that contains the tar file(s) to be extracted
  #tarnames: the single tar file to extract (only used if itype=1)

  write("", file="runtar.bat")
  outdir <- exname
  extractDir <- paste0(outdir,"extracted")  #directory that will contain the extracted files
  #if (dir.exists(extractDir) & (itype != 1)) {
  #  print("Target directory exists, so files will not be extracted.")
  #  return(NULL)
  #}

  dir.create(extractDir,showWarnings=FALSE)  #make the directory if it doesn't already exist

  #Get the list of files for tar extract
  if(itype == 0) {  #extract all the tar files in the directory
    FileList <- list.files(outdir,pattern="*.tar")
  }else{
    FileList <- tarnames
  }

  #untar the list of files
  for (ix in 1:length(FileList)) {
    #first get a list of the files and find out how many directory levels down they are. Just check the first file.
    ndir <- stringr::str_count(untar(paste0(outdir,FileList[ix]), list=TRUE),pattern="/")
    cmd <-paste0("tar -xf \"",outdir,FileList[ix],"\""," --strip-components=",ndir," -C ",extractDir)
    write(cmd, file="runtar.bat", append=FALSE)
    #untar the files
    system("runtar.bat")
  }

  #Get the list of .gz files to extract
  if (itype == 0) {
    FileList2 <- list.files(extractDir,pattern="*.gz",recursive=TRUE)
  } else {
    FileList2 <- c()
    for(ix2 in 1:length(FileList)){
      FLsub <- list.files(extractDir,pattern=gsub(pattern = "*.gz.tar", "",
                                                  tarnames[ix2]))
      FLsub <- grep(pattern="*.gz",FLsub, value = TRUE)
      FileList2 <-c(FileList2,FLsub)
    }
  }
  #Extract the list of files
  #just extract the outputs that have a year associated with it (no year = parameter file)
  FileList3 <- grep(paste0("[[:digit:]]",".xml.gz"),FileList2, value = TRUE)
  for (ix3 in 1:length(FileList3)) {
    filename <- paste0(extractDir,"/",FileList3[ix3])
    print(paste("unzipping",filename))
    R.utils::gunzip(filename, overwrite = TRUE, remove = FALSE)
  }

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
