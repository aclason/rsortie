#' Run the SORTIE model
#'
#' @description
#' `runSortie()` passes a parameter file to SORTIE to start a simulation.
#'
#' @param fname [character()] File path and name of SORTIE parameter file to be run
#' @param sortie_loc Location of SORTIE program
#'
#' @details default location for sortie_loc = 0: "C:/Program Files (x86)/SORTIE/bin/coremodel.exe/",
#' if the program is installed else, use sortie_loc = 1 and specify the location of coremodel.exe,
#' end the character string of the core model location after coremodel.exe (i.e don't add a
#' final forward slash)
#'
#' @return
#' @export
#'
#' @examples
#'# MySortieLoc <- "C:/Program Files/SORTIE/bin/coremodel.exe"
#'#(runSortie(parameterfile.xml,0))
#'
runSortie <-function(fname, sortie_loc=0) {
  ##To do - make parallel processing compatable
  #This function could be called as a stand-alone and may not be run with files created by the R scripts
  #So, we need to read the given xml, and write it again to put in the missing line breaks.
  res <- xml2::read_xml(fname)
  xml2::write_xml(res, "temp_run.xml")

  if (sortie_loc==0) {
    cmd=paste0("\"C:\\Program Files (x86)\\SORTIE\\bin\\coremodel.exe\" ","temp_run.xml")
  } else {

    cmd=paste0("\"", gsub("/","\\\\",sortie_loc),"\"", "temp_run.xml")
  }
  write(cmd, file="runsortie.bat")

  system("runsortie.bat")

}

