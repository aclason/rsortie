#' Run the SORTIE model
#'
#' @description
#' `runSortie()` is a wrapper function that passes a parameter file to SORTIE to start a simulation.
#'
#' @param fname [character()] File path and name to be run
#' @param sortie_loc SORTIE location '0'
#'
#' @return
#' @export
#'
#' @examples
#'#(runSortie(parameterfile.xml,0))
#'
runSortie <-function(fname, sortie_loc) {
  ##To do - make parallel processing compatable
  #This function could be called as a stand-alone and may not be run with files created by the R scripts
  #So, we need to read the given xml, and write it again to put in the missing line breaks.
  res <- xml2::read_xml(fname)
  xml2::write_xml(res, "temp_run.xml")

  if (sortie_loc==0) {
    cmd=paste0("\"C:\\Program Files (x86)\\SORTIE\\bin\\coremodel.exe\" ","temp_run.xml")
  } else {
    cmd=paste0("\"",sortie_loc,"\" temp_run.xml")
  }
  write(cmd, file="runsortie.bat")

  system("runsortie.bat")

}

