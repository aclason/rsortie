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


#' Run SORTIE in parallel
#'
#' @description
#' `runSortiePar()` is a wrapper function that passes parameter files to process parallel SORTIE simulations
#'
#' @param fname File path and parameter file names to run
#' @param numcores How many cores. Right now, the number of cores is the same as the number of files to run, but we will
#' update this to allow serial runs passed across cores
#' @param sortie_loc '0' defaults to Program Files (x86) for SORTIE location. If stored elsewhere, sortie_loc = location
#'
#' @return
#' @export
#'
#' @examples
#'#(runSortiePar(parameterfile.xml,numcores = 2, 0))
#'
runSortiePar <- function(fname, numcores, sortie_loc) {
  cl <- parallel::makeCluster(numcores)
  doParallel::registerDoParallel(cl)
  parallel::clusterEvalQ(cl, c(library(foreach))) #probably not the right way to embed foreach

  for(ii in 1:length(fname)){
    res <- xml2::read_xml(fname[ii])
    xml2::write_xml(res, paste0("temp_run",ii,".xml"))
    if (sortie_loc==0) {
      cmd=paste0("\"C:\\Program Files (x86)\\SORTIE\\bin\\coremodel.exe\" ",paste0("temp_run",ii,".xml"))
    } else {
      cmd=paste0("\"",sortie_loc,"\" temp_run.xml")
    }
    write(cmd, file=paste0("runsortie",ii,".bat"))
  }
  `%dopar%` <- foreach::`%dopar%`
  foreach::foreach(i=1:length(fname))%dopar%{
    system(paste0(getwd(),"/",paste0("runsortie",i,".bat")))
  }

  parallel::stopCluster(cl)
}
