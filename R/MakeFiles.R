#' FindFileLine
#'
#' @param VarTranslation [data.frame()] File type
#' @param base_path [character()] variable codename
#' @param param_path [character()] variable group name
#' @param xmls_path [character()] variable master group
#'
#' @return
#' @export
#'
#' @examples
#' MakeFiles(VarTranslation, base_path, param_path, xmls_path)
#'

MakeFiles <- function(VarTranslation, base_path, param_path, xmls_path){
  VariableNames <- VarTranslation #to do is change the VariableNames object to vartranslation in findfileline etc.
  ListOfFiles <- c()
  for (ix in 1:length(xmlList)) { #start loop over xml files

    #read the given xml
    res <- read_xml(paste0(base_path,xmlList[ix]))
    #write the xml to a file again (this will put in the missing line breaks)
    write_xml(res, "temp.xml")

    #read the newly printed file, this time as lines of text
    tmp <- readLines("temp.xml", encoding="UTF-8")
    xml1 <- gsub("\\\\", "//",tmp)    #reverse the slash marks

    #make a vector that contains the length of each file type
    for (ip in 1:numtype[1]) {
      for (ip2 in 1:max(1,numtype[2])) {
        for (ip3 in 1:max(1,numtype[3])) {
          for (ip4 in 1:max(1,numtype[4])) {
            for (ip5 in 1:max(1,numtype[5])) {
              ip_vals <- c(ip,ip2,ip3,ip4,ip5)
              newname <- ""
              newname <- paste(substr(xmlList[ix],1,nchar(xmlList[ix])-4),"-",substr(paramList1[[1]][ip],1,nchar(paramList1[[1]][ip])-4),sep="")
              #newname <- paste(substr(xmlList[ix],1,nchar(xmlList[ix])-4),sep="")    #remove type 1 files from the naming
              for (iii in 2:5) {
                if (numtype[iii] >0) {
                  newname <- paste(newname,"-",substr(paramList1[[iii]][ip_vals[iii]],1,nchar(paramList1[[iii]][ip_vals[iii]])-4),sep="")
                }
              }

              #for each of the files, prepare it, and process it
              # note: we have to do all five files each time because we don't know which of the files might have the output directories (which need 'newname')

              for (iii in 1:5) {
                if (numtype[iii] > 0) {
                  #print(paste("MakeFiles",iii,ip_vals[iii]))
                  #print(paramList1[[iii]][ip_vals[iii]])
                  xml2 <- ModifyFile(paste0(param_path,paramList1[[iii]][ip_vals[iii]]),xml1)
                } else {
                  xml2 <- xml1
                }
                xml1 <- xml2
              }

              xml2 <- gsub("//", "\\\\", xml2)    #turn any forward slashes into back into double backwards slashes
              #write the new file
              newname <- paste(newname,".xml",sep="")
              #newname <- "erin/debug2.xml"
              writeLines(paste0(xmls_path,newname))
              ListOfFiles <- c(ListOfFiles, newname)    #store the newly created file in a list so it can be run automatically later
            }
          }
        }
      }
    }
  }
}
