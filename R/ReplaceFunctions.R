
#' Replace parameter values in base XML file
#'
#' @description
#' `replaceInfo()` updates the base parameter file with the new values from the new parameter values file.
#'
#' @param rf [character()] Base XML parameter file to be modified
#' @param variable_names [list()] Variable Names translation file
#' @param pf1 [character()] Parameter file with new values
#' @param ncols [double()] Number of columns
#' @param newname [character()] New file name
#'
#' @return
#' @export
#'
#' @examples
#' xml2 <- ReplaceInfo(xml1, variable_names, pf1, ncols, newname)
#'
replaceInfo <- function(rf, variable_names, pf1, ncols, newname) {

  #file format for pf1:
  #The first line will be a header that has the species names, which must be in " and the same as in the xml file
  #The second line of the file will say either "na" or have the name of the group to which the parameters belong
  #This will stay in effect until another line with no parameters is reached that also says "na" or a new name.
  #Note that names are only required for types 3 & 4 (behaviour) and 7 (harvest), not for types 1, 2, 5, 6

  mastergroup <- pf1[2,1]

  for (i in 3:nrow(pf1)) {
    #i <- 1
    itype <- 0
    #read the parameter file and determine the type of the variable (see description above)
    #The variable name is stored in column 1 and must have no quotes
    paramName <- pf1[i,1]

    if (pf1[i,2]==""||pf1[i,2]=="NA") {         #check to see if this is a groupName line
      mastergroup <- paramName  #if so, set the groupName
      next                      #and skip the rest of the loop.
    }

    exactParamName <- paste0("^",paramName,"$") #need this to distinguish cases like Output from ShortOutput ()
    #need to force the inclusion of the period for regular expressions
    if(grepl("\\.",exactParamName)==TRUE){ #if there's a period or decimal in the exact name, add slashes
      exactParamName <- gsub("\\.", "\\\\.", exactParamName)

    }
    iline <- grep(exactParamName,variable_names[,1])
    itype <- as.numeric(variable_names[iline,2])             #ensures that itype is a number, with no blank

    #print(paste("Looking for Variable: ", paramName, " of type ", itype))

    #codename <- trimws(variable_names[iline,3])              #removes any whitespace before and after the variable name
    codename <- gsub("\"","",trimws(variable_names[iline,3]))  ##AC change
    groupname <- trimws(variable_names[iline,4])

    iloop <- 1

    if (length(itype) > 0) {

      if (itype==2 || itype == 4 || itype == 6 || itype == 8) { #these are species related parameters. AC added itype 8
        iloop <- ncols
      }

      for (j in 1:iloop) {
        if (itype==2 || itype == 4 || itype == 8) { #these are species related parameters, so we put the species in the codename
          codename <- paste(variable_names[iline,3],"=",pf1[1,j+1],sep="")
        } else if (itype==6) {
          mastergroup <- pf1[1,j+1]   #we don't need the master group until we get a new one, so store the species in it for now
        }
        ln1 <- findFileLine(rf,itype,codename, groupname, mastergroup)
                #print(paste("Variable",codename," is on line: ", ln1))

        if (length(ln1) > 0) {
          if (itype == 5 ) {     #changing the output filename
            #need to change the slashes in the directory name (or they get stripped during the paste)
            dname <- gsub("\\\\", "//",pf1[i,j+1])
            if (codename=="so_filename") {
              fname <- paste(dname,newname,".out",sep="")
            } else if (codename=="ou_filename") {
              fname <- paste(dname,newname,"_det.gz.tar",sep="")
            }
            x <-replaceParameter(ln1, rf, fname)
          } else if(itype == 7) {
            #find what species are associated with this harvest event, and loop through those species
            #do this by calling the line function again to find out if each species is present or not.
            #ln1 returns a list of all the instances that need replacement
            savename <- codename
            sp_present <- c(rep(0,ncols))   #set up a blank vector
            for (jj in 1:ncols) {
              codename <- paste("ds_applyToSpecies species=",pf1[1,jj+1],sep="")
              isline <- 0
              isline <- findFileLine(rf,itype,codename, groupname, mastergroup)
              if (isline > 0) {
                sp_present[jj] <- 1
                #print(paste("species ", codename, "is found" ))
              }
            }
            #now we know which species are present and we know what lines have the information
            for (jj in 1:ncols) {
              if (sp_present[jj] >0) {
                #print(paste(ln1[jj], " species parameter ", pf1[i,jj+1] ))
                x <-replaceParameter(ln1[jj], rf, pf1[i,jj+1])
                rf <- x
              }
            }
          } else {
            x <-replaceParameter(ln1, rf, pf1[i,j+1])
          }
          rf <- x
        } else
          print(paste("WARNING! Variable: ", codename , " not found."))
      }
    } else {
      print(paste("WARNING! The type of variable: ",paramName, " could not be determined"))
    }

  }  #end of loop over lines of the parameter file

  #rf <- gsub("//", "\\\\", rf)    #turn any forward slashes into back into double backwards slashes
  return(rf)

}



#' Replace a parameter
#'
#' @description
#' `replaceParameter()` replaces a parameter in the base file with a new value.
#'
#' @param ln1 [double()] Line number of parameter to replace
#' @param rf [character()] Base XML parameter file to be modified
#' @param varvalue [character()] Output file name
#'
#' @return
#' @export
#'
#' @examples
#' replaceParameter(ln1, rf, fname)
#'
replaceParameter <- function(ln1, rf, varvalue) {
  #this routine works for cases where the xml file has the format:
  #   <timesteps>10</timesteps>
  #We assume the tag (e.g., timesteps) and its new value have been given.

  #find the line number for the first thing in the file (eg timesteps)
  # ln1 <- grep(pf1[i,ncol],rf)

  #Now, the line number has been passed in, along with the variable value

  #Find the characters just before the start and end of the value
  st_start <- stringr::str_locate(rf[ln1],">")
  st_end <- stringr::str_locate(rf[ln1],"</")

  #print(paste(ln1, varvalue, st_start, st_end))
  #print(paste(rf[ln1],substr(rf[ln1],st_start[1]+1,st_end[1]-1)))

  #and replace the value
  #newln <- str_replace(rf[ln1],substr(rf[ln1],st_start[1]+1,st_end[1]-1),as.character(varvalue))
  newln <- stringr::str_replace(rf[ln1],paste0(">",substr(rf[ln1],st_start[1]+1,st_end[1]-1),"<"), paste0(">",as.character(varvalue),"<"))
  rf[ln1] <- newln
  return(rf)
}


#' Remove a species
#'
#' @description
#' `removeSpecies()` removes a species from the base parameter file.
#'
#' @param sp [character()] Species name
#' @param rf [character()] Base XML file to be modified
#'
#' @return
#' @export
#'
#' @examples
#' RemoveSpecies(sp, rf)
#'
removeSpecies <-function(sp,rf) {
  #example code to remove all information about a species from the file
  #NOTE THIS WILL NOT WORK FOR HARVEST OR OTHER TYPES WHERE SPECIES IS ON A DIFFERENT LINE
  #
  sprows <- grep(sp, rf)
  rfnb <- rf[-sprows]

}


#' Remove a row
#'
#' @description
#' `removeRow()` removes a row from the base parameter file.
#'
#' @param ln1 [double()] Row number to remove
#' @param rf [character()] Base XML file to be modified
#'
#' @return
#' @export
#'
#' @examples
#' removeRow(lnm,xml1)
#'
removeRow <-function(ln1, rf) {
  #ln1 is the row to remove
  #rf is the file
  rfnb <- rf[-c(ln1[1]:ln1[2])]

}





#' Replace Lines
#'
#' @description
#' `replaceLines()` replaces a chunk of the parameter file with a different set of lines. It assumes the new set of lines are complete and completely replace relevant section.
#'
#' @param rf [character()] Base XML file to be modified
#' @param pf1 [character()] Parameter file with new values
#'
#' @return
#' @export
#'
#' @examples
#' xml2 <- replaceLines(xml1, pf1)
#'
replaceLines <- function(rf, pf1) {       #rf is the main file, pf1 is the parameter file

  #Find what group this file contains. #(Feb 2021) There is no longer the extra line at the top of the file
  firstline <- 1

  grouptext <- substring(pf1[firstline],stringr::str_locate(pf1[firstline], "<")+1,stringr::str_length(pf1[firstline])-1)[1]

  #now remove the numbers from the end of the grouptext. Note that there can be more than 1 number (eg Plant24)
  gtext <- stringr::str_replace_all(grouptext, "[:digit:]", "")

  #If the grouptext is a harvest one, then we will do it differently than if it isn't
  #This allows a chunk of harvest related code to have several different bits in it and they will all be replaced.

  harvestext <- c("EpisodicMortality","SelectionHarvest","Harvest","Plant")
  #loop through the file
  #    We will do a chunk, see if there is more, and do that, until it is all gone.
  i <-1
  while (i <length(pf1)) {
    #"i" has the group name that we are replacing
    #if the next line contains -999 then we are not replacing anything, just removing
    bRemove <- FALSE
    bRemove <- grepl("-999",pf1[i+1],fixed=TRUE)

    #get the beginning and end line of this group in the INPUT file
    grouptext <- stringr::str_trim(stringr::str_replace_all(pf1[i],c("<"="",">"="")))  #get the name of the group we are replacing

    lni <- grep(grouptext,pf1)

    #find the text inside the MAIN file
    grouptext <- stringr::str_trim(pf1[i])        #get the name of the group we are replacing
    endgroup <- stringr::str_replace_all(grouptext,"<","</")
    #print(paste("looking for",i,grouptext,endgroup))

    lnb <- grep(grouptext,rf)     #look for this text inside the main file. This should return the beginning of the group
    lne <- grep(endgroup,rf)     #look for this text inside the main file. This should return the end of the group
    #print(paste(lnb,lne))
    lnm <- c(lnb,lne)

    if (length(lnm)>0) {
      x <- removeRow(lnm, rf)  #remove the rows from the main file
      if (!bRemove) {          #if it is not a removal, then we will need to add the new lines in
        rf2 <- append(x,pf1[lni[1]:lni[2]],lnm[1]-1)
        #print(paste("replaced lines after:",lnm[1], "for group",grouptext))
      } else
      {rf2 <- x}    #set it to the version with the removed rows
    } else {
      print(paste("the group: ",grouptext,"was not found in the main file."))
      rf2 <- rf   #nothing happened
    }
    i <- lni[2]+1
    if (i < length(pf1)) { rf <- rf2}
    #print(paste("newi",i,length(pf1)))
  }
  #}
  return(rf2)

}


#' Update Number of Years (timesteps)
#'
#' @description
#' `updateNumYears()` Updates the number of years for a simulation
#'
#' @param xmls [character()] XML file to be modified with pathway included
#' @param num_years [numeric()] Number of years to run a simulation
#'
#' @return
#' @export
#'
#' @examples
#' updateNumYears(xmls, num_years)
#'
updateNumYears <- function(xmls,num_years){

  for(ix in 1:length(xmls)){

    res <- xml2::read_xml(xmls[ix])
    #write the xml to a file again (this will put in the missing line breaks)
    xml2::write_xml(res, "temp.xml")
    #read the newly printed file, this time as lines of text
    tmp <- readLines("temp.xml", encoding="UTF-8")
    xml1 <- gsub("\\\\", "//",tmp)

    ln1 <- findFileLine(rf = xml1, itype = 1, varname = "timesteps")

    xml2 <- replaceParameter(ln1 = ln1, rf = xml1, varvalue = num_years)
    xml2 <- gsub("//", "\\\\", xml2)

    writeLines(xml2,xmls[ix])

  }
}
##helper function to read in .kmz files

#' Read keyhole
#'
#' @description
#' `read_keyhole()` is a helper function to read in .kmz spatial files.
#'
#' @param file [character()] KMZ File
#'
#' @return
#' @export
#'
#' @examples
#' read_keyhole(file)
#'
read_keyhole <- function(file) {
  # get file extension
  ext <- strsplit(basename(file), split = '\\.')[[1]][-1]
  # if kml
  if (ext == 'kml') {
    layers <- st_layers(file)$name
    if (length(layers) > 1) {
      return(Reduce('rbind', lapply(layers, sf::read_sf, dsn = file)))
    }
    return(read_sf(file))
  } else {
    target_file <- '.temp.kml.zip'
    fs::file_copy(file, target_file, overwrite = T)
    unzip(target_file, overwrite = T)
    sf_out <- read_sf('doc.kml')
    fs::file_delete(target_file)
    fs::file_delete('doc.kml')
    return(sf_out)
  }
}

#' Update the parameter file with a new timestep
#'
#' @description
#' `updateNumYears()` Update the parameter file with a new timestep
#'
#' @param xmls [character()] name of xml files (includes pathway)
#' @param num_years [numeric()] number of years to update parameter file
#'
#' @return
#' @export
#'
#' @examples
#' updateNumYears(xmls,num_years)
#'
updateNumYears <- function(xmls,num_years){

  for(ix in 1:length(xmls)){

    res <- xml2::read_xml(xmls[ix])
    #write the xml to a file again (this will put in the missing line breaks)
    xml2::write_xml(res, "temp.xml")
    #read the newly printed file, this time as lines of text
    tmp <- readLines("temp.xml", encoding="UTF-8")
    xml1 <- gsub("\\\\", "//",tmp)

    ln1 <- findFileLine(rf = xml1, itype = 1, varname = "timesteps")

    xml2 <- replaceParameter(ln1 = ln1, rf = xml1, varvalue = num_years)
    xml2 <- gsub("//", "\\\\", xml2)

    writeLines(xml2,xmls[ix])

  }
}



#' Update the parameter file with a stemmap
#'
#' @description
#' `updateStemMap()` Update the parameter file with a stemmap.
#'
#' @param xmls [character()] name(s) of xml files (includes pathway)
#' @param MapNames [character()] name(s) of the text files to add to the parameter file (includes pathway)
#' @param TreatmentBase [vector()] names that appear in both XMLs and Map names to link
#' which stemmap with which parameter file
#'
#' @return
#' @export
#'
#' @examples
#' updateStemMap(xmls,MapNames)
#'
updateStemMap <- function(xmls,MapNames,TreatmentBase){

  for(ix in 1:length(xmls)){

    res <- xml2::read_xml(xmls[ix])
    #write the xml to a file again (this will put in the missing line breaks)
    xml2::write_xml(res, "temp.xml")
    #read the newly printed file, this time as lines of text
    tmp <- readLines("temp.xml", encoding="UTF-8")
    xml1 <- gsub("\\\\", "//",tmp)

    ln1 <- findFileLine(rf = xml1, itype = 1, varname = "tr_treemapFile")

    if(length(ln1) == 0){
      print(paste("Current parameter file",xmls[ix], "is missing the stem map placeholder"))
      next
    }

    Tr_use <- TreatmentBase[stringr::str_detect(xmls[ix], TreatmentBase)]
    Map_use <- grep(Tr_use, MapNames, value = TRUE)

    xml2 <- replaceParameter(ln1 = ln1, rf = xml1, varvalue = Map_use)
    xml2 <- gsub("//", "\\\\", xml2)

    writeLines(xml2,xmls[ix])

  }
}



#' Update Behaviour Position Value
#'
#' @param paramValfiles
#' @param behaviour
#' @param newPos
#' @importFrom data.table fread
#' @importFrom data.table fwrite
#' @importFrom stringr str_replace
#'
#' @return
#' @export
#'
#' @examples
behaviourPosition <- function(paramValfiles, behaviour, newPos){
  for(i in 1:length(paramValfiles)){
    pv <- fread(paramValfiles[i])
    oldPos <- as.character(pv[grep(paste0(behaviour,"*[[:digit:]]"), pv[[1]]),1])
    pv[[1]] <- str_replace(pv[[1]],oldPos,paste0(behaviour,newPos))
    fwrite(pv,paramValfiles[i])
  }

}
