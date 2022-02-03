
#' Initiate tree list
#'
#' @param initname
#' @param numDigits
#' @param diamBinMin
#' @param diamBinMax
#' @param diaminc
#'
#' @return
#' @export
#'
#' @examples
InitTreeList <- function(initname,numDigits=0, diamBinMin, diamBinMax, diaminc){
  de<-data.frame(paste0(initname,formatC(seq(diamBinMin,diamBinMax, by=diaminc),
                                         digits = numDigits, format = "f")),
                 rep(6,length(seq(diamBinMin,diamBinMax, by=diaminc))),
                 paste0("tr_initialDensity sizeClass\"=s",
                        formatC(seq(diamBinMin,diamBinMax, by=diaminc),
                                digits = numDigits, format = "f"),"\""),
                 rep("tr_idVals",length(seq(diamBinMin,diamBinMax, by=diaminc))))
  names(de)<-names(VariableNames)
  newdf <- rbind(VariableNames, de)
  return(newdf)
}


#' Make new parameter files
#'
#' @description `makeFiles()` takes a base parameter file and replaces specific
#' parameter values to generate new parameter files to be run in SORTIE.
#'
#' @param lstFiles The text file [character()]or [dataframe()] that contains all the file names to update and be updated
#' @param base_path [character()] the file path to the base parameter file(s) location
#' @param param_path [character()] the file path to the parameter value file(s) location
#' @param xmls_path [character()] the file path to the new output parameter file(s) location
#' @param TreeListTransL
#'
#' @details Common use of this function would be to change the number of timesteps or initiate different
#' starting stands to create a series of parameter files for an experiment
#'
#' @details
#' There are many different types of parameter values that can be updated.
#' The [VariableNames()] file is then essential to ensure the parameter name that is defined in the R environment
#' can be found in the base parameter file to replace the correct value. This csv file must be setup correctly
#' with the following columns:#'
#'     col 1: input parameter name defined
#'     col 2: type
#'     col 3: name in the line being replaced in the base parameter file
#'     col 4: group name
#'
#' Column 2 defines the type of parameter to be replaced and these are the valid values:
#'     1 = basic case: variable parameter is directly after the name
#'     2 = basic case with species: same, but with a species name after it
#'     3 = behaviorlist type: but basic parameter - similar to 1
#'     4 = behaviorlist type: but with species - similar to 2
#'     5 = output files: so parameter file will have a directory name
#'     6 = groups with species on the previous line, e.g. for initial density
#'
#' @return This function will generate new .xml files in the [xmls_path()] directory
#' @export
#'
#' @examples
#' a <- data.frame("type"=c(0,1,1), "name"=c("a1.csv","a2.csv","a3.csv"))
#' makeFiles(lstFiles=a, base_path=".", param_path=".", xmls_path=".")
#'
makeFiles <- function(lstFiles, base_path, param_path, xmls_path,TreeListTransL=NULL){
  if(is.character(lstFiles)){
    lstFiles <- read.csv(lstFiles)
  } else if(is.data.frame(lstFiles)){
    lstFiles <- lstFiles #if a user has created a data.frame in R and not provided a csv that should be fine
  }else{
    stop("provide a valid file name or dataframe of files to update and be updated")
  }

  if(is.null(TreeListTransL)){
    VariableNames <- VariableNames #use Variable Names unless adding tree inits, then add those rows
  }else{
    VariableNames <- TreeListTransL
  }

  xmlList <- c()
  paramList1 <- vector("list",5)
  maxtype <- 0
  for (i in 1:nrow(lstFiles)) {
    fn <- as.character(trimws(lstFiles$name[i]))
    itype <- lstFiles$type[i]

    if (itype == 0) {
      xmlList <- c(xmlList,fn)
    }
    else {
      paramList1[[itype]] <- c(paramList1[[itype]],list(fn))
    }
    if (itype > maxtype) {maxtype <- itype}
  }
  numtype <- c()
  for (iii in 1:5) {
    numtype <- c(numtype,length(paramList1[[iii]]))
  }


  ListOfFiles <- c()
  for (ix in 1:length(xmlList)) { #start loop over xml files

    #read the given xml
    if(file.exists(paste0(base_path,xmlList[ix]))){
      res <- xml2::read_xml(paste0(base_path,xmlList[ix]))
    }else{
      stop("the base xml file does not exist")
    }

    #write the xml to a file again (this will put in the missing line breaks)
    xml2::write_xml(res, "temp.xml")

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
              for (iii in 2:5) {
                if (numtype[iii] >0) {
                  newname <- paste(newname,"-",substr(paramList1[[iii]][ip_vals[iii]],1,nchar(paramList1[[iii]][ip_vals[iii]])-4),sep="")
                }
              }

              #for each of the files, prepare it, and process it
              # note: we have to do all five files each time because we don't know which of the files might have the output directories (which need 'newname')

              for (iii in 1:5) {
                if (numtype[iii] > 0) {
                  xml2 <- ModifyFile(paste0(param_path,paramList1[[iii]][ip_vals[iii]]),xml1,newname)
                } else {
                  xml2 <- xml1
                }
                xml1 <- xml2
              }

              xml2 <- gsub("//", "\\\\", xml2)    #turn any forward slashes into back into double backwards slashes
              #write the new file
              newname <- paste(newname,".xml",sep="")
              writeLines(xml2,paste0(xmls_path,newname))
              print(paste("parameter file",newname,"created"))
            }
          }
        }
      }
    }
  }
}

#' Find a file line
#'
#' @description
#' `FindFileLine()` finds the line within the base parameter file that is to be modified.
#'
#' @param rf [character()] Base XML parameter file to be modified
#' @param itype [integer()] File type
#' @param varname [character()] variable codename
#' @param vargroup [character()] variable group name
#' @param varmaster [character()] variable master group
#'
#' @return
#' @export
#'
#' @examples
#' FindFileLine(rf,itype,codename, groupname, mastergroup)
#'
FindFileLine <- function(rf,itype, varname, vargroup, varmaster) {
  #This routine finds the right line in the file
  ln1 <- 0
  lng <- 0


  if (itype==1 || itype == 2 || itype == 5 || itype == 8) {
    #Basic find: the variable is unique
    ln1 <- grep(varname,rf)

  } else if (itype == 3 || itype == 4) {  #behaviour list parameter sections
    #need to find two levels: first the MasterGroup (eg QuadratLight1)
    #and then within that, the subgroup, if necessary (eg gr_nciMaxPotentialGrowth)
    #need to find the line of the master group first
    lnm <- grep(varmaster,rf)     #this should return two values: the beginning and end of the group
    if (length(lnm)>0) {
      if (itype==3) {             #unique line within this group
        #print(paste(varmaster, varname, length(lnm)))
        if (length(lnm) >1 ) {
          lng2 <- grep(varname,rf[lnm[1]:lnm[2]])  #this returns the line number within this group
        } else {                                  #this only was found in the opening line (like for grids)
          lng2 <- grep(varname,rf[lnm[1]:length(rf)])  #so find the variable in all places after the opening. There might be several
        }
        ln1 <- lnm[1]+lng2-1    # to get the overall line number  (note,if there are sevearl with that varname, we just use the first instance)
      } else {
        lng <- grep(vargroup,rf[lnm[1]:lnm[2]])   #this should return two values: the beginning and end of the subgroup
        if (length(lng)>0) {
          stline <- lng[1] + lnm[1]-1          #need to translate the line number within the group to full file line number
          endline <- lng[2] + lnm[1] -1
          lng2 <- grep(varname,rf[stline:endline])  #this returns the line number within this group
          ln1 <- stline+lng2-1    # to get the overall line number
        } else {
          print(paste("WARNING! Variable Group:", vargroup, "not found."))
        }
      }
    } else {
      print(paste("WARNING! Master Variable:", varmaster, "not found. Check that the behaviour list name and number in your parameter values file matches the base xml behaviour list name and number"))
    }
  } else if (itype == 6) {   #Initial Density section
    #For this type, we need to find the section with the right species.
    #So we need to first add in the species (passed in as the master group for convenience)
    fullname <- paste(vargroup," whatSpecies=",varmaster,sep="")
    lnm <- grep(fullname,rf)     #this should return  the beginning of the group
    lne <- lnm + min(grep(vargroup,rf[lnm+1:length(rf)]))    #ending line
    if (length(lnm)>0) {
      tempRF <-gsub("\"","",rf[lnm[1]:lne])   #This makes a copy of the search area without the " marks
      lng2 <- grep(varname,tempRF)  #this returns the line number within this group
      if (length(lng2)>0) {
        ln1 <- lnm[1]+lng2[1]-1               # to get the overall line number
      }
    } else {
      print(paste("WARNING! Variable Group 6:", fullname, "not found."))
    }

  } else if (itype == 7) {   #Harvest
    #For this type, we need to find right instance of the item.
    #The variable vargroup will contain the which instance we want (e.g.  2)
    lnm <- grep(varmaster,rf)     #this should return  the beginning of the group
    lne <- lnm + min(grep(varmaster,rf[lnm+1:length(rf)]))    #ending line
    if (length(lnm)>0) {
      lng2 <- grep(varname,rf[lnm[1]:lne[1]])  #this returns the line numbers within this group
      if (length(lng2)>0) {
        ln1 <- lnm[1]+lng2-1               # to get the overall line numbers
      }
      #print(paste("group line:", lnm, "start line: ", lng2))
    } else {
      print(paste("WARNING! Variable Group 7:", varmaster, "not found."))
    }

  } else {
    print(paste("WARNING! Variable,", varname, "with type:", itype, "is not a known type."))
  }

  return(ln1)
}


#' Replace a parameter
#'
#' @description
#' `ReplaceParameter()` replaces a parameter in the base file with a new value.
#'
#' @param ln1 [double()] Line number of parameter to replace
#' @param rf [character()] Base XML parameter file to be modified
#' @param varvalue [character()] Output file name
#'
#' @return
#' @export
#'
#' @examples
#' ReplaceParameter(ln1, rf, fname)
#'
ReplaceParameter <- function(ln1, rf, varvalue) {
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
#' `RemoveSpecies()` removes a species from the base parameter file.
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
RemoveSpecies <-function(sp,rf) {
  #example code to remove all information about a species from the file
  #NOTE THIS WILL NOT WORK FOR HARVEST OR OTHER TYPES WHERE SPECIES IS ON A DIFFERENT LINE
  #
  sprows <- grep(sp, rf)
  rfnb <- rf[-sprows]

}


#' Remove a row
#'
#' @description
#' `RemoveRow()` removes a row from the base parameter file.
#'
#' @param ln1 [double()] Row number to remove
#' @param rf [character()] Base XML file to be modified
#'
#' @return
#' @export
#'
#' @examples
#' RemoveRow(lnm,xml1)
#'
RemoveRow <-function(ln1, rf) {
  #ln1 is the row to remove
  #rf is the file
  rfnb <- rf[-c(ln1[1]:ln1[2])]

}



#' Prepare a file to be updated
#'
#' @description
#' `PrepareFile()` prepares the new parameter file for `ModifyFile()`
#'
#' @param pfname [character()] File path and name of the parameter file with new values
#'
#' @return pf1
#' @export
#'
#' @examples
#' PrepareFile(paramFile)
#'
PrepareFile <-function(pfname) {
  if(file.exists(pfname)){
    tempf1 <- readLines(as.character(pfname))
  }else{
    stop("must provide a valid parameter values file")
  }
  #determine the number of species by counting the number of commas in the first line
  ncols <- stringr::str_count(tempf1[1], ",")

  if (ncols>0) {
    #strip the " from this file
    tempf <- gsub("\"","",tempf1)
    #The first line will be a header that has the species names, which must be in " and the same as in the xml file
    pf1 <- stringr::str_split_fixed(tempf, ",", n=ncols+1)
    #TEST
    pf1[1,] <- paste0("\"",pf1[1,],"\"")

  } else {    #if there are no commas in the first line, we will assume that it is an xml section to insert
    tempf <-tempf1
    pf1 <- tempf
  }

  return(pf1)
}



#' Modify a parameter file
#'
#' @description
#' `ModifyFile()` replaces the base parameter file with new, updated values.
#'
#' @param paramFile [character()] Parameter file with new values
#' @param xml1 [character()] Base XML parameter file to be modified
#' @param newname name of the new parameter file being created. Automatically generated in makeFiles function
#'
#' @return
#' @export
#'
#' @examples
#' ModifyFile("./Inputs/ParameterValues/D3.csv",xml1, newname)
#'
ModifyFile <-function(paramFile, xml1, newname) {
  pf1 <- PrepareFile(paramFile)
  if (!is.null(ncol(pf1))) {#usual file type with variables on the lines and values in columns
    ncols <- ncol(pf1)-1
    xml2 <- ReplaceInfo(xml1, VariableNames, pf1, ncols, newname)
  } else { #there are no columns here so we will assume it is a .xml chunk
    p2 <- NULL
    try(p2 <- xml2::read_xml(toString(paramFile)),silent=TRUE)
    if (!is.null(p2)) {
      xml2::write_xml(p2, "p2.xml",options=c("no_declaration","format")) #Note, we have now removed the extra line.
      p2 <- readLines("p2.xml", encoding="UTF-8")
    } else {
      p2<- readLines(toString(paramFile), encoding="UTF-8")
    }
    pf1 <- gsub("\\\\", "//",p2)

    xml2 <- ReplaceLines(xml1, pf1)
  }
  return(xml2)
}

#' Replace parameter values in base XML file
#'
#' @description
#' `ReplaceInfo()` updates the base parameter file with the new values from the new parameter values file.
#'
#' @param rf [character()] Base XML parameter file to be modified
#' @param VariableNames [list()] Variable Names translation file
#' @param pf1 [character()] Parameter file with new values
#' @param ncols [double()] Number of columns
#' @param newname [character()] New file name
#'
#' @return
#' @export
#'
#' @examples
#' xml2 <- ReplaceInfo(xml1, VariableNames, pf1, ncols, newname)
#'
ReplaceInfo <- function(rf, VariableNames, pf1, ncols, newname) {

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

    exactParamName <- paste("\\b",paramName,"\\b",sep="")   #need this to distinguish cases like Output from ShortOutput ()
    iline <- grep(exactParamName,VariableNames[,1])
    itype <- as.numeric(VariableNames[iline,2])             #ensures that itype is a number, with no blank

    #print(paste("Looking for Variable: ", paramName, " of type ", itype))

    #codename <- trimws(VariableNames[iline,3])              #removes any whitespace before and after the variable name
    codename <- gsub("\"","",trimws(VariableNames[iline,3]))  ##AC change
    groupname <- trimws(VariableNames[iline,4])

    iloop <- 1

    if (length(itype) > 0) {

      if (itype==2 || itype == 4 || itype == 6 || itype == 8) { #these are species related parameters. AC added itype 8
        iloop <- ncols
      }

      for (j in 1:iloop) {
        if (itype==2 || itype == 4 || itype == 8) { #these are species related parameters, so we put the species in the codename
          codename <- paste(VariableNames[iline,3],"=",pf1[1,j+1],sep="")
        } else if (itype==6) {
          mastergroup <- pf1[1,j+1]   #we don't need the master group until we get a new one, so store the species in it for now
        }
        ln1 <- FindFileLine(rf,itype,codename, groupname, mastergroup)

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
            x <-ReplaceParameter(ln1, rf, fname)
          } else if(itype == 7) {
            #find what species are associated with this harvest event, and loop through those species
            #do this by calling the line function again to find out if each species is present or not.
            #ln1 returns a list of all the instances that need replacement
            savename <- codename
            sp_present <- c(rep(0,ncols))   #set up a blank vector
            for (jj in 1:ncols) {
              codename <- paste("ds_applyToSpecies species=",pf1[1,jj+1],sep="")
              isline <- 0
              isline <- FindFileLine(rf,itype,codename, groupname, mastergroup)
              if (isline > 0) {
                sp_present[jj] <- 1
                #print(paste("species ", codename, "is found" ))
              }
            }
            #now we know which species are present and we know what lines have the information
            for (jj in 1:ncols) {
              if (sp_present[jj] >0) {
                #print(paste(ln1[jj], " species parameter ", pf1[i,jj+1] ))
                x <-ReplaceParameter(ln1[jj], rf, pf1[i,jj+1])
                rf <- x
              }
            }
          } else {
            x <-ReplaceParameter(ln1, rf, pf1[i,j+1])
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


#' Replace Lines
#'
#' @description
#' `ReplaceLines()` replaces a chunk of the parameter file with a different set of lines. It assumes the new set of lines are complete and completely replace relevant section.
#'
#' @param rf [character()] Base XML file to be modified
#' @param pf1 [character()] Parameter file with new values
#'
#' @return
#' @export
#'
#' @examples
#' xml2 <- ReplaceLines(xml1, pf1)
#'
ReplaceLines <- function(rf, pf1) {       #rf is the main file, pf1 is the parameter file

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
      x <- RemoveRow(lnm, rf)  #remove the rows from the main file
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


#' Run the SORTIE model
#'
#' @description
#' `RunSortie()` is a wrapper function that passes a parameter file to SORTIE to start a simulation.
#'
#' @param fname [character()] File path and name to be run
#' @param sortie_loc SORTIE location '0'
#'
#' @return
#' @export
#'
#' @examples
#'RunSortie(parameterfile.xml,0)
#'
RunSortie <-function(fname, sortie_loc) {
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
  write(cmd, file="runSortie.bat")

  system("runSortie.bat")

}


#' Run SORTIE in parallel
#'
#' @description
#' `RunSortiePar()` is a wrapper function that passes parameter files to process parallel SORTIE simulations
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
#'RunSortiePar(parameterfile.xml,numcores = 2, 0)
#'
RunSortiePar <- function(fname, numcores, sortie_loc) {
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
    write(cmd, file=paste0("runSortie",ii,".bat"))
  }
  `%dopar%` <- foreach::`%dopar%`
  foreach::foreach(i=1:length(fname))%dopar%{
    system(paste0(getwd(),"/",paste0("runSortie",i,".bat")))
  }

  parallel::stopCluster(cl)
}

#' Extract output files
#'
#' @description
#' `ExtractFiles()` reads the directory and extract all files from any .gz.tar files that are present.
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
#'ExtractFiles(itype,exname,onename,extime)
#'
ExtractFiles <- function(itype,exname,onename,extime) {  #used for .gz.tar files - e.g., trees
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
#' `ReadPlotFile()` reads .out output files.
#'
#' @param outdir [character()] Output directory
#'
#' @return
#' @export
#'
#' @examples
#'ReadPlotFile(outdir)
#'
ReadPlotFile <- function(outdir) {
  dt <- data.table()
  FileList <- list.files(outdir,pattern="*.out")
  dt <- fread(paste0(outdir,FileList[1]), sep="\t", header=T,na.strings = "--", skip=5)
  #dt_table <- rbind(dt_table,dt)
  #return(dt_table)
  return(dt)

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

