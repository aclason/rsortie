#' Create new SORTIE parameter files
#'
#' @description `makeFiles` takes a base SORTIE parameter file (base xmls) and replaces specific
#' values (new vals) to generate new parameter files (new xmls) to be run in SORTIE.
#'
#' @param lstFiles The text file [character()]or [data.frame()] that contains all the file names to update and be updated
#' @param path_basexmls (optional) the file path to the base parameter file(s) location [character()]
#' @param path_newvals (optional) the file path to the new value file(s) location [character()]
#' @param path_newxmls (optional) the file path to the new output parameter file(s) location [character()]
#' @param treelist_add
#'
#' @details Common use of this function would be to change the number of timesteps, initiate different
#' starting stands, or test the effect of changing the values that define a given behaviour (process)
#'
#' @details
#' The `makeFiles` function requires a table indicating the base SORTIE parameter file (.xml)
#' and the files containing values to replace in the base parameter file. This table can be read from file, with
#' `lstFiles()` being a character that contains the pathway and file name of the list of files
#' table (i.e. "pathway/listofmyfiles.csv"). If the list of files is held in a data.frame in the R environment
#' (as the example), then `lstFiles()` requires just the name of that object.
#'
#' There are two optional pathway arguments available if the required files (base parameter file(s), and new
#' values file(s)) are not yet present in the R environment. `makeFiles()` will read those files in and translate
#' them into the appropriate format, but requires file directory location be passed by the `path_basexml` and
#' `path_newvals` arguments. The `path_newxmls` is provided to allow users the ability to organize and store
#' newly generated SORTIE parameter files in different directories, but if not passed, the new files will
#' be placed in the working directory.
#'
#' NOTE - you do not need to assign the makeFiles function to a named object in R, as the output is written to
#' file, not returned as an object.
#'
#' @return This function will generate new .xml files in the `path_newxmls` directory, or if `path_newxmls`
#' is not defined, the new .xml will be exported to the working directory.
#'
#' @export
#'
#' @examples
#' exFiles <- data.frame("type"=c(0,0,1), "name"=c("samplebasexml","samplebasexml","gmf_time_new"))
#' makeFiles(lstFiles=exFiles)
#'
makeFiles <- function(lstFiles, path_basexmls = path_basexmls, path_newvals = path_newvals,
                      path_newxmls = path_newxmls, treelist_add = NULL){
  #determine what type of file the list of files is
  if(is.character(lstFiles)){
    lstFiles <- read.csv(lstFiles)
  } else if(is.data.frame(lstFiles)){
    lstFiles <- lstFiles #if a user has created a data.frame in R and not provided a csv that should be fine
  }else{
    stop("provide a valid file name or dataframe of files to update and be updated")
  }

  #determine whether there has been an addition to the Variable names file
  if(missing(treelist_add)){
    VariableNames <- VariableNames #use Variable Names unless adding tree inits, then add those rows
  }else{
    VariableNames <- treelist_add
  }

  #create the hierarchy for updating - how many base files, how many new parameter value files?
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

    # read in the base parameter file or rename it if its already in the environment
    if (missing(path_basexmls)){
      if (is.null(xmlList[ix])){
        stop("the base xml file does not exist")
      } else {
        xml1 <- get(xmlList[ix]) #it's possible to break here if we don't make sure the readLines/gsteps happen
      }
    } else {
      res <- xml2::read_xml(paste0(path_basexmls,xmlList[ix]))
      #write the xml to a file again (this will put in the missing line breaks)
      xml2::write_xml(res, "temp.xml")
      #read the newly printed file, this time as lines of text
      tmp <- readLines("temp.xml", encoding="UTF-8")
      xml1 <- gsub("\\\\", "//",tmp)    #reverse the slash marks
    }

    #Start loop through the parameter value files defined by num types
    #make a vector that contains the length of each file type
    for (ip in 1:numtype[1]) {
      for (ip2 in 1:max(1,numtype[2])) {
        for (ip3 in 1:max(1,numtype[3])) {
          for (ip4 in 1:max(1,numtype[4])) {
            for (ip5 in 1:max(1,numtype[5])) {
              ip_vals <- c(ip,ip2,ip3,ip4,ip5)
              #naming doesn't work when you pass a data.frame - not a csv
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
                  if(missing(path_newvals)){
                    #need one more check for the parameter values to send error
                      tempf1 <- paramList1[[iii]][ip_vals[iii]]
                      write.csv(get(tempf1[[ip]]), "tempf1.csv", row.names = FALSE) #not sure about index
                      pfname <- readLines(as.character("tempf1.csv")) #not sure about this index
                    } else {
                      tempf1 <- paste0(path_newvals,paramList1[[iii]][ip_vals[iii]])
                      pfname <- readLines(as.character(tempf1))
                    }
                  #if ()
                  #} else {
                  #  stop("must provide a valid parameter values file")
                  #}
                  xml2 <- modifyFile(pfname,xml1,newname)
                  #xml2 <- ModifyFile(paste0(path_newvals,paramList1[[iii]][ip_vals[iii]]),xml1,newname)
                } else {
                  xml2 <- xml1
                }
                xml1 <- xml2
              }

              xml2 <- gsub("//", "\\\\", xml2)    #turn any forward slashes into back into double backwards slashes
              #write the new file
              newname <- paste(newname,".xml",sep="")
              if(missing(path_newxmls)){
                writeLines(xml2,newname)
              } else {
                writeLines(xml2,paste0(path_newxmls,newname))
              }
              print(paste("parameter file",newname,"created"))
            }
          }
        }
      }
    }
  }
}

#' Add new variables that translate updates to base SORTIE parameter file
#'
#' @description `treelistDfn()` adds additional initial tree diameter size classes and prefixes
#' to `VariableNames`
#'
#' @details `VariableNames` is a table that translates the names of parameters found within behaviours
#' (variables) defined by a user in the newvals object to the names of these parameters (variables) found in
#' the base SORTIE parameter file. This file is essential to finding the right variable within the right
#' behaviour to update with new values during a `makeFiles()` call.
#'
#' For details on how to write a new `VariableNames` file, see the *Structure of rsortie* vignette, linked below
#'
#' There is a default `VariableNames` loaded with the rsortie package, but a user may wish to add additional
#' variable translations.
#'
#'
#' @param initname Naming convention [character()] used to describe diameter size classes
#' @param numDigits Number of decimal places required to match the base parameter file
#' @param diamMin Minimum diameter size
#' @param diamMax Maximum diameter size
#' @param diamInc Size (in cm) of diameter bins
#'
#' @seealso \href{https://aclason.github.io/rsortie/articles/use_rsortie.html}{Structure of rsortie}
#'
#' @return
#' @export
#'
#' @examples
#' samplebasexml[1:30]
#' VariableNames[1:30,]
treelistDfn <- function(initname,numDigits=0, diamMin, diamMax, diamInc){
  de<-data.frame(paste0(initname,formatC(seq(diamMin,diamMax, by=diamInc),
                                         digits = numDigits, format = "f")),
                 rep(6,length(seq(diamMin,diamMax, by=diamInc))),
                 paste0("tr_initialDensity sizeClass=s\"",
                        formatC(seq(diamMin,diamMax, by=diamInc),
                                digits = numDigits, format = "f"),"\""),
                 rep("tr_idVals",length(seq(diamMin,diamMax, by=diamInc))))
  names(de)<-names(VariableNames)
  newdf <- rbind(VariableNames, de)
  #return(newdf)
}

#' Find the location in a SORTIE base parameter file from variable name
#'
#' @description
#' `findFileLine()` finds the line within the base SORTIE parameter file that the new values (file or dataframe)
#' identifies is a variable to be modified. This file line applies only to the base parameter file and is
#'  passed to `replaceInfo()`.
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
#' samplebasexml[1:4]
#' VariableNames[1:4,]
#' gmf_time_new
#' findFileLine(rf=samplebasexml,itype=1,varname="timesteps")
#'
findFileLine <- function(rf,itype, varname, vargroup, varmaster) {
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

#' Match formatting of the new values file to the base SORTIE parameter file
#'
#' @description
#' `prepareFile()` is called by `modifyFile()` and receives an table of values that will replace the
#'values already in the base SORTIE parameter file. Typically this function is not called outside of the
#'`makeFiles()` function sequence.
#'
#' @param pfname [character()] File path and name of the parameter file with new values
#'
#' @return a formatted table containing the values of
#' @export
#'
#' @examples
#' prepareFile(paramFile)
#'
prepareFile <-function(pfname) {
  #determine the number of species by counting the number of commas in the first line
  ncols <- stringr::str_count(pfname[1], ",")

  if (ncols>0) {
    #strip the " from this file
    tempf <- gsub("\"","",pfname)
    #The first line will be a header that has the species names, which must be in " and the same as in the xml file
    pf1 <- stringr::str_split_fixed(tempf, ",", n=ncols+1)
    #TEST
    pf1[1,] <- paste0("\"",pf1[1,],"\"")
  } else {    #if there are no commas in the first line, we will assume that it is an xml section to insert
    tempf <-pfname
    pf1 <- tempf
  }
  return(pf1)
}

#' Modify a parameter file
#'
#' @description
#' `modifyFile()` replaces the base parameter file with new, updated values.
#'
#' @param paramFile [character()] Parameter file with new values
#' @param xml1 [character()] Base XML parameter file to be modified
#' @param newname name of the new parameter file being created. Automatically generated in makeFiles function
#'
#' @return
#' @export
#' @details This function is called by `makeFiles()`. Doing a value replacement (making a new parameter file)
#' does not require `modifyFile()` called explicitly.
#'
#' @examples
#' modifyFile(newsortievals,xml1, newname)
#'
modifyFile <-function(paramFile, xml1, newname) {
  pf1 <- prepareFile(paramFile)
  if (!is.null(ncol(pf1))) {#usual file type with variables on the lines and values in columns
    ncols <- ncol(pf1)-1
    xml2 <- replaceInfo(xml1, VariableNames, pf1, ncols, newname)
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

    xml2 <- replaceLines(xml1, pf1)
  }
  return(xml2)
}



