#' Replace Info
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

###############################################################
###############################################################
###############################################################
#Function to replace a chunk of the file with a different set of lines
#It assumes the new set of lines are complete and completely replace relevant section
###############################################################

#' Replace Lines
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

  grouptext <- substring(pf1[firstline],str_locate(pf1[firstline], "<")+1,str_length(pf1[firstline])-1)[1]

  #now remove the numbers from the end of the grouptext. Note that there can be more than 1 number (eg Plant24)
  gtext <- str_replace_all(grouptext, "[:digit:]", "")

  #If the grouptext is a harvest one, then we will do it differently than if it isn't
  #This allows a chunk of harvest related code to have several different bits in it and they will all be replaced.

  harvestext <- c("EpisodicMortality","SelectionHarvest","Harvest","Plant")

  #if (grouptext %in% harvestext) {
  # if (gtext %in% harvestext) {
  #     #so now we know this is harvest.
  #   #Find the lines in the harvest file for each group listed above
  #
  #   for (i in 1:length(harvestext)) { #added 1:length()
  #     linegroup <- grep(harvestext[i],pf1) #AC: I added harvestext to index over, but not sure if this is right(SB: could be done either way. this way i is an index. The original way i was the text in the loop)
  #     #print(linegroup)
  #     if (length(linegroup)>0) {
  #       in_harvest <- str_trim(str_replace_all(pf1[linegroup[1]],c("<"="",">"="")))  #get the actual name of the group we are replacing
  #       lnm <- grep(in_harvest,rf)     #look for this text inside the main file. This should return two values: the beginning and end of the group
  #       #print(i)
  #       #print(paste(lnm, linegroup[1],linegroup[2], in_harvest))
  #       if (length(lnm)>0) {
  #         x <- RemoveRow(lnm, rf)
  #         rf2 <- append(x,pf1[linegroup[1]:linegroup[2]],lnm[1]-1)
  #
  #       }  else {
  #         print(paste("the group: ",in_harvest,"was not found in the main file."))
  #       }
  #
  #       #print(paste("appended the lines ",lnm[1]-1, linegroup[1]))
  #       rf <- rf2
  #     }
  #
  #   }
  #
  # }
  # else {

  #loop through the file
  #    We will do a chunk, see if there is more, and do that, until it is all gone.
  i <-1
  while (i <length(pf1)) {
    #"i" has the group name that we are replacing
    #if the next line contains -999 then we are not replacing anything, just removing
    bRemove <- FALSE
    bRemove <- grepl("-999",pf1[i+1],fixed=TRUE)

    #get the beginning and end line of this group in the INPUT file
    grouptext <- str_trim(str_replace_all(pf1[i],c("<"="",">"="")))  #get the name of the group we are replacing

    lni <- grep(grouptext,pf1)

    #find the text inside the MAIN file
    grouptext <- str_trim(pf1[i])        #get the name of the group we are replacing
    endgroup <- str_replace_all(grouptext,"<","</")
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
