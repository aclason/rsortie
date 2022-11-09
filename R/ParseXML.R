#' Parse output data frame
#'
#' @description
#' `parseXML()` takes the SORTIE output XML file name and returns the file output data frame
#'
#' @param xmlname [character()] XML file name
#'
#' @return
#' The file output data frame.
#' @export
#'
#' @examples
#' parseXML(xmlname)
#'
parseXML <- function(xmlname){
  #Routine will take the file xmlname (assume it is a SORTIE output file)
  #it will return the file output_df
  #it takes awhile!

  #Note, it has only been tested with a tree output file

  # Set default working directory for trial
  #dir_base <- "C:/Users/mbayly/Desktop/Projects/SarahBeukema/"
  #setwd(dir_base); list.files()


  # Load external libraries to help parse xml
  library(xml2) # may need to install...
  `%>%` <- dplyr::`%>%`
  #library(dplyr)
  #library(tidyr)

  #set tree type codes
  tp_name <- matrix(c(1:6,"Seedling","Sapling","Adult","Stump","Snag","Woody"),nrow=6,ncol=2)
  colnames(tp_name) <- c("tp","Type")

  # Open xml file
  # dat <- read_xml("Alana/ICH-A4-p_det_9.xml.gz")
  dat <- read_xml(xmlname)

  # Read the tree species
  tree_species <- xml_find_all(dat, "//tm_species")
  (tree_species_all <- xml_attr(tree_species, "speciesName"))




  #---------------------------------------------------------
  # Loop through each tree header group - probably a faster way to do this
  # also not even sure if this is necessary...

  # Get xml nodes of all trees
  all_trees <- xml_find_all(dat, ".//tm_treeSettings"); length(all_trees)

  # build list object to export data
  build_list_in_loop <- list()


  # Loop through each tree header group
  for(i in 1:length(all_trees)){

    # for debugging
    # i = 1

    # Filter for the current tree group
    this_tree <- all_trees[[i]]

    # Get the group species name
    this_species <- xml_attr(this_tree, "sp")

    # Get the group tp value
    this_tp <- xml_attr(this_tree, "tp")

    # The go one level deeper to the tm_floatCodes

    # first child - usually alive or dead
    child <- xml_child(this_tree)
    cont <- xml_contents(child)
    colnames <- xml_attr(cont, "label")
    colpos <- xml_text(cont)
    colpos <- as.numeric(as.character(colpos))


    # make sure to get alive and dead attr for
    # some trees
    if( length(xml_children(this_tree)) == 2){

      child2 <- xml_child(this_tree, search=2)
      cont2 <- xml_contents(child2)
      colnames2 <- xml_attr(cont2, "label")
      colpos2 <- xml_text(cont2)
      colpos2 <- as.numeric(as.character(colpos2))

      # append
      colpos <- c(colpos, colpos2)
      colnames <- c(colnames, colnames2)

    }


    # Build export datadframe

    this_values <- data.frame(
      tree_species = this_species,
      tp = this_tp,
      colnames = colnames,
      colpos = colpos,
      counter = i
    )

    # append values to list - will merge at end
    build_list_in_loop[[i]] <- this_values

  } # end of loop through i header groups....


  # merge and combine all headers
  all_headers <- do.call("rbind", build_list_in_loop)

  # Replace dead with -1
  all_headers$colpos <- ifelse((all_headers$colpos == 0 || all_headers$colpos == 2) & all_headers$colnames == "dead", -1, all_headers$colpos)

  # end of header info
  head(all_headers)




  #=====================================================
  # DATA: Get data for each tree

  # Get xml nodes of all trees
  all_tree_dat <- xml_find_all(dat, ".//tree"); length(all_tree_dat)

  # build list object to export individual tree data
  build_list_in_loop <- list()

  #print("Reading tree data")


  # Loop through each tree header group
  for(i in 1:length(all_tree_dat)){


    # for debugging
    # i = 8098

    # Filter for the current tree
    this_tree <- all_tree_dat[[i]]

    # Get the species name
    this_species <- xml_attr(this_tree, "sp")

    # Get the tp value
    this_tp <- xml_attr(this_tree, "tp")

    # Get the index labels and values
    children <- xml_children(this_tree)

    # Get the xml content (the values)
    cont <- xml_contents(children)
    values <- xml_text(cont)
    values <- as.numeric(as.character(values))

    # get the xml label names
    colnames <- xml_attr(children, "c")
    colnames <- as.numeric(as.character(colnames))

    # set first value to -1 for dead to avoid dup zero
    c <- as.character(children[[1]])
    mstart <- c(0,0)
    if(grepl("int", c) & all(colnames[1:2] == mstart)){
      colnames[1] <- -1
    }
    mstart <- c(2,0)
    if(grepl("int", c) & all(colnames[1:2] == mstart)){
      colnames[1] <- -1
    }


    # Build export data frame
    this_values <- data.frame(
      tree_species_id = this_species,
      tp = this_tp,
      colpos = colnames,
      values = values,
      tree_id = i
    )

    # append values to list - will merge at end
    build_list_in_loop[[i]] <- this_values


    # Go one level deeper
    #if(length(xml_children(this_tree)) != 8){
    #  stop("didnt code for this... MJB")
    #}

    if(i %% 5000 == 0){
      pc <- round(i/length(all_tree_dat), 2)*100
      print(paste0(pc, "% complete"))
    }

  } # end of loop through i trees ......


  tree_dat <- do.call("rbind", build_list_in_loop)
  #head(tree_dat)







  #==================================================

  # Merge data and fix headers
  head(all_headers)
  head(tree_dat)

  # Add on the species column
  tree_dat$tree_species_id <- as.numeric(as.character(tree_dat$tree_species_id))
  tree_dat$tree_species <- tree_species_all[(tree_dat$tree_species_id + 1)]

  # Merge the headers to the data
  # have to make a special join column
  # need to join by tree species, tp code and column index
  all_headers$join <- paste0(all_headers$tree_species, "_",
                             all_headers$tp, "_",
                             all_headers$colpos)


  tree_dat$join <- paste0(tree_dat$tree_species, "_",
                          tree_dat$tp, "_",
                          tree_dat$colpos)


  # Merge together
  tree_datj <- tree_dat[,c("tree_id", "join", "values")]

  big_merge <- merge(tree_datj, all_headers, by.x="join", by.y="join", all.x=TRUE)
  head(big_merge)
  nrow(big_merge)


  # Convert from long format to short format
  long_f2 <- big_merge[,c("tree_species", "tp", "tree_id", "values", "colnames")]
  unique(long_f2$colnames)

  long_f <- merge(long_f2, tp_name, by="tp")

  #write.csv(long_f, file="longf_out.csv")


  # Fix format
  sprd <- long_f %>%  tidyr::spread(colnames, values)

  #plot(sprd$X, sprd$Y, pch='.', col=as.factor(sprd$tree_species))


  # Output df
  output_df <- sprd
  #head(output_df)
  #str(output_df)

  #outf<-stringr::str_replace(xmlname,".xml",".csv")
  #write.csv(output_df, file=outf)

  return(output_df)
}



