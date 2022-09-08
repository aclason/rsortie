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


#' Parse spatial files
#'
#' @description
#' `parseMap()` takes the output XML file name and returns the outputs stored in maps.
#'
#' @param xmlname [character()] XML file name
#'
#' @return
#' The output contained in grids from SORTIE run as a data frame
#' @export
#'
#' @examples
#' parseMap(grid_data)
#'
parseMap <-function(xmlname){
  #Routine will take the file xmlname (assume it is a SORTIE output file)
  #it will return the file sprd

  #This routine is just a map portion of the output file, assuming it is there
  `%>%` <- magrittr::`%>%`
  library(xml2)

  grid_data <- read_xml(xmlname)

  #read the map file notes
  #grid_loc <- xml_find_all(dat, ".//grid")


  #+++++++++++++++++++++++++++
  #start the actual routine
  #++++++++++++++++++++++++++

  # build list object to export data
  build_map_in_loop <- list()

  #find the labels for this map
  #integer
  codes <- xml_children(xml_find_all(grid_data, ".//ma_intCodes"))
  colnames <- xml_attr(codes, "label")
  colpos <- xml_text(codes)
  colpos <- as.numeric(as.character(colpos))
  fieldname <- rep("int", length(colpos))

  #float
  codes <- xml_children(xml_find_all(grid_data, ".//ma_floatCodes"))
  colnames2 <- xml_attr(codes, "label")
  colpos2 <- xml_text(codes)
  colpos2 <- as.numeric(as.character(colpos2))
  fieldname2 <- rep("fl", length(colpos2))

  #boolean
  codes <- xml_children(xml_find_all(grid_data, ".//ma_boolCodes"))
  colnames3 <- xml_attr(codes, "label")
  colpos3 <- xml_text(codes)
  colpos3 <- as.numeric(as.character(colpos3))
  fieldname3 <- rep("bool", length(colpos3))

  #packageIntCodes
  codes <- xml_children(xml_find_all(grid_data, ".//ma_packageIntCodes"))
  colnames4 <- xml_attr(codes, "label")
  colpos4 <- xml_text(codes)
  colpos4 <- as.numeric(as.character(colpos4))
  fieldname4 <- rep("pint", length(colpos4))

  #packageFloatCodes
  codes <- xml_children(xml_find_all(grid_data, ".//ma_packageFloatCodes"))
  colnames5 <- xml_attr(codes, "label")
  colpos5 <- xml_text(codes)
  colpos5 <- as.numeric(as.character(colpos5))
  fieldname5 <- rep("pfl", length(colpos5))


  #append them all
  colpos <- c(colpos, colpos2, colpos3, colpos4, colpos5)
  colnames <- c(colnames, colnames2, colnames3, colnames4, colnames5)
  fieldname <- c(fieldname, fieldname2, fieldname3, fieldname4, fieldname5)

  all_headers <- data.frame(
    fieldn = fieldname,
    colnames = colnames,
    colpos = colpos
  )


  # Read the map
  map_loc_all <- xml_find_all(grid_data, ".//ma_v")

  # build list object to export individual items
  map_list <- list()

  #print("Reading map data")

  # Loop through each map header
  j <- 0
  for(i in 1:length(map_loc_all)){
    #for(i in 1:10){

    # for debugging
    #i <- 1

    # Filter for the current tree
    this_point <- map_loc_all[[i]]

    # Get the x location
    this_x <- xml_attr(this_point, "x")
    # Get the y location
    this_y <- xml_attr(this_point, "y")

    # Get the index labels and values
    # Note that these children may also have children if they are a package <pkg>
    children <- xml_children(this_point)

    #In case we have packages, first deal with the NON-packages
    subchildren <- children[xml_name(children)!="pkg"]

    #field names
    fieldname <- xml_name(subchildren)

    # Get the xml content (the values)
    cont <- xml_contents(subchildren)
    values <- xml_text(cont)
    values <- as.numeric(as.character(values))

    # get the xml label names
    colnames <- xml_attr(subchildren, "c")
    colnames <- as.numeric(as.character(colnames))


    # Build two export data frames
    this_values <- data.frame(
      fieldn = fieldname,
      colpos = colnames,
      values = values,
      point_id = i
    )
    this_point_set <- data.frame(point_id = i, x = this_x, y = this_y)

    j<- j+1
    # append values to list - will merge at end
    build_map_in_loop[[j]] <- this_values
    map_list[[j]] <- this_point_set

    #now we need to repeat this for packages, if present
    pkgchildren <- children[xml_name(children)=="pkg"]  #these will nodes that we need to loop through.

    if (length(pkgchildren)>0) {
      for (k in 1:length(pkgchildren)) {
        subchildren <- xml_children(pkgchildren[k])

        #field names
        fieldname <- xml_name(subchildren)

        # Get the xml content (the values)
        cont <- xml_contents(subchildren)
        values <- xml_text(cont)
        values <- as.numeric(as.character(values))

        # get the xml label names
        colnames <- xml_attr(subchildren, "c")
        colnames <- as.numeric(as.character(colnames))

        # Build two export data frames
        ik<-as.numeric(paste0(i,".",k))
        this_values <- data.frame(
          fieldn = fieldname,
          colpos = colnames,
          values = values,
          point_id = ik
        )
        this_point_set <- data.frame(point_id = ik, x = this_x, y = this_y)

        j<- j+1
        # append values to list - will merge at end
        build_map_in_loop[[j]] <- this_values
        map_list[[j]] <- this_point_set
      }
    }

    if(i %% 100 == 0){
      pc <- round(i/length(map_loc_all), 2)*100
      print(paste0(pc, "% complete"))
    }

  }   #end points loop

  map_dat <- do.call("rbind", build_map_in_loop)
  map_points <- do.call("rbind", map_list)

  #MERGE data and fix headers
  # Merge the headers to the data
  # have to make a special join column
  # need to join by fieldname and column index
  all_headers$join <- paste0(all_headers$fieldn, "_",
                             all_headers$colpos)

  map_dat$join <- paste0(map_dat$fieldn, "_",
                         map_dat$colpos)

  # Merge together
  map_datj <- map_dat[,c("point_id", "join", "values")]

  big_merge <- merge(map_datj, all_headers, by.x="join", by.y="join", all.x=TRUE)
  #head(big_merge)
  nrow(big_merge)

  #get rid of some columns
  smaller <- big_merge[,!(names(big_merge) %in% c("join","colpos","fieldn"))]

  # Fix format
  #library(tidyr)
  sprd <- smaller %>% tidyr::spread(colnames, values)
  sprd <- merge(sprd, map_points, by.x="point_id", by.y="point_id", all.x=TRUE)
  #head(sprd)


  # Output df
  return(sprd)

}
