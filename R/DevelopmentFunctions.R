
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

  #the only way I can figure it out right now is to split the xml grids out

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
  mapname <- rep(NA, length(colpos))
  #float
  #number and names of the float grids
  grid_chunks <- xml_find_all(grid_data, ".//grid")
  grid_names <- xml_attr(xml_find_all(grid_data, ".//grid"),"gridName")

  colnames_l <- list()
  colpos_l <- list()
  fieldname_l <- list()
  mapname_l <- list()
  for(iii in 1:length(grid_chunks)){
    codes <- xml_children(xml_find_all(grid_chunks[[iii]], ".//ma_floatCodes"))
    colnames_l[[iii]] <- xml_attr(codes, "label")
    colpos_t <- xml_text(codes)
    colpos_l[[iii]] <- as.numeric(as.character(colpos_t))
    fieldname_l[[iii]] <- rep("fl", length(colpos_t))
    mapname_l[[iii]] <- rep(gsub(" ","",grid_names[[iii]]),length(colpos_t))
  }
  colnames2 <- unlist(colnames_l)
  colpos2 <- unlist(colpos_l)
  fieldname2 <- unlist(fieldname_l)
  mapname2 <- unlist(mapname_l)

  #boolean
  codes <- xml_children(xml_find_all(grid_data, ".//ma_boolCodes"))
  colnames3 <- xml_attr(codes, "label")
  colpos3 <- xml_text(codes)
  colpos3 <- as.numeric(as.character(colpos3))
  fieldname3 <- rep("bool", length(colpos3))
  mapname3 <- rep(NA, length(colpos3))

  #packageIntCodes
  codes <- xml_children(xml_find_all(grid_data, ".//ma_packageIntCodes"))
  colnames4 <- xml_attr(codes, "label")
  colpos4 <- xml_text(codes)
  colpos4 <- as.numeric(as.character(colpos4))
  fieldname4 <- rep("pint", length(colpos4))
  mapname4 <- rep(NA, length(colpos4))

  #packageFloatCodes
  codes <- xml_children(xml_find_all(grid_data, ".//ma_packageFloatCodes"))
  colnames5 <- xml_attr(codes, "label")
  colpos5 <- xml_text(codes)
  colpos5 <- as.numeric(as.character(colpos5))
  fieldname5 <- rep("pfl", length(colpos5))
  mapname5 <- rep(NA, length(colpos5))

  #append them all
  colpos <- c(colpos, colpos2, colpos3, colpos4, colpos5)
  colnames <- c(colnames, colnames2, colnames3, colnames4, colnames5)
  fieldname <- c(fieldname, fieldname2, fieldname3, fieldname4, fieldname5)
  mapname <- c(mapname, mapname2, mapname3, mapname4, mapname5)

  all_headers <- data.table(
    mapnm = mapname,
    fieldn = fieldname,
    colnames = colnames,
    colpos = colpos
  )

  # have to make a special join column
  # need to join by fieldname and column index - this doesn't work b/c there's multiple identical join names
  all_headers$join <- paste0(all_headers$fieldn, "_",
                             all_headers$colpos)

  map_dat_pts <- list()
  #we need to loop through the grids individually to track which grid holds which float code
  for(ix in 1:length(grid_chunks)){
    # Read the map
    map_loc_all <- xml_find_all(grid_chunks[[ix]], ".//ma_v")

    # build list object to export individual items
    map_list <- list()

    print(paste("Reading",grid_names[[ix]],"map data"))

    # Loop through each map header
    j <- 0
    for(i in 1:length(map_loc_all)){
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
     # pkgchildren <- children[xml_name(children)=="pkg"]  #these will nodes that we need to loop through.

      #if (length(pkgchildren)>0) {
       # for (k in 1:length(pkgchildren)) {
        #  subchildren <- xml_children(pkgchildren[k])

          #field names
         #  fieldname <- xml_name(subchildren)

          # Get the xml content (the values)
        #  cont <- xml_contents(subchildren)
         # values <- xml_text(cont)
        #  values <- as.numeric(as.character(values))

          # get the xml label names
         # colnames <- xml_attr(subchildren, "c")
        #  colnames <- as.numeric(as.character(colnames))

          # Build two export data frames
       #   ik<-as.numeric(paste0(i,".",k))
       #   this_values <- data.frame(
        #    fieldn = fieldname,
        #    colpos = colnames,
        #    values = values,
        #    point_id = ik
         # )
        #  this_point_set <- data.frame(point_id = ik, x = this_x, y = this_y)

       #   j<- j+1
        #   append values to list - will merge at end
        #    build_map_in_loop[[j]] <- this_values
        #   map_list[[j]] <- this_point_set
        #  }
   #   }

      if(i %% 100 == 0){
        pc <- round(i/length(map_loc_all), 2)*100
        print(paste0(pc, "% complete"))
      }

    }   #end points loop

    map_dat <- as.data.table(do.call("rbind", build_map_in_loop))
    map_points <- as.data.table(do.call("rbind", map_list))

    map_dat$join <- paste0(map_dat$fieldn, "_",
                           map_dat$colpos)
    # Merge together
    map_datj <- map_dat[,c("point_id", "join", "values")]
    map_datj$mapnm <- gsub(" ","",grid_names[ix])

    val_name_merge <- as.data.table(merge(map_datj, all_headers, by=c("mapnm","join"), all.x=TRUE))
    dat_pts <- merge(val_name_merge,map_points, by="point_id")
    map_dat_pts[[ix]] <- dat_pts[,.(mapnm,point_id,x,y,colnames,values)]
  }

  map_all_dat <- rbindlist(map_dat_pts)

  # Output dt
  return(map_all_dat)

}

#' Parse spatial files
#'
#' @description
#' `parseMapOld()` takes the output XML file name and returns the outputs stored in maps.
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
parseMapOld <-function(xmlname){
  #Routine will take the file xmlname (assume it is a SORTIE output file)
  #it will return the file sprd

  #This routine is just a map portion of the output file, assuming it is there
  `%>%` <- magrittr::`%>%`
  library(xml2)

  grid_data <- read_xml(xmlname)

  #read the map file notes
  #grid_loc <- xml_find_all(grid_data, ".//grid")


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

    #if (length(pkgchildren)>0) {
    # for (k in 1:length(pkgchildren)) {
    #  subchildren <- xml_children(pkgchildren[k])

    #field names
    # fieldname <- xml_name(subchildren)

    # Get the xml content (the values)
    #cont <- xml_contents(subchildren)
    #values <- xml_text(cont)
    #values <- as.numeric(as.character(values))

    # get the xml label names
    #colnames <- xml_attr(subchildren, "c")
    #colnames <- as.numeric(as.character(colnames))

    # Build two export data frames
    #ik<-as.numeric(paste0(i,".",k))
    #this_values <- data.frame(
    #  fieldn = fieldname,
    # colpos = colnames,
    #  values = values,
    #  point_id = ik
    #)
    #this_point_set <- data.frame(point_id = ik, x = this_x, y = this_y)

    #j<- j+1
    # append values to list - will merge at end
    #  build_map_in_loop[[j]] <- this_values
    # map_list[[j]] <- this_point_set
    #}
    #}

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
  # need to join by fieldname and column index - this doesn't work b/c there's multiple identical join names
  all_headers$join <- paste0(all_headers$fieldn, "_",
                             all_headers$colpos)

  all_headers$altjo <- paste0(all_headers$colnames, "_",
                              all_headers$colpos)

  map_dat$join <- paste0(map_dat$fieldn, "_",
                         map_dat$colpos)

  map_dat$altjo <- paste0(map_dat$colnames, "_",
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
