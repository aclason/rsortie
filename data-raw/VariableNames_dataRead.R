VariableNames <- read.csv("D:/Github/r-SORTIE/Data_rsortiePkg/VariableNames_default.csv", header=TRUE, strip.white = TRUE)

usethis::use_data(VariableNames, overwrite = TRUE)

res <- xml2::read_xml(paste0("D:/Github/r-SORTIE/Data_rsortiePkg/GMF.xml"))
xml2::write_xml(res, "D:/Github/r-SORTIE/temp.xml")
tmp <- readLines("D:/Github/r-SORTIE/temp.xml", encoding="UTF-8")
samplebasexml <- gsub("\\\\", "//",tmp)

usethis::use_data(samplebasexml, overwrite = TRUE)

gmf_mort_new <- read.csv("D:/Github/r-SORTIE/Data_rsortiePkg/gmf_mort_new.csv")

usethis::use_data(gmf_mort_new, overwrite=TRUE)

gmf_time_new <- read.csv("D:/Github/r-SORTIE/Data_rsortiePkg/gmf_time_new.csv")

usethis::use_data(gmf_time_new, overwrite=TRUE)
