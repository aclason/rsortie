VariableNames <- read.csv("D:/Github/r-SORTIE/VariableNames_default.csv", header=TRUE, strip.white = TRUE)

usethis::use_data(VariableNames, overwrite = TRUE)
