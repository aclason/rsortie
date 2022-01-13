library(data.table)
VariableNames <- fread("D:/Github/SORTIEparams/Inputs/ICH/VariableNames_DateCreek.csv", header=TRUE, strip.white = TRUE)

usethis::use_data(VariableNames)
