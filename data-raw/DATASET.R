## Find a dataset compatible with all functions of tabkris2 :
# At least two continuous variable, with and without missing data
# At least three binary variable, 0/1, Male/Female and one for varint
# At least two categorial variable, one for description, one for varint
# At least one ordinal variable
# Varint should approximately display equal group or at least not small groups

library(mice)

boys <- boys

B7 <- readRDS("data-raw/data.rds")

usethis::use_data(boys, overwrite = T, compress = "gzip")

usethis::use_data(B7, overwrite = T, compress = "gzip")

