
# Load Library ------------------------------------------------------------

library(edgar)
## Not run:
output <- getFilings(
  cik.no = 'ALL', form.type = "S-1", filing.year = 2014:2019,
  downl.permit = "y", useragent = "mibj17ad@student.cbs.dk")
#
# SESSION2 FINAL ----------------------------------------------------------
#FIles
# Get all files in all subdirectories in the current working directory 
# Current working directory only has the `Edgar filings_full text` and 
# `Master Indexes` folders with all the txt files for the year 2016
files_all_dir <- list.files(path = ".", pattern = "*.txt", recursive = T)
print(paste("Number of files: ", length(files_all_dir))) #715 for 2016

# Loop to read and get count of occurence of the words
all_data <- data.frame()#COMPANY_NAME = NA, ENVIRONMENT = NA)
for (i in 1:length(files_all_dir)){
  print(files_all_dir[i])
  x <- readLines(con = files_all_dir[i])
  COMPANY_NAME <- x[grepl(pattern = "COMPANY CONFORMED NAME:.*", x = x, ignore.case = T)]
  COMPANY_NAME <- strsplit(COMPANY_NAME, split = "\t")[[1]]
  COMPANY_NAME <- COMPANY_NAME[length(COMPANY_NAME)]
  #
  idxs_env = grep(pattern = "\\<environment.*\\>", x = x, ignore.case = T)
  vec_env <- c(strsplit(paste(x[idxs_env], collapse = " ")," ")[[1]]); #print(vec_env)
  vec2_env <- grep(pattern = "environment.*", x = vec_env, ignore.case = T)
  ENVIRONMENT_LEN <- length(vec2_env)
  #
  idxs_soc = grep(pattern = "social", x = x, ignore.case = T)
  vec_soc <- c(strsplit(paste(x[idxs_soc], collapse = " ")," ")[[1]]); #print(vec_soc)
  vec2_soc <- grep(pattern = "social", x = vec_soc, ignore.case = T)
  SOCIAL_LEN <- length(vec2_soc)
  #
  idxs_cop_gov = grep(pattern = "\\<corporate governance\\>", x = x, ignore.case = T)
  vec_cop_gov <- c(paste(x[idxs_cop_gov], collapse = " ")); #print(vec_cop_gov)
  vec2_cop_gov <- gregexpr("Corporate\\b \\bGovernance",vec_cop_gov, ignore.case = T)[[1]]
  COP_GOV_LEN <- length(vec2_cop_gov)
  COP_GOV_LEN
  #
  all_data <- rbind(
    all_data, 
    data.frame(
      COMPANY_NAME = COMPANY_NAME, ENVIRONMENT = ENVIRONMENT_LEN,
      SOCIAL = SOCIAL_LEN, CORPORATE_GOVERNANCE = COP_GOV_LEN))
}
#
View(all_data)
write.csv(x = all_data, file = "unaggregated_data.csv", row.names = F)
#
all_data_app <- aggregate(
  .~COMPANY_NAME, data = all_data, FUN = sum
)
View(all_data_app)
#
write.csv(x = all_data_app, file = "count_data.csv", row.names = F)
#
