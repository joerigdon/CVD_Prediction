##Load necessary packages
#install.packages('docxtractr')
library(docxtractr)
#library(officer)
#source("/Users/joerigdon/Documents/Sanjay/Code/Evaluate.R")
source("/Users/joerigdon/Documents/Useful_Functions/Tables_v2.R")

##Read in supplemental tables to manipulate
tabs = docx_extract_all_tbls(read_docx('/Users/joerigdon/Documents/Sanjay/Tables/Model_results_2019-09-25.docx'))

t1a = as.data.frame(tabs[[1]])
t1b = as.data.frame(tabs[[2]])
t2a = as.data.frame(tabs[[3]])
t2b = as.data.frame(tabs[[4]])
t3a = as.data.frame(tabs[[5]])
t3b = as.data.frame(tabs[[6]])
t4a = as.data.frame(tabs[[7]])
t4b = as.data.frame(tabs[[8]])
t5a = as.data.frame(tabs[[9]])
t5b = as.data.frame(tabs[[10]])
t6a = as.data.frame(tabs[[11]])
t6b = as.data.frame(tabs[[12]])

getCr = function(test) {
gnd = as.numeric(substr(test[, 3], 1, 6))
cstat = as.numeric(substr(test[, 4], 1, 6))
round((gnd-1)^2 + (cstat-1)^2, 4)
}

tabCr = as.data.frame(cbind(
getCr(t1a), getCr(t1b), getCr(t2a), getCr(t2b), getCr(t3a), getCr(t3b), getCr(t4a), getCr(t4b), getCr(t5a), getCr(t5b), getCr(t6a), getCr(t6b)
))

#library(officer)
word.tab(tab=tabCr, dest="/Users/joerigdon/Documents/Sanjay/Criteria_output.docx", help=FALSE)
write.csv(tabCr, "/Users/joerigdon/Documents/Sanjay/Criteria_output.csv", row.names=FALSE)


#read.table("/Users/joerigdon/Documents/Sanjay/Code/Model6C_output.docx")
