##Load functions
source("/Users/joerigdon/Documents/Useful_Functions/Tables_v2.R")

dta = read.csv("/Users/joerigdon/Documents/Sanjay/Criteria_output.csv", header=TRUE)
word.tab(dta, dest="/Users/joerigdon/Documents/Sanjay/Criteria_output.docx")

dta2 = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Help_2019-09-29.csv", header=TRUE)
word.tab(dta2, dest="/Users/joerigdon/Documents/Sanjay/Tables/Help_2019-09-29.docx", help=FALSE)
