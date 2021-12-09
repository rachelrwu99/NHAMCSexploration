##Cleaning NHAMCS
<<<<<<< HEAD


nhames <- nhamcs[3:n,] #get rid of first three
#nhamcs_clean <- nhamcs 
#nhamcs_clean %>% select(c())

nhamcs_clean <- nhamcs[ , 1:345]  %>% #got rid of therapeutic drug categorizations, controlled substance status code, composition status code
  select(-c("nopay")) 
               

##Variable of interest hddiag1: visits that resulted in a hospital admissions

###To Do:
  ##take out med and med1-12
  ##take out "gpmed 1-12" 

  ##remove ones with N/A, -9, -8, -6
  
=======
nhames <- nhamcs[3:n,] #get rid of first three
nhamcs_clean <- nhamcs 
nhamcs_clean %>% select(c())
>>>>>>> 46adb6913f92d9b30be39843da6c96d65cc2b8fb
