library(stringr)
rm(list = ls())

##### String Processing/Manipulation #####
text <- c("nose", "letter38", "window9", "apple0")
grep("[0-9]", text) # identify positions of elements which include numbers
grep("[0-9]", text, value = TRUE)  # report elements which include numbers
grep("[0-9]", text, value = TRUE) %>% gsub("[0-9]", "", .) # report the non-numeric characters for elements which include numbers

# Gsub Examples 
text <- c("X_ABC_extract1_DF", 
          "A_NJU_extract2_PP",
          "J_H1H",
          "II_00_extract4_PP")
str_split(text, "_", simplify = T)[,3] # Extract text between 2nd and third underscore if it exists otherwise return an empty cell
gsub("a", "[a]", text)
gsub("a|A", "[a]", text)

test <- c("a_dddd2_as_a", "l_o.o.+_d_3")
sub("_", "", test) # Replaces first "_"
sub("_d", "#", test) # Replaces first "_d"
sub("_d*", "#", test) # Replaces first "_" but if there is also any number of consecutive trailing d's, these are also replaced 
sub("_d+", "#", test) # Replaces first "_d" but if there are more consecutive trailing d's, these are also replaced. 
sub("_d{2}", "#", test) # Replaces first "_" followed by 2 "d"
sub("_d{2,}", "#", test) # Replaces first "_" followed by 2 or more "d"
sub("_d{2,3}", "#", test) # Replaces first "_" followed by 2 up to 3 "d"
sub("_d{1,3}", "#", test) # Replaces first "_" followed by 1 up to 3 "d"
sub("_(o.)*", "#", test) # Replaces first "_" but if there is also any number of consecutive trailing (d2)'s, these are also replaced 
