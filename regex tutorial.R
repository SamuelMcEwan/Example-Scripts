
sub("_", "", test)
?sub
test <- c("asdbjk_dje12_asndiofe3_alfej")


sub("^([^_]+_){1}", "\\1", test)

test1 <- c("a_dddd2_as_a", "l_o.+_d_3")
test2 <- c("Shootings", "suspectS", "SNAKES", "a")

sub("S", "#", test2) # Will replace the first occurence of the capital letter S.  
sub("[Ss]", "#", test2) # Will replace the first occurence of either s or S. Same as sub(s|S, "#", test2  
sub("s|S", "#", test2) # Will replace the first occurence of either s or S.  

gsub("S", "#", test2) # Will replace ALL occurences of the capital letter S.  
gsub("[Ss]", "#", test2) # Will replace ALL occurence of either s or S. Same as gsub(s|S, "#", test2)  
gsub("s|S", "#", test2) # Will replace ALL occurences of either s or S.  

sub("_", "#", test)
sub("_.*", "##", test)
sub(".*_", "##", test)
sub("_.*_", "##", test)
sub("_.*a", "##", test) # There must be an a after the first _

# test <- 
  
test <- c("a_dddd2_as_a", "l_o.o.+_d_3")
sub("_", "#", test) # Replaces first "_"
sub("_d", "#", test) # Replaces first "_d"
sub("_d*", "#", test) # Replaces first "_" but if there is also any number of consecutive trailing d's, these are also replaced 
sub("_d+", "#", test) # Replaces first "_d" but if there are more consecutive trailing d's, these are also replaced. 
sub("_d{2}", "#", test) # Replaces first "_" followed by 2 "d"
sub("_d{2,}", "#", test) # Replaces first "_" followed by 2 or more "d"
sub("_d{2,3}", "#", test) # Replaces first "_" followed by 2 up to 3 "d"
sub("_d{1,3}", "#", test) # Replaces first "_" followed by 1 up to 3 "d"
sub("_(o.)*", "#", test) # Replaces first "_" but if there is also any number of consecutive trailing (d2)'s, these are also replaced 


sub("^[^_]*_([^_]*)_", "#", test)



test2 <- "hello_world_2012_is_not the end of the world.pdf"
sub("_([^_]*)_", "#", test2) # There must be at-least one opening and closing underscore.

sub("_([^_]*)_", "#", test2) # There must be at-least one opening and closing underscore.


sub("^[^_]*_([^_]*)_", "#", test2)



gregexpr(pattern = "_", test)
str_locate_all(pattern ="_", test)

strsplit(test, )
?strsplit


gregexpr(pattern ='2',"the2quickbrownfoxeswere2tired")

stri_locate_all(pattern = '2', "the2quickbrownfoxeswere2tired", fixed = TRUE)





# This link seems helpful
# https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285


# I wish to extract d2 and o.+ as they are between the first and second underscore
sub("_", "", test) # Deletes the first underscore in the string
sub("_.*", "", test) # Deletes everything from the first underscore onswards in the string
sub(".*_", "", test) # Deletes everything before and including the last underscore in the string


sub("_", "", test) # Deletes everything before and including the last underscore in the string

sub("[^_]{1}", "", test) # Deletes everything before and including the last underscore in the string
sub("[^_]{2}", "", test) # Deletes everything before and including the last underscore in the string
sub("[^_]{3}", "", test) # Deletes everything before and including the last underscore in the string

# 



unlist(strsplit(test2, "_"))[2]



x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
strsplit(x, "e")

unlist(strsplit("a.b.c", "."))
## [1] "" "" "" "" ""
## Note that 'split' is a regexp!
## If you really want to split on '.', use
unlist(strsplit("a.b.c", "[.]"))
## [1] "a" "b" "c"
## or
unlist(strsplit("a.b.c", ".", fixed = TRUE))

## a useful function: rev() for strings
strReverse <- function(x){
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")}
strReverse(c("abc", "Statistics"))

## get the first names of the members of R-core
a <- readLines(file.path(R.home("doc"),"AUTHORS"))[-(1:8)]
a <- a[(0:2)-length(a)]
(a <- sub(" .*","", a))
# and reverse them
strReverse(a)

## Note that final empty strings are not produced:
strsplit(paste(c("", "a", ""), collapse="#"), split="#")[[1]]
# [1] ""  "a"
## and also an empty string is only produced before a definite match:
strsplit("", " ")[[1]]    # character(0)
strsplit(" ", " ")[[1]]   # [1] ""



sub("^([^_]_)", "", test)


sub("^([^_]){1}", "\\1", test)
