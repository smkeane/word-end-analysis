# imports stringr library
install.packages("stringr")
library(stringr)

# accesses nouns.csv file that has noun info in it
nouns = read.csv("nouns.csv")
# make a list of the final letter from each word
nouns$finalSegment = str_match(nouns$noun,".$")
# makes table of terminal letters and the number of times words with that terminal letter were male or female
xtabs(~finalSegment+gender, nouns)

# creates a subset of words that don't end in 'a' or 'o'
notAorO = subset(nouns, finalSegment!="a" & finalSegment!="o")

# makes a column of all the last letters of the words in notAorO called finalSegment
notAorO$finalSegment = str_match(notAorO$noun, ".$")
# creates a table, a, of the terminal letters and the number of times their word was male or female
a = xtabs(~finalSegment+gender, notAorO)
# makes a column and then a table for the terminal 2 letters
notAorO$final2Segments = str_match(notAorO$noun, "..$")
b = xtabs(~final2Segments+gender, notAorO)
# same thing for terminal 3 letters
notAorO$final3Segments = str_match(notAorO$noun, "...$")
c = xtabs(~final3Segments+gender, notAorO)
# again for terminal 4 letters
notAorO$final4Segments = str_match(notAorO$noun, "....$")
d = xtabs(~final4Segments+gender, notAorO)

# function that makes a list of accuracy values for each table of terminal letters (tables a, b, c, and d):
getAccuracy <- function(x) {
# finds if word ending in certain terminal letter(s) more likely to be female or male
# appends probability that words ending in given terminal letter(s) will be this more likely gender
# example: from set a, words ending in 'e' are female 1 time and male 6 times, so since 6 > 1, 6/(1+6)=0.857, which is added to the accuracies set 
if (x[1]>x[1,2]){
		accuracies = (x[1]/(x[1]+x[1,2]))
	}else{ 
		accuracies = (x[1,2]/(x[1]+x[1,2]))
} 
# does same thing through rest of rows, adds probabilities to accuracies set
for(i in (2:nrow(x))) {
	if (x[i]>x[i,2]){
		accuracies = c(accuracies,(x[i]/(x[i]+x[i,2])))
	}else{ 
		accuracies = c(accuracies,(x[i,2]/(x[i]+x[i,2])))
}}
# returns set of numbers that show the accuracy of predicting a word's gender based on its terminal letters
return(accuracies)
}

# sets of each segment list's accuracy percentages 
# enter the set's name below to view the accuracies and the column name (a, b, c, or d) to see the rows the values correspond to
e = getAccuracy(a)
f = getAccuracy(b)
g = getAccuracy(c)
h = getAccuracy(d)

# take samples of each 1,000 times w/ replacement:
sim1 = replicate(10000, mean(sample(e, replace=T)))
sim2 = replicate(10000, mean(sample(f, replace=T)))
sim3 = replicate(10000, mean(sample(g, replace=T)))
sim4 = replicate(10000, mean(sample(h, replace=T)))

# find overlap between each of these samples:
# example: diff1VS2 gives a value to how similar the odds of guessing the gender correctly based on the last letter are to guessing based on the last two letters
diff1VS2 = 100*mean(sim1 %in% sim2)
diff2VS3 = 100*mean(sim2 %in% sim3)
diff3VS4 = 100*mean(sim3 %in% sim4)