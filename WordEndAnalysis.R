install.packages("stringr")
library(stringr)
nouns = read.csv("nouns.csv")
nouns$finalSegment = str_match(nouns$noun,".$")
xtabs(~finalSegment+gender, nouns)
notAorO = subset(nouns, finalSegment!="a" & finalSegment!="o")
notAorO$finalSegment = str_match(notAorO$noun, ".$")
a = xtabs(~finalSegment+gender, notAorO)
notAorO$final2Segments = str_match(notAorO$noun, "..$")
b = xtabs(~final2Segments+gender, notAorO)
notAorO$final3Segments = str_match(notAorO$noun, "...$")
c = xtabs(~final3Segments+gender, notAorO)
notAorO$final4Segments = str_match(notAorO$noun, "....$")
d = xtabs(~final4Segments+gender, notAorO)

# make a list of accuracy values for each segment list:
getAccuracy <- function(x) {
if (x[1]>x[1,2]){
		accuracies = (x[1]/(x[1]+x[1,2]))
	}else{ 
		accuracies = (x[1,2]/(x[1]+x[1,2]))
} 
for(i in (2:nrow(x))) {
	if (x[i]>x[i,2]){
		accuracies = c(accuracies,(x[i]/(x[i]+x[i,2])))
	}else{ 
		accuracies = c(accuracies,(x[i,2]/(x[i]+x[i,2])))
}}
return(accuracies)
}

# sets of each segment list's accuracy %'s
e = getAccuracy(a)
f = getAccuracy(b)
g = getAccuracy(c)
h = getAccuracy(d)

# take samples of each 1,000 times w/ replacement:
sim1 = replicate(10000, mean(sample(e, replace=T)))
sim2 = replicate(10000, mean(sample(f, replace=T)))
sim3 = replicate(10000, mean(sample(g, replace=T)))
sim4 = replicate(10000, mean(sample(h, replace=T)))

# find overlap between each:
diff1VS2 = 100*mean(sim1 %in% sim2)
diff2VS3 = 100*mean(sim2 %in% sim3)
diff3VS4 = 100*mean(sim3 %in% sim4)