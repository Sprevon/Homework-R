# first Principle analysis
communityData <- read.csv("D:\\Program\\DataSpell\\datafile\\R-Class\\quiz\\midterm\\data\\LA.csv")
firstPrinciple <- princomp(communityData[, -1], cor = TRUE)
summary(firstPrinciple)
screeplot(firstPrinciple,type="barplot")

# factor analysis
factorAnalysis <- factanal(communityData[, -1], factors = 6, scores = "Bartlett")
factorAnalysis$loadings