#读取csv
bloodData <- read.csv("D:\\Program\\DataSpell\\datafile\\R-Class\\homework\\homework1\\data\\blood.csv")
model <- lm(HGB ~ Ca + Mg + Fe + Mn + Cu, data = bloodData)
summary(model)