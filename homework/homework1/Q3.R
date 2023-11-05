# 第一次作业第三题
houseData <- read.csv("D:\\Program\\DataSpell\\datafile\\R-Class\\homework\\homework1\\data\\USA_Housing.csv")
model <- lm(AvgPrice ~ AvgAreaIncome + AvgAreaHouseAge +
AvgAreaNumberRooms + AvgAreaNumberofBedrooms + AreaPopulation,
            data = houseData)
summary(model)