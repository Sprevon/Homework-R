industryData <- read.csv("D:\\Program\\DataSpell\\datafile\\R-Class\\quiz\\midterm\\data\\industries.csv", header = TRUE)
# 拆分文字项，处理数据
location <- industryData$`地区`
industries <- industryData[, -1]
scaled <- scale(industries)
row.names(industries) <- location
# 分析处理
industryAnalysis <- dist(industries, diag = TRUE)
hc <- hclust(industryAnalysis)
# 可视化
par(mfrow=c(2,1),mar=c(0,4,4,0),cex=1.5)
plot(hc ,hang=-1, family='SimHei')
# 输出
result<-cutree(hc,k=3)
print(result)

# 结果图见 quiz/midterm/photo/Rplot02.png