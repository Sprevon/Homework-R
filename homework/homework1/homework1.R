# 生成随机数列
randomList <- rnorm(100, mean=0, sd=1)
# 生成在负无穷到-1间数量的统计变量
a1 <- 0
# 生成-1到0间数量统计的变量
a2 <- 0
# 生成0到1间数量统计的变量
a3 <- 0
# 生成1到正无穷间数量的统计变量
a4 <- 0
# 遍历统计
for (i in 1:100){
  randomValue <- randomList[i]
  if(randomValue <= -1){
    a1 <- a1 + 1
  }else if(randomValue > -1 && randomValue <= 0){
    a2 <- a2 + 1
  }else if(randomValue > 0 && randomValue <= 1){
    a3 <- a3 + 1
  }else{
    a4 <- a4 + 1
  }
}
print("---------------------------")
print(c("number less than -1: ", a1))
print(c("number between -1 and 0", a2))
print(c("number between 0 and 1", a3))
print(c("number larger than 1", a4))

print("-------------------------")
sort(randomList, decreasing = TRUE)
print(randomList)

