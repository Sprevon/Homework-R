# 定义常量
limitBooking <- 30
price <- 200
compensate <- 100

# 定义函数
calculateProfit <- function(passenger) {
  total <- 0
  if (passenger - 30 > 0) {
    overflow <- passenger - 30
    total <- 200 * passenger - 100 * overflow
  }else {
    total <- 200 * passenger
  }
  return(total)
}

# 定义计算变量
TWENTY_EIGHT <- calculateProfit(28)
TWENTY_NINE <- calculateProfit(29)
THIRTY <- calculateProfit(30)
THIRTY_ONE <- calculateProfit(31)
THIRTY_TWO <- calculateProfit(32)
THIRTY_THREE <- calculateProfit(33)
THIRTY_FOUR <- calculateProfit(34)

# 定义数据
booking31 <- data.frame(number = c(TWENTY_EIGHT, TWENTY_NINE, THIRTY, THIRTY_ONE),
                        probability = c(0.07, 0.26, 0.52, 0.15))
booking32 <- data.frame(number = c(TWENTY_EIGHT, TWENTY_NINE, THIRTY, THIRTY_ONE, THIRTY_TWO),
                        probability = c(0.06, 0.26, 0.5, 0.12, 0.06))
booking33 <- data.frame(number = c(TWENTY_EIGHT, TWENTY_NINE, THIRTY, THIRTY_ONE, THIRTY_TWO, THIRTY_THREE),
                        probability = c(0.06, 0.25, 0.49, 0.10, 0.06, 0.04))
booking34 <- data.frame(number = c(TWENTY_EIGHT, TWENTY_NINE, THIRTY, THIRTY_ONE, THIRTY_TWO, THIRTY_THREE, THIRTY_FOUR),
                        probability = c(0.05, 0.24, 0.48, 0.10, 0.06, 0.04, 0.03))
# 计算期望
expect31 <- sum(booking31$number * booking31$probability)
expect32 <- sum(booking32$number * booking32$probability)
expect33 <- sum(booking33$number * booking33$probability)
expect34 <- sum(booking34$number * booking34$probability)
compareData <- data.frame(names = c('31', '32', '33', '34'),
                          expects = c(expect31, expect32, expect33, expect34))
# 获取最大值
max_index <- which.max(compareData$expects)
maxExpect <- compareData$name[max_index]
print(paste("Best overbooking number: ", maxExpect))

# 得到：
# [1] "Best overbooking number:  34"