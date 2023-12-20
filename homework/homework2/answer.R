library(caret)
library(tidyr)
library(corrplot)
library(VIM)

# 导包
peng <- read.csv('D:/Program/DataSpell/datafile/R-Class/homework/homework2/penguins_size.csv',
                 stringsAsFactors = F)
sex_label <- table(peng$sex)

# 删除无效数据.由于无效数据仅仅10条，可以选择删除
sum(!complete.cases(peng))
peng <- na.omit(peng)

# 获取热力图
peng_cor <- cor(peng[, 3:6])
corrplot.mixed(peng_cor)

# 获取不同种群特征
peng_spec <- peng[, -2]
plot_spec <- gather(peng_spec, key = "variable", value = "value", c(-species))
ggplot(plot_spec, aes(fill = species)) +
  theme_bw() +
  geom_density(aes(value), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

# Logistic回归种群特征
peng <- peng[, -7]
peng_spec <- peng[, -2]
peng_spec$species <- factor(peng_spec$species, levels = c("Adelie", "Gentoo", "Chinstrap"), labels = c( 0, 1, 2))
index <- createDataPartition(peng_spec$species, p = 0.7)
peng_spec_train <- peng_spec[index$Resample1,]
peng_spec_test <- peng_spec[-index$Resample1,]
peng_spec_lm <- glm(species ~ ., data = peng_spec_train, family = "binomial")
peng_spec_step <- step(peng_spec_lm, direction = "both")
summary(peng_spec_step)

# 获取位置特征
peng_isl <- peng[, -1]
plot_sil <- gather(peng_isl, key = "variable", value = "value", c(-island))
ggplot(plot_sil, aes(fill = island)) +
  theme_bw() +
  geom_density(aes(value), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")


# Logistic回归种群特征
peng_isl <- peng[, -1]
peng_isl$island <- factor(peng_isl$island, levels = c("Biscoe", "Dream", "Torgersen"), labels = c( 0, 1, 2))
peng_isl_index <- createDataPartition(peng_isl$island, p = 0.7)
peng_isl_train <- peng_isl[peng_isl_index$Resample1,]
peng_isl_test <- peng_isl[-peng_isl_index$Resample1,]
peng_isl_lm <- glm(island ~ ., data = peng_isl_train, family = "binomial")
peng_isl_step <- step(peng_isl_lm, direction = "both")
summary(peng_isl_step)

# 获取性别特征
peng_sex <- peng[, c(-1, -2)]
plot_sex <- gather(peng_sex, key = "variable", value = "value", c(-sex))
ggplot(plot_sex, aes(fill = sex)) +
  theme_bw() +
  geom_density(aes(value), alpha = 0.5) +
  facet_wrap(~variable, scales = "free")

# Logistic回归性别特征
peng_sex <- peng[, c(-1, -2)]
peng_sex$sex <- factor(peng_sex$sex, levels = c("MALE", "FEMALE"), labels = c( 0, 1))
peng_sex_index <- createDataPartition(peng_sex$sex, p = 0.7)
peng_sex_train <- peng_sex[peng_sex_index$Resample1,]
peng_sex_test <- peng_sex[-peng_sex_index$Resample1,]
peng_sex_lm <- glm(sex ~ ., data = peng_sex_train, family = "binomial")
peng_sex_step <- step(peng_sex_lm, direction = "both")
summary(peng_sex_step)