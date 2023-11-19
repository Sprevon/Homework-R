# first Principle analysis
communityData <- read.csv("D:\\Program\\DataSpell\\datafile\\R-Class\\quiz\\midterm\\data\\LA.csv")
firstPrinciple <- princomp(communityData[, -1], cor = TRUE)
summary(firstPrinciple)
screeplot(firstPrinciple,type="barplot")

print("---------------------------------------")

# factor analysis
factorAnalysis <- factanal(communityData[, -1], factors = 6, scores = "Bartlett")
factorAnalysis$loadings

# 得到：
# Importance of components:
#                          Comp.1    Comp.2    Comp.3     Comp.4    Comp.5     Comp.6     Comp.7     Comp.8
# Standard deviation     2.276290 1.4526977 1.3046265 1.06283297 0.9180742 0.90433923 0.79680013 0.65673588
# Proportion of Variance 0.370107 0.1507379 0.1215750 0.08068671 0.0602043 0.05841639 0.04534932 0.03080729
# Cumulative Proportion  0.370107 0.5208449 0.6424199 0.72310664 0.7833109 0.84172733 0.88707664 0.91788393
#                            Comp.9    Comp.10    Comp.11    Comp.12     Comp.13      Comp.14
# Standard deviation     0.60545708 0.55773624 0.47952347 0.40015285 0.285223173 2.365740e-02
# Proportion of Variance 0.02618416 0.02221927 0.01642448 0.01143731 0.005810876 3.997663e-05
# Cumulative Proportion  0.94406809 0.96628736 0.98271184 0.99414915 0.999960023 1.000000e+00
# [1] "---------------------------------------"
#
# Loadings:
#            Factor1 Factor2 Factor3 Factor4 Factor5 Factor6
# Income      0.581   0.560           0.281  -0.213
# Schools                     0.361
# Diversity                   0.106           0.956   0.260
# Age         0.731   0.574           0.127           0.142
# Homes       0.255   0.658
# Vets        0.479   0.745          -0.179          -0.161
# Asian                               0.209   0.292   0.931
# Black      -0.126          -0.290  -0.918          -0.229
# Latino     -0.951  -0.258           0.127
# White       0.860   0.238   0.130   0.414
# Population -0.195  -0.180   0.619
# Area        0.145   0.418   0.657   0.121  -0.164  -0.111
# Longitude  -0.441  -0.357  -0.478          -0.102   0.114
# Latitude            0.285   0.375   0.246
#
#                Factor1 Factor2 Factor3 Factor4 Factor5 Factor6
# SS loadings      3.089   2.174   1.447   1.294   1.098   1.093
# Proportion Var   0.221   0.155   0.103   0.092   0.078   0.078
# Cumulative Var   0.221   0.376   0.479   0.572   0.650   0.728
#
# 可见，从分析的角度看，面对存在多个变量的数据 principle components analysis 不方便进行比较，因为其主要元素需要手动设置。
# 而 factor analysis 则可较为方便，因为其可以自动生成因素