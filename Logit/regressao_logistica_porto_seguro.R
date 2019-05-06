dados.treino <- read.csv("~/Desktop/TCC/Porto Seguro/porto_seguro_limpo_treino.csv")

dados.teste <- read.csv("~/Desktop/TCC/Porto Seguro/porto_seguro_limpo_teste.csv")

# PACOTES NECESSÁRIOS
library(gmodels)
library(boot)
library(hmeasure)
library(pROC)

# RODAR REG LOG
fit1=glm(data = dados.treino, target~ps_ind_01+ps_ind_03+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+ps_ind_14+ps_ind_15+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+ps_reg_01+ps_reg_02+ps_car_08_cat+ps_car_11+ps_car_15+ps_calc_01+ps_calc_02+ps_calc_03+ps_calc_04+ps_calc_05+ps_calc_06+ps_calc_07+ps_calc_08+ps_calc_09+ps_calc_10+ps_calc_11+ps_calc_12+ps_calc_13+ps_calc_14+ps_calc_15_bin+ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+ps_calc_20_bin+ps_car_07_cat_null+ps_car_07_cat_1+ps_car_05_cat_null+ps_car_05_cat_1+ps_car_03_cat_null+ps_car_03_cat_1+ps_car_02_cat_1+ps_ind_04_cat_1+ps_ind_02_cat_null+ps_ind_02_cat_2_3_4+ps_ind_05_cat_0+ps_ind_05_cat_1_3_4_5_6+ps_ind_05_cat_2+ps_car_01_cat_6_7+ps_car_01_cat_3_4_5_10+ps_car_01_cat_0_1_2_8_11+ps_car_01_cat_9+ps_car_04_cat_0_4+ps_car_04_cat_1_2+ps_car_04_cat_3_8+ps_car_04_cat_6_9+ps_car_06_cat_0_1_3_4_6_7_11_14+ps_car_06_cat_10_12_15_16+ps_car_06_cat_2_5_8_17+ps_car_09_cat_0_2_3+ps_car_09_cat_1_4+ps_car_11_cat_A+ps_car_11_cat_B+ps_car_11_cat_C+ps_reg_03_no_out+ps_car_12_no_out+ps_car_13_no_out+ps_car_14_no_out, family = binomial())
summary(fit1)

# STEPWISE
fit2=step(fit1)
summary(fit2)

# PROB
dados.teste$pred=predict(fit2, newdata = dados.teste, type = "response")
head(dados.teste$pred, 5)

# CROSS VALIDATION
cv.glm(data=dados.teste, glmfit=fit2, K=10)$delta[1]

# ROC E AUC
medidah=HMeasure(dados.teste$target, dados.teste$pred) 
summary(medidah)

# CURVA ROC
roc1 = roc(dados.teste$target, dados.teste$pred) 
plot(roc1, lwd=4, col="red")
roc1

# p-value 50%
fit3=step(fit1, k=0.45)
summary(fit3)

dados.teste$predfit3=predict(fit3, newdata = dados.teste, type = "response")
head(dados.teste$predfit3, 5)

roc2 = roc(dados.teste$target, dados.teste$predfit3) 
plot(roc2, lwd=4, col="red")
roc2
