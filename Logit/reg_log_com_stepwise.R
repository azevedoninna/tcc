dados.treino <- read.csv("~/Desktop/TCC/Porto Seguro/porto_seguro_stepwise_reagrupado_treino.csv")

dados.teste <- read.csv("~/Desktop/TCC/Porto Seguro/porto_seguro_stepwise_reagrupado_teste.csv")

dados <- rbind(dados.treino, dados.teste)
  
# PACOTES NECESSÁRIOS
library(gmodels)
library(boot)
library(hmeasure)
library(pROC)

# RODAR REG LOG
fit=glm(data = dados.treino, target~ps_ind_01+ps_ind_03+ps_ind_07_bin+ps_ind_08_bin+ps_ind_15+ps_ind_16_bin+ps_ind_17_bin+ps_reg_01+ps_reg_02+ps_car_11+ps_car_15+ps_calc_01+ps_calc_02+ps_calc_03+ps_car_01_cat_6_7+ps_car_01_cat_3_4_5_10+ps_car_01_cat_0_1_2_8_11+ps_car_01_cat_9+ps_car_07_cat_null+ps_car_07_cat_1+ps_car_11_cat_A+ps_car_11_cat_B+ps_car_11_cat_C+ps_car_03_cat_1+ps_ind_04_cat_1+ps_ind_02_cat_1+ps_ind_05_cat_0+ps_ind_05_cat_1_3_4_5_6+ps_car_04_cat_0_1_2_4+ps_car_04_cat_3_8+ps_car_09_cat_0_2_3+ps_reg_03_no_out+ps_car_12_no_out+ps_car_13_no_out+ps_car_14_no_out, family = binomial())
fit.dados=glm(data = dados, target~ps_ind_01+ps_ind_03+ps_ind_07_bin+ps_ind_08_bin+ps_ind_15+ps_ind_16_bin+ps_ind_17_bin+ps_reg_01+ps_reg_02+ps_car_11+ps_car_15+ps_calc_01+ps_calc_02+ps_calc_03+ps_car_01_cat_6_7+ps_car_01_cat_3_4_5_10+ps_car_01_cat_0_1_2_8_11+ps_car_01_cat_9+ps_car_07_cat_null+ps_car_07_cat_1+ps_car_11_cat_A+ps_car_11_cat_B+ps_car_11_cat_C+ps_car_03_cat_1+ps_ind_04_cat_1+ps_ind_02_cat_1+ps_ind_05_cat_0+ps_ind_05_cat_1_3_4_5_6+ps_car_04_cat_0_1_2_4+ps_car_04_cat_3_8+ps_car_09_cat_0_2_3+ps_reg_03_no_out+ps_car_12_no_out+ps_car_13_no_out+ps_car_14_no_out, family = binomial())
summary(fit)

# PROB
dados.teste$pred=predict(fit, newdata = dados.teste, type = "response")
head(dados.teste$pred, 5)

# CROSS VALIDATION
cross.validation.treino = 1 - cv.glm(data=dados.treino, glmfit=fit, K=10)$delta[1]
cross.validation.dados = 1 - cv.glm(data=dados, glmfit=fit.dados, K=10)$delta[1]
cross.validation.treino
cross.validation.dados

# ROC E AUC
medidah=HMeasure(dados.teste$target, dados.teste$pred) 
medidah$metrics

# CURVA ROC
roc = roc(dados.teste$target, dados.teste$pred) 
plot(roc, lwd=4, col="red")
roc
