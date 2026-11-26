#a propria IA mentora que aconcelhou escrevermos para separar oque cada parte do codigo faz!
#pacotes necessarios 
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(modelsummary)

base <- read_excel("vigitel_2023.xlsx", guess_max = 25000)

nrow(base)   

#tiramos colunas que não faziam sentido para a nossa analize

base_limpa <- select(base, -c(ano, q8a:q9, q11, q12, q13, r128a, r190, q15, q16, q17:q20, q25, q26, q27:q31, r301_a:r301_l,r302_a:r302_m,q35, q36, q37,
                        q38, q40, q40b, q52, q53, q54, q55, q56, r149, r150_hh, r150_mm, q59b, q61, q61_fx, q64, q67, q68,r157, r401:r402,
                        r404, r403, q74, q75, r203, r129, q76, r204, r133a, r133b, r205, r206, r207, q79a, q80:q82, r501:r506_ou, q69_ou,
                        q70, q71, moradores, adultos, fet, cat_esc, fxesc, q9_i, q11_i, mais20, fumocasa, fumocasa2024, fumotrab, fumotrab2024,
                        eletronico, eletronicoexp, fumantetotal, hortareg, frutareg, flvreg, cruadia, cozidadia, hortadia, sucodia, sofrutadia,
                        frutadia, flvdia, flvreco, score_sf_2cat, score_upp_2cat, inativo, inativo_2023, q51medio, q51medio_2023, q54medio, q54medio_2023,
                        deslocdia, deslocdia_2023, deslocsemana, deslocsemana_2023, atiocusemana, faxinasemana, af3dominios, af3dominios_2023,
                        af3dominios_insu, af3dominios_insu_2023, tv_d_3, tempo_tela_stv, tempo_tela_total, saruim, iddmamo:papatres, ind_med_has, 
                        med_has, trat_med_has, ind_med_db, med_db, trat_med_db, insulina, depressao, ind_med_depr, trat_med_depr,
                        q43a_ou, r404_ou, r505_1, r505_2, r505_3, r505_4, r505_5, r505_90, frutareg2024, flvdia2024, exeletronico, q60, fesc, q59a, q59c,
                        q47, q48, q49, r147, r148_hh, r148_mm, q50, q51, q42, q43a, q44, q45, q46, af, freq, time, time_2023,ati_livre_2023, ativo_livre,
                        imc_i, excpeso, excpeso_i, obesid, obesid_i))

summary(base_limpa)

#dados como 777/888 que significavam nao sei ou nao respondeu ficam como na para serem retirados da base

base_final <- mutate(base_limpa, civil = replace(civil, civil %in% c(777, 888), NA),
                                 q14   = replace(q14,   q14   %in% c(777, 888), NA),
                                 q69   = replace(q69,   q69   %in% c(777, 888, 80), NA),
                                 q88   = replace(q88,   q88   %in% c(777, 888), NA))

nrow(base_final)

#tiramos da base os na

base_final <- filter(base_final, is.na(q14) | q14 != 1)
nrow(base_final)
sem_imc <- filter(base_final, is.na(imc))
com_imc <- filter(base_final, !is.na(imc))
mean(sem_imc$q6);                     mean(com_imc$q6)
mean(sem_imc$q8_anos, na.rm = TRUE);  mean(com_imc$q8_anos, na.rm = TRUE)
mean(sem_imc$q7 == 2);                mean(com_imc$q7 == 2)
base_final <- filter(base_final, !is.na(imc))
nrow(base_final)
base_final <- filter(base_final, !is.na(q69), !is.na(civil), !is.na(q88))
summary(base_final)

nrow(base_final)

#deixamos a base com as variaveis que vamos usar na hora de fazer o MQO

base_modelo <- mutate(base_final, horas_ex_seman = ati_livre / 60,
                      horas_ex_seman2 = horas_ex_seman ^2,
                      faz_ex = ifelse(ati_livre > 0, 1, 0),
                      idade = q6,
                      idade2 = q6^2,
                      homem = ifelse(q7 == 1, 1, 0),
                      anos_estudo  = q8_anos,
                      tem_plano = ifelse(q88 %in% c(1, 2), 1, 0),
                      preta = ifelse(q69 == 2, 1, 0),
                      parda = ifelse(q69 == 4, 1, 0),
                      outras_cores = ifelse(q69 %in% c(3, 5), 1, 0),
                      casado = ifelse(civil == 2, 1, 0),
                      uniao_estavel = ifelse(civil == 3, 1, 0),
                      viuvo = ifelse(civil == 4, 1, 0),
                      separado = ifelse(civil == 5, 1, 0),
                      q59a_horas = replace(q59a_horas, q59a_horas == 1, 0.5),
                      q59c_horas = replace(q59c_horas, q59c_horas == 1, 0.5),
                      tela_total = q59a_horas + q59c_horas,
                      cidade_f = factor(cidade))

summary(base_modelo)

#verificando se nossa base se parece com os dados oficiais

base_imc <- filter(base, !is.na(imc))

100 * mean(base_imc$imc >= 30)                                       # porcentagem de obesidade, sem peso
100 * weighted.mean(base_imc$imc >= 30, w = base_imc$pesorake2025)   # porcentagem de obesidade, com peso
100 * mean(base_imc$imc >= 25)                                       # porcentagem de excesso de peso, sem peso
100 * weighted.mean(base_imc$imc >= 25, w = base_imc$pesorake2025)   # porcentagem deexcesso de peso, com peso

base_imc_i <- filter(base, !is.na(imc_i))
100 * weighted.mean(base_imc_i$imc_i >= 30, w = base_imc_i$pesorake2025)   # obesidade, com peso e imputacao
100 * weighted.mean(base_imc_i$imc_i >= 25, w = base_imc_i$pesorake2025)   # excesso de peso, com peso e imputacao

mean(base_modelo$idade);  weighted.mean(base_modelo$idade, w = base_modelo$pesorake2025)
mean(base_modelo$homem);  weighted.mean(base_modelo$homem, w = base_modelo$pesorake2025)

#verificando a distribuição do IMC nos nossos dados

mean(base_modelo$imc)
sd(base_modelo$imc)
summary(base_modelo$imc)

png("histograma_imc.png", width = 800, height = 600)

hist(base_modelo$imc,
     breaks = 50,
     main   = "Distribuição do IMC",
     xlab   = "IMC",
     ylab   = "Frequência")
abline(v = 25, col = "red", lty = 2)    # excesso de peso
abline(v = 30, col = "red", lty = 2)    # obesidade

dev.off()

#verificando como se comnpratam os dados de hotas de exercicio ma nossa base

mean(base_modelo$horas_ex_seman == 0)
length(unique(base_modelo$horas_ex_seman))

fazem_exercicio <- base_modelo$horas_ex_seman[base_modelo$horas_ex_seman > 0]
summary(fazem_exercicio)
quantile(fazem_exercicio, probs = c(0.90, 0.95, 0.99))
hist(fazem_exercicio, breaks = 30, main = "Horas de exercício", xlab = "Horas por semana")

#diferença entre grupos que praticam exercicio nas variaveis gerais

grupos <- group_by(base_modelo, faz_ex)

# tabela geral: media de cada variavél qie interessa nos dois grupos
tabela <- summarise(grupos,
                    n = n(),
                    across(c(imc, idade, homem, anos_estudo, preta, parda, outras_cores,
                             casado, uniao_estavel, viuvo, separado, score_upp, refritl5,
                             tela_total, fumante, alcabu, atitrans, atiocu, atidom, tem_plano),
                           ~ mean(.x, na.rm = TRUE)))
t(tabela)

# Curiosidade para ver a associacao na alimentação
alimentacao <- summarise(grupos,
                         n = n(),
                         ultraprocessados_media = mean(score_upp),
                         ultraprocessados_5mais = mean(score_upp >= 5),
                         refrigerante_5dias     = mean(refritl5),
                         in_natura_media        = mean(score_sf))
t(alimentacao)

# testes de diferenca de medias
t.test(imc ~ faz_ex, data = base_modelo)
t.test(idade ~ faz_ex, data = base_modelo)
t.test(anos_estudo ~ faz_ex, data = base_modelo)
t.test(score_upp ~ faz_ex, data = base_modelo)
t.test(refritl5 ~ faz_ex, data = base_modelo)
t.test(score_sf ~ faz_ex, data = base_modelo)

#IMC de quem faz e de quem nao faz exercicio
boxplot(imc ~ faz_ex, data = base_modelo,
        names = c("Não faz exercício", "Faz exercício"),
        main = "IMC por grupo", ylab = "IMC")

#IMC medio para cada valor de horas, e quantas pessoas ha em cada valor
medias   <- aggregate(imc ~ horas_ex_seman, data = base_modelo, FUN = mean)
contagem <- aggregate(imc ~ horas_ex_seman, data = base_modelo, FUN = length)
medias$n <- contagem$imc
medias

plot(medias$horas_ex_seman, medias$imc, type = "b",
     xlab = "Horas de exercício por semana", ylab = "IMC médio")

#so os valores de horas com pelo menos 100 pessoas
medias_100 <- medias[medias$n >= 100, ]
plot(medias_100$horas_ex_seman, medias_100$imc, type = "b",
     xlab = "Horas de exercício por semana", ylab = "IMC médio")

#IMC medio por anos de estudo, separado por quem faz e quem nao faz exercicio
m <- aggregate(imc ~ anos_estudo + faz_ex, data = base_modelo, FUN = mean)

plot(m$anos_estudo[m$faz_ex == 0], m$imc[m$faz_ex == 0], type = "b",
     col = "red", ylim = range(m$imc),
     xlab = "Anos de estudo", ylab = "IMC médio")
lines(m$anos_estudo[m$faz_ex == 1], m$imc[m$faz_ex == 1],
      type = "b", col = "blue")
legend("topright", legend = c("Não faz exercício", "Faz exercício"),
       col = c("red", "blue"), lty = 1)

#teste para ver se idade entra como linear ou nao
idade_imc <- aggregate(imc ~ idade, data = base_modelo, FUN = mean)
plot(idade_imc$idade, idade_imc$imc, type = "b",
     xlab = "Idade", ylab = "IMC médio")

#modelos

#modelo 1 com a variavél de interesse e os controles
m1 <- lm(imc ~ horas_ex_seman + horas_ex_seman2 + idade + idade2 + homem + anos_estudo + preta + parda + outras_cores +
              casado + uniao_estavel + viuvo + separado + tem_plano + cidade_f + atitrans + atiocu + atidom, data = base_modelo)

#modelo 2 = modelo 1 mais as variaveis de hábitos
m2 <- update(m1, . ~ . + score_upp + refritl5 + tela_total + fumante + alcabu)

#modelo 3 = modelo 1 mais a dummy de exercicio
m3 <- update(m1, . ~ . + faz_ex)

#modelo 4 = modelo 3 mais as variaveis de hábitos
m4 <- update(m3, . ~ . + score_upp + refritl5 + tela_total + fumante + alcabu)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

coef(m1)["horas_ex_seman"]
coef(m1)["horas_ex_seman2"]
coef(m2)["horas_ex_seman"]
coef(m2)["horas_ex_seman2"]
coef(m3)["horas_ex_seman"]
coef(m3)["horas_ex_seman2"]
coef(m4)["horas_ex_seman"]
coef(m4)["horas_ex_seman2"]

nobs(m1)
nobs(m2)
nobs(m3)
nobs(m4)

#vendo sobre heterocedasticidade

plot(fitted(m2), resid(m2), xlab = "Valores ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")

#testes de heterocedasticidade nos quatro modelos
bptest(m1); bptest(m2); bptest(m3); bptest(m4)
bptest(m1, ~ fitted(m1) + I(fitted(m1)^2))
bptest(m2, ~ fitted(m2) + I(fitted(m2)^2))
bptest(m3, ~ fitted(m3) + I(fitted(m3)^2))
bptest(m4, ~ fitted(m4) + I(fitted(m4)^2))

#erros-padrao robustos, so as linhas do exercicio
rob1 <- coeftest(m1, vcov. = vcovHC(m1, type = "HC1"))
rob2 <- coeftest(m2, vcov. = vcovHC(m2, type = "HC1"))
rob3 <- coeftest(m3, vcov. = vcovHC(m3, type = "HC1"))
rob4 <- coeftest(m4, vcov. = vcovHC(m4, type = "HC1"))

rob1[c("horas_ex_seman", "horas_ex_seman2"), ]
rob2[c("horas_ex_seman", "horas_ex_seman2"), ]
rob3[c("horas_ex_seman", "horas_ex_seman2", "faz_ex"), ]
rob4[c("horas_ex_seman", "horas_ex_seman2", "faz_ex"), ]

#comparar com os erros-padrao usuais
summary(m2)$coefficients[c("horas_ex_seman", "horas_ex_seman2"), ]
summary(m4)$coefficients[c("horas_ex_seman", "horas_ex_seman2", "faz_ex"), ]

#testando agora para ver se é sigificativo o exercicio em cada modelo
linearHypothesis(m1, c("horas_ex_seman = 0", "horas_ex_seman2 = 0"), vcov. = vcovHC(m1, type = "HC1"))
linearHypothesis(m2, c("horas_ex_seman = 0", "horas_ex_seman2 = 0"), vcov. = vcovHC(m2, type = "HC1"))
linearHypothesis(m3, c("horas_ex_seman = 0", "horas_ex_seman2 = 0"), vcov. = vcovHC(m3, type = "HC1"))
linearHypothesis(m4, c("horas_ex_seman = 0", "horas_ex_seman2 = 0"), vcov. = vcovHC(m4, type = "HC1"))

#testanto para os exercicios e a dummy de fazer exercicio
linearHypothesis(m3, c("horas_ex_seman = 0", "horas_ex_seman2 = 0", "faz_ex = 0"), vcov. = vcovHC(m3, type = "HC1"))
linearHypothesis(m4, c("horas_ex_seman = 0", "horas_ex_seman2 = 0", "faz_ex = 0"), vcov. = vcovHC(m4, type = "HC1"))

#largura dos residuos ao longo dos valores ajustados
faixa_ajustado <- cut(fitted(m2), quantile(fitted(m2), probs = seq(0, 1, 0.2)), include.lowest = TRUE)
tapply(resid(m2), faixa_ajustado, sd)

V1 <- vcovHC(m1, type = "HC1")
V2 <- vcovHC(m2, type = "HC1")
V3 <- vcovHC(m3, type = "HC1")
V4 <- vcovHC(m4, type = "HC1")

#efeito marginal de 1 hora a mais em 0 h, 3,5 h (mediana) e 7 h (percentil 90)
deltaMethod(m2, "horas_ex_seman", vcov. = V2)
deltaMethod(m2, "horas_ex_seman + 2*horas_ex_seman2*3.5", vcov. = V2)
deltaMethod(m2, "horas_ex_seman + 2*horas_ex_seman2*7", vcov. = V2)

#vertice da parabola e quantas pessoas estao acima dele
vert <- deltaMethod(m2, "-horas_ex_seman / (2*horas_ex_seman2)", vcov. = V2)
mean(base_modelo$horas_ex_seman > vert$Estimate)

#diferenca de IMC entre 150 minutos (2,5 h) e zero
deltaMethod(m1, "2.5*horas_ex_seman + 2.5^2*horas_ex_seman2", vcov. = V1)
deltaMethod(m2, "2.5*horas_ex_seman + 2.5^2*horas_ex_seman2", vcov. = V2)

#modelos com dummy associacao total de 3,5 h em relacao a quem nao se exercita
deltaMethod(m3, "faz_ex + 3.5*horas_ex_seman + 3.5^2*horas_ex_seman2", vcov. = V3)
deltaMethod(m4, "faz_ex + 3.5*horas_ex_seman + 3.5^2*horas_ex_seman2", vcov. = V4)

#relacao entre a dummy e as horas
cor(base_modelo$faz_ex, base_modelo$horas_ex_seman)

#diferenca para 2,5 h nos modelos com dummy, para comparar com o modelo 2
deltaMethod(m3, "faz_ex + 2.5*horas_ex_seman + 2.5^2*horas_ex_seman2", vcov. = V3)
deltaMethod(m4, "faz_ex + 2.5*horas_ex_seman + 2.5^2*horas_ex_seman2", vcov. = V4)

grade <- seq(0, 14, by = 0.1)

#pontos + parabola sem controles

#quantas pessoas ficam fora do grafico por terem IMC acima de 50 (vai na nota da figura)
sum(base_modelo$imc > 50)

#salvando a figura A para o relatorio
set.seed(2023)
png("figura_A.png", width = 800, height = 600)

plot(jitter(base_modelo$horas_ex_seman), base_modelo$imc,
     pch = 16, cex = 0.5, col = rgb(0, 0, 0, 0.1),
     ylim = c(10, 50),
     xlab = "Horas de atividade física por semana", ylab = "IMC")
simples <- lm(imc ~ horas_ex_seman + horas_ex_seman2, data = base_modelo)
lines(grade, predict(simples, newdata = data.frame(horas_ex_seman = grade, horas_ex_seman2 = grade^2)),
      col = "red", lwd = 2)

dev.off()

#modelos adicionais

#sem valores extremos (quem tem 60+ de IMC)
sum(base_modelo$imc > 60)
m2_sem_extremos <- update(m2, subset = (imc <= 60))
deltaMethod(m2_sem_extremos, "2.5*horas_ex_seman + 2.5^2*horas_ex_seman2",
            vcov. = vcovHC(m2_sem_extremos, type = "HC1"))

#sem o plano de saude
m2_sem_plano <- update(m2, . ~ . - tem_plano)
deltaMethod(m2_sem_plano, "2.5*horas_ex_seman + 2.5^2*horas_ex_seman2",
            vcov. = vcovHC(m2_sem_plano, type = "HC1"))

#tabelas de resultados

#tabela 1: os quatro modelos, so as linhas do exercicio
modelos <- list("(1)" = m1, "(2)" = m2, "(3)" = m3, "(4)" = m4)

#linhas escritas a mao: controles, habitos, efeitos fixos, observacoes e r2 ajustado
rodape <- data.frame(
  term  = c("Controles", "Hábitos", "Efeitos fixos de cidade", "Observações", "R² ajustado"),
  "(1)" = c("Sim", "Não", "Sim", nobs(m1), formatC(summary(m1)$adj.r.squared, format = "f", digits = 3)),
  "(2)" = c("Sim", "Sim", "Sim", nobs(m2), formatC(summary(m2)$adj.r.squared, format = "f", digits = 3)),
  "(3)" = c("Sim", "Não", "Sim", nobs(m3), formatC(summary(m3)$adj.r.squared, format = "f", digits = 3)),
  "(4)" = c("Sim", "Sim", "Sim", nobs(m4), formatC(summary(m4)$adj.r.squared, format = "f", digits = 3)),
  check.names = FALSE)

options(modelsummary_format_numeric_latex = "plain")

modelsummary(modelos,
             vcov     = "HC1",
             coef_map = c("horas_ex_seman"  = "Horas por semana (H)",
                          "horas_ex_seman2" = "Horas por semana ao quadrado (H²)",
                          "faz_ex"          = "Pratica atividade física (P)"),
             gof_map  = NA,
             add_rows = rodape,
             stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             title    = "Atividade física no lazer e IMC: modelos 1 a 4",
             notes    = c("Variável dependente: IMC. Estimação por MQO.",
                          "Erros-padrão robustos (HC1) entre parênteses.",
                          "Controles: idade, idade ao quadrado, sexo, anos de estudo,",
                          "cor, estado civil, plano de saúde e atividade física no",
                          "deslocamento, no trabalho e em casa.",
                          "Hábitos: ultraprocessados, refrigerante, tempo de tela,",
                          "fumo e consumo abusivo de álcool."),
             output   = "tabela1.tex")

#tabela 2: robustez do modelo 2
modelos2 <- list("Principal"       = m2,
                 "Sem plano"       = m2_sem_plano,
                 "Sem IMC > 60"    = m2_sem_extremos)

#diferenca de IMC entre 2,5 h e zero em todos os modelos de uma vez
dif <- sapply(modelos2, function(m) {
  r <- deltaMethod(m, "2.5*horas_ex_seman + 2.5^2*horas_ex_seman2", vcov. = vcovHC(m, type = "HC1"))
  c(r$Estimate, r$SE)
})

linha_dif <- data.frame(
  term = c("Diferença para 2,5 h", ""),
  rbind(formatC(dif[1, ], format = "f", digits = 3),
        paste0("(", formatC(dif[2, ], format = "f", digits = 3), ")")),
  check.names = FALSE)

rodape2 <- data.frame(
  term              = c("Observações", "R² ajustado"),
  "Principal"       = c(nobs(m2),              formatC(summary(m2)$adj.r.squared,              format = "f", digits = 3)),
  "Sem plano"       = c(nobs(m2_sem_plano),    formatC(summary(m2_sem_plano)$adj.r.squared,    format = "f", digits = 3)),
  "Sem IMC > 60"    = c(nobs(m2_sem_extremos), formatC(summary(m2_sem_extremos)$adj.r.squared, format = "f", digits = 3)),
  check.names = FALSE)

modelsummary(modelos2,
             vcov     = "HC1",
             coef_map = c("horas_ex_seman"  = "Horas por semana (H)",
                          "horas_ex_seman2" = "Horas por semana ao quadrado (H²)"),
             gof_map  = NA,
             add_rows = rbind(linha_dif, rodape2),
             stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             title    = "Robustez do modelo 2",
             notes    = c("Variável dependente: IMC. Estimação por MQO.",
                          "Erros-padrão robustos (HC1) entre parênteses.",
                          "Todas as colunas têm os controles, os hábitos e os efeitos fixos",
                          "de cidade do modelo 2. Sem plano: sem a dummy de plano de saúde.",
                          "Sem IMC > 60: retira as 20 pessoas com IMC acima de 60.",
                          "A linha da diferença não leva estrelas; a significância",
                          "é lida pelo erro-padrão."),
             output   = "tabela2.tex")

sessionInfo()
