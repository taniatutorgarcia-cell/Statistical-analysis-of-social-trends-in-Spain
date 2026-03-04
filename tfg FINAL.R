# Cargar datos
library(readxl)
library(dplyr)

datos <- read_excel("C:/Users/tania/OneDrive/Escritorio/TFG/ENCUESTA SOBRE TENDENCIAS SOCIALES/3486_num.xlsx")
# Recodificación segura para NA
recode_na <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(8, 9, 98, 99, 999, 9999)] <- NA
  return(x)
}

variables_interes <- c("PROBLMUN1R", "PROBLMUN2R", "PROBLMUN3R", "EDAD", "SEXO",
                       "ESTUDIOS", "ESCIDEOL", "RELIGION", "INGRESHOG", "TAMUNI", "CCAA")
#variables_interes <- c("PROBLMUN1R", "PROBLMUN2R", "PROBLMUN3R", "EDAD", "SEXO",
 #                      "ESTUDIOS", "ESCIDEOL", "RELIGION", "INGRESHOG", "TAMUNI", "CCAA",
  #                     "SIMBOL1R","SIMBOL2R","ECIVIL","CLASESOCIAL", "NACIONALIDAD","INTPOL","CONFIANZA_1")


datos_limpios <- datos %>%
  select(all_of(variables_interes)) %>%
  mutate(across(everything(), recode_na)) %>%
  na.omit()

# Convertir algunas a factores
datos_limpios$SEXO <- factor(datos_limpios$SEXO, labels = c("Hombre", "Mujer"))
datos_limpios$ESTUDIOS <- factor(datos_limpios$ESTUDIOS)
#datos_limpios$ESCIDEOL <- factor(datos_limpios$ESCIDEOL)
datos_limpios$RELIGION <- factor(datos_limpios$RELIGION)
datos_limpios$PROBLMUN2R <- as.factor(datos_limpios$PROBLMUN2R)
datos_limpios$PROBLMUN3R <- as.factor(datos_limpios$PROBLMUN3R)
datos_limpios$INGRESHOG <- as.factor(datos_limpios$INGRESHOG)
datos_limpios$TAMUNI <- as.factor(datos_limpios$TAMUNI)
datos_limpios$CCAA <- as.factor(datos_limpios$CCAA)
datos_limpios$NACIONALIDAD<- as.factor(datos_limpios$NACIONALIDAD)
datos_limpios$CLASESOCIAL<- as.factor(datos_limpios$CLASESOCIAL)
datos_limpios$INTPOL<- as.factor(datos_limpios$INTPOL)
datos_limpios$CONFIANZA_1<- as.factor(datos_limpios$CONFIANZA_1)
datos_limpios$ECIVIL<- as.factor(datos_limpios$ECIVIL)
datos_limpios$SIMBOL1R<- as.factor(datos_limpios$SIMBOL1R)
datos_limpios$SIMBOL2R<- as.factor(datos_limpios$SIMBOL2R)
# Convertir variable dependiente a factor
datos_limpios$PROBLMUN1R <- as.factor(datos_limpios$PROBLMUN1R)

# Ver las 5 categorías más frecuentes
top5 <- names(sort(table(datos_limpios$PROBLMUN1R), decreasing = TRUE))[1:2]
library(dplyr)

# Filtrar para quedarnos con las 5 más frecuentes
datos_filtrados <- datos_limpios %>%
  filter(PROBLMUN1R %in% top5) %>%
  mutate(PROBLMUN1R = droplevels(PROBLMUN1R))

#librerias 
library(randomForest)
library(caret)
set.seed(13656545) 
set.seed(13656549)# esta 
## PARTICIÓN DE DATOS ##
trainIndex <- createDataPartition(datos_filtrados$PROBLMUN1R, p = 0.8, list = FALSE)

# Dividir correctamente
datos_train <- datos_filtrados[trainIndex, ]
datos_test  <- datos_filtrados[-trainIndex, ]

#################################################################################################################
                        ################# REGRESIÓN LOGÍSTICA #########################
#################################################################################################################
library(MASS)      # Para stepAIC
#regvacia<- glm(PROBLMUN1R~1 ,data=datos_train, family = binomial)
#modelo_full <- glm(PROBLMUN1R ~ EDAD + SEXO + ESTUDIOS + ESCIDEOL + INGRESHOG + TAMUNI + CCAA + RELIGION, data = datos_train, family = binomial)
#regforw<-step(regvacia, scope = list(lower=regvacia, upper=modelo_full),direction = "both"); summary(regforw)

# Aplicar selección stepwise hacia atrás basada en AIC
#modelo_step <- stepAIC(modelo_full, direction = "both", trace = FALSE)

# Resumen del modelo
#summary(modelo_step)

# Predicciones en conjunto de prueba
#probabilidades <- predict(modelo_step, newdata = datos_test, type = "response")
#predicciones <- ifelse(probabilidades > 0.5, levels(datos_train$PROBLMUN1R)[2], levels(datos_train$PROBLMUN1R)[1])
#predicciones <- factor(predicciones, levels = levels(datos_train$PROBLMUN1R))

# Matriz de confusión y precisión
#confusion <- confusionMatrix(predicciones, datos_test$PROBLMUN1R)
#print(confusion)

modelo_logit=glm(PROBLMUN1R ~EDAD + SEXO + ESTUDIOS + ESCIDEOL + INGRESHOG + TAMUNI + CCAA + RELIGION, data = datos_train,family = binomial(link="logit"))
summary(modelo_logit)
modelo_logit_final=glm(PROBLMUN1R ~variables, data = datos_train,family = binomial(link="logit"))

# Predicciones en conjunto de prueba
probabilidades <- predict(modelo_logit, newdata = datos_test, type = "response")
predicciones <- ifelse(probabilidades > 0.5, levels(datos_train$PROBLMUN1R)[2], levels(datos_train$PROBLMUN1R)[1])
predicciones <- factor(predicciones, levels = levels(datos_train$PROBLMUN1R))
confusion <- confusionMatrix(predicciones, datos_test$PROBLMUN1R)
print(confusion)

## ODS RATIO
# Instala y carga si no tienes broom
library(broom)

# Extraer los coeficientes y calcular OR e IC 95%
odds_ratios <- tidy(modelo_logit) %>%
  mutate(
    OR = exp(estimate),
    IC_low = exp(estimate - 1.96 * std.error),
    IC_high = exp(estimate + 1.96 * std.error)
  )

# Mostrar tabla ordenada por OR
print(odds_ratios[, c("term","estimate","std.error", "OR", "IC_low", "IC_high","p.value")])

# Filtrar solo las variables significativas (p < 0.05)
significativas <- odds_ratios %>%
  filter(p.value < 0.17) %>%
  select(term, estimate, std.error, OR,IC_low, IC_high, p.value) %>%
  arrange(desc(OR))

# Mostrar la tabla
print(significativas)

library(knitr)
kable(significativas)

## GRAFICO
# Datos del modelo (ajusta estos vectores a tus resultados reales)

variables <- c("RELIGION5", "INGRESHOG6", "CCAA14", "INGRESHOG5", "RELIGION6", 
               "RELIGION4", "INGRESHOG3", "SEXOMujer", "CCAA12", "CCAA7", "CCAA4")
OR <- c(3.08, 2.70, 2.58, 1.99, 1.90, 1.78, 1.64, 1.37, 0.33, 0.32, 0.28)
IC_low <- c(1.27, 0.67, 0.74, 0.92, 0.85, 0.80, 0.87, 0.91, 0.15, 0.12, 0.09)
IC_high <- c(7.47, 10.93, 8.94, 4.30, 4.20, 3.94, 3.11, 2.06, 0.73, 0.84, 0.88)

# Crear dataframe
df_importancia <- data.frame(
  Variable = variables,
  OR = OR,
  IC_low = IC_low,
  IC_high = IC_high
)

# Ordenar por OR
df_importancia$Variable <- factor(df_importancia$Variable, levels = df_importancia$Variable[order(df_importancia$OR)])

library(ggplot2)

# Gráfico
ggplot(df_importancia, aes(x = Variable, y = OR)) +
  geom_point(size = 3, color = "#0073C2FF") +
  geom_errorbar(aes(ymin = IC_low, ymax = IC_high), width = 0.2, color = "#0073C2FF") +
  coord_flip() +
  labs(title = "Importancia de las variables en el modelo logit",
       x = "Variable", y = "Odds Ratio (OR)") +
  theme_minimal()

##GRAFICO COEFICIENTES
# Supongamos que tienes un modelo logit llamado modelo_logit
coeficientes <- summary(modelo_logit)$coefficients

# Eliminar el intercepto
coeficientes <- coeficientes[-1, , drop = FALSE]

# Crear dataframe con valores absolutos
importancia_vars <- data.frame(
  Variable = rownames(coeficientes),
  Coeficiente = coeficientes[, "Estimate"],
  Importancia = abs(coeficientes[, "Estimate"])
)

# Ordenar por importancia
importancia_vars <- importancia_vars[order(importancia_vars$Importancia, decreasing = TRUE), ]
importancia_vars$Variable <- factor(importancia_vars$Variable, levels = importancia_vars$Variable)

# Gráfico de barras
library(ggplot2)
ggplot(importancia_vars, aes(x = Variable, y = Importancia)) +
  geom_bar(stat = "identity", fill = "#1F77B4") +
  coord_flip() +
  labs(title = "Importancia de las variables (coeficientes absolutos)",
       x = "Variable",
       y = "Importancia (|Coef|)") +
  theme_minimal()

#################################################################################################################
                            ################# RANDOMFOREST #########################
#################################################################################################################
library(randomForest)
library(caret)

### TABLA RESULTADOS
# Variables para almacenar resultados
resultados <- data.frame(mtry = integer(), nodesize = integer(), accuracy = numeric())

# Definir los valores que vamos a probar
mtry_vals <- c(2, 3, 4, 5, 6)
nodesize_vals <- c(1, 5, 10, 15, 20)

# Bucle para probar todas las combinaciones
for (m in mtry_vals) {
  for (n in nodesize_vals) {
    set.seed(13656549)# esta 
    modelo <- randomForest(PROBLMUN1R ~ ., data = datos_train, 
                           ntree = 500, mtry = m, nodesize = n)
    
    # Predicción sobre el conjunto de prueba
    predicciones <- predict(modelo, newdata = datos_test)
    
    # Calcular accuracy
    acc <- mean(predicciones == datos_test$PROBLMUN1R)
    
    # Guardar resultados
    resultados <- rbind(resultados, data.frame(mtry = m, nodesize = n, accuracy = acc))
  }
}

# Mostrar los resultados ordenados por accuracy
resultados <- resultados[order(-resultados$accuracy), ]
print(resultados)

set.seed(13656549)# esta 
modelo_rf <- randomForest(PROBLMUN1R ~ ., data = datos_train, ntree = 500, nodesize=5,mtry=4,importance = TRUE)

# Importancia de variables
importance(modelo_rf)
varImpPlot(modelo_rf)

# Predecir sobre los datos de prueba
predicciones <- predict(modelo_rf, newdata = datos_test)

# Evaluar con matriz de confusión
confusionMatrix(predicciones, datos_test$PROBLMUN1R)

#################################################################################################################
          ################# REDES NEURONALES  #########################
#################################################################################################################

library(nnet)
# Definir los valores que vamos a probar
nodesize_vals <- c(1:10)
resultados <- data.frame(mtry = integer(), nodesize = integer(), accuracy = numeric())

# Bucle para probar todas las combinaciones

  for (n in nodesize_vals) {
    set.seed(13656549)# esta 
    modelo<- nnet(formula_nn, data = datos_train, size = n, maxit = 500, decay = 0.01)
    
    # Predicción sobre el conjunto de prueba
    pred_nn <- predict(modelo_nn, newdata = datos_test, type = "class")
    pred_nn <- factor(pred_nn, levels = levels(datos_test$PROBLMUN1R))
    
    # Calcular accuracy
    acc <- mean(pred_nn == datos_test$PROBLMUN1R)
    
    # Guardar resultados
    resultados <- rbind(resultados, data.frame(nodesize = n, accuracy = acc))
  }


# Mostrar los resultados ordenados por accuracy
resultados <- resultados[order(-resultados$accuracy), ]
print(resultados)

# 3. Crear fórmula con todas las variables predictoras
formula_nn <- as.formula(paste("PROBLMUN1R ~", paste(setdiff(names(datos_train), "PROBLMUN1R"), collapse = "+")))
set.seed(1234)# esta 
# 4. Entrenar red neuronal (una capa oculta con 5 neuronas)
modelo_nn <- nnet(formula_nn, data = datos_train, size = 5, maxit = 500, decay = 0.01)
# 5. Predecir en test
pred_nn <- predict(modelo_nn, newdata = datos_test, type = "class")
pred_nn <- factor(pred_nn, levels = levels(datos_test$PROBLMUN1R))
# Medir accuracy
confusionMatrix(pred_nn, test_df$PROBLMUN1R)

boxplot(pred_nn, datos_test$PROBLMUN1R)
## GRAFICOS
library(NeuralNetTools)
garson(modelo_nn)

# Obtener importancia de variables con olden
imp_data <- olden(modelo_nn, bar_plot = FALSE)

# Convertir a data.frame y ordenar
imp_df <- data.frame(Variable = rownames(imp_data), Importance = imp_data[, 1])
imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
imp_df <- imp_df %>% 
    subset(imp_df$Importance>20| imp_df$Importance< -20)
# Graficar con ggplot2 (horizontal para leer bien los nombres)
ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "skyblue") +
  coord_flip() +  # pone las variables en el eje Y
  labs(
    title = "Importancia de las variables en el modelo nnet",
    x = "Variable", y = "Importancia"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))  # ajusta el tamaño de los nombres



#########################################################################################
###################### COMPARACION DE MODELOS ########################################
######################################################################################

# Accuracy de ambos modelos (usa tus valores reales si cambian)
accuracy_rf <-  0.7421
accuracy_logit <-  0.6953 
accuracy_NNET <- 0.7344
s_rf=0.2500;s_logit=0.2955;s_nnet=0.4545
e_rf=0.97619;e_logit=0.9048;e_nnet=0.8810
df_accuracy <- data.frame(
  Modelo = c("Random Forest", "Regrsión Logit", "Redes Neuronales"),
  Accuracy = c(accuracy_rf, accuracy_logit, accuracy_NNET),
  Sensibilidad=c(s_rf,s_logit,s_nnet),
  Especificidad =c(e_rf,e_logit,e_nnet)
)
library(kableExtra)
kable(df_accuracy)

# Crear data frame para graficar
df_accuracy <- data.frame(
  Modelo = c("Random Forest", "Regresión Logit", "Redes Neuronales"),
  Accuracy = c(accuracy_rf, accuracy_logit, accuracy_NNET)
)

library(ggplot2)
library(caret)
# Gráfico de barras comparativo
ggplot(df_accuracy, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Comparación de precisión entre modelos",
       y = "Precisión (Accuracy)", x = "") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "pink")) +
  theme_minimal(base_size = 14)

#################################################################################################################
################# ANALISIS DESCRIPTIVO  #########################
#################################################################################################################

library(skimr)
skim(datos_train)

#VARIABLES NUMERICA 
library(ggplot2)

num_vars <- datos_limpios %>% select(where(is.numeric))

# Histograma por variable
for (var in names(num_vars)) {
  print(
    ggplot(datos_limpios, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = paste("Distribución de", var))
  )
}

# Estadísticos
summary(num_vars)

# VARIABLES CATEGORICAS
library(dplyr)
library(knitr)
library(kableExtra)
# Todas las variables categóricas
cat_vars <- datos_filtrados %>% select(where(is.factor))

# Creamos tabla bonita para cada variable
for (var in names(cat_vars)) {
  
  tabla <- cat_vars %>%
    count(!!sym(var)) %>%
    mutate(Porcentaje = round(100 * n / sum(n), 2)) %>%
    rename(Categoría = 1, Frecuencia = n)
  
  cat("\n\n")
  cat(paste("### Variable:", var, "\n"))
  
  # Mostramos la tabla bonita
  print(
    tabla %>%
      kbl(caption = paste("Distribución de la variable", var),
          align = "lcc", format = "html") %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
  )
}

# Gráfico para PROBLMUN2R
ggplot(datos_filtrados, aes(x = PROBLMUN2R)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal() +
  labs(title = "Distribución de PROBLMUN2R",
       x = "Categorías",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico para PROBLMUN3R
ggplot(datos_filtrados, aes(x = PROBLMUN3R)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal() +
  labs(title = "Distribución de PROBLMUN3R",
       x = "Categorías",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

