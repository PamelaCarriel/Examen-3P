# --------------------------------------------------
# Simulación de detección de defectos
# --------------------------------------------------

# Paquetes (asegúrate de tener ggplot2 instalado)
# install.packages("ggplot2")
library(ggplot2)

set.seed(123)  # Para reproducibilidad

N <- 1e5   # número de piezas simuladas
pD <- 0.05 # probabilidad de defecto
alpha <- 0.90 # sensibilidad

# Creamos un vector de tasas de falso positivo (beta) para explorar
beta_values <- seq(0.01, 0.20, by = 0.01)

results <- data.frame(
  beta = beta_values,
  P_D_given_Tplus_teorico = NA,
  P_D_given_Tplus_sim = NA
)

for(i in seq_along(beta_values)){
  
  beta_i <- beta_values[i]
  
  # 1. Generamos las piezas defectuosas (TRUE/FALSE)
  defectos <- runif(N) < pD
  
  # 2. Detección del test
  # Para cada pieza defectuosa, probabilidad de T+ es alpha; si no es defectuosa, T+ con prob beta_i
  test_positive <- ifelse(defectos, 
                          runif(N) < alpha, 
                          runif(N) < beta_i)
  
  # 3. Cálculo teórico de P(D|T+)
  p_Tplus <- alpha * pD + beta_i * (1 - pD)
  p_D_given_Tplus_teo <- (alpha * pD) / p_Tplus
  
  # 4. Cálculo empírico de P(D|T+)
  # Se mira entre todos los que dieron T+, cuántos son realmente defectuosos
  Tplus_indices <- which(test_positive)
  p_D_given_Tplus_emp <- mean(defectos[Tplus_indices])
  
  results$P_D_given_Tplus_teorico[i] <- p_D_given_Tplus_teo
  results$P_D_given_Tplus_sim[i] <- p_D_given_Tplus_emp
}

# Representamos gráficamente los resultados teóricos vs simulados
ggplot(results, aes(x = beta)) +
  geom_line(aes(y = P_D_given_Tplus_teorico), linetype = "solid") +
  geom_point(aes(y = P_D_given_Tplus_sim)) +
  labs(x = "Falso Positivo (beta)", 
       y = "P(D | T+)",
       title = "Probabilidad a posteriori de defecto vs Tasa de falso positivo") +
  theme_minimal()

