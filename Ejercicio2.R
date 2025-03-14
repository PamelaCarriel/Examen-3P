# --------------------------------------------------
# Simulación de la vida útil del componente
# --------------------------------------------------

# install.packages("ggplot2")  # Descomentar si no se tiene instalado
library(ggplot2)

set.seed(456)      # Para reproducibilidad
n <- 1e5           # Número de simulaciones
lambda <- 0.1      # Parámetro de la distribución exponencial

# Generación de la variable aleatoria X ~ Exponential(lambda)
x <- rexp(n, rate = lambda)

# Secuencia para evaluar la densidad y función acumulativa teóricas
x_seq <- seq(0, max(x), length.out = 1000)
densidad_teo <- lambda * exp(-lambda * x_seq)
acumulada_teo <- 1 - exp(-lambda * x_seq)

# Creación del data frame para ggplot
df <- data.frame(x = x)

# Histograma de la simulación junto con la densidad teórica
ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 50, 
                 fill = "lightblue", color = "black", alpha = 0.5) +
  geom_line(data = data.frame(x_seq, densidad_teo), 
            aes(x = x_seq, y = densidad_teo), color = "red", size = 1) +
  labs(title = "Histograma de la vida útil y densidad teórica",
       x = "Vida útil", y = "Densidad") +
  theme_minimal()

# Gráfico de la función de distribución acumulativa teórica
ggplot(data.frame(x_seq, acumulada_teo), aes(x = x_seq, y = acumulada_teo)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Función de distribución acumulada teórica",
       x = "Vida útil", y = "F(x)") +
  theme_minimal()

