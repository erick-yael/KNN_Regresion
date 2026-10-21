# Librerías necesarias
library(ggplot2)
library(FNN)  # Para el KNN

# Generar datos de ejemplo
set.seed(123)
x <- seq(0, 10, length.out = 100)
y <- sin(x) + rnorm(100, 0, 0.3)  # Datos con algo de ruido
data <- data.frame(x, y)

# Graficar los datos originales sin ninguna curva
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.9) +
  labs(title = "Datos",
       x = "X",
       y = "Y") +
  theme_minimal()

# Función para realizar KNN y visualizar
plot_knn_regression <- function(k) {
  # Crear secuencia de predicción
  x_grid <- seq(min(x), max(x), length.out = 200)
  
  # Ajustar KNN regresión
  knn_result <- knn.reg(train = matrix(x, ncol = 1), 
                        test = matrix(x_grid, ncol = 1), 
                        y = y, 
                        k = k)
  
  # Crear un dataframe para la visualización
  pred_data <- data.frame(x_grid, knn_result$pred)
  colnames(pred_data) <- c("x", "y_pred")
  
  # Visualizar con ggplot
  ggplot() +
    geom_point(data = data, aes(x = x, y = y), color = "blue", alpha = 0.6) +
    geom_line(data = pred_data, aes(x = x, y = y_pred), color = "red", size = 1) +
    labs(title = paste("KNN Regression con k =", k),
         x = "X",
         y = "Y") +
    theme_minimal()
}

# Modificar el valor de k aquí
k_value <- 7
plot_knn_regression(k_value)

