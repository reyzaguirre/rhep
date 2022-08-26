#' Predice la nota final del curso EP1 y EP2
#'
#' Esta función predice la nota final del curso basado en datos históricos y un
#' modelo de regresión lineal.
#'
#' @param curso 1 o 2 (corresponde a EP1 o EP2).
#' @param vez Numero de veces que se lleva el curso (1, 2 o 3).
#' @param pp Promedio ponderado.
#' @param prob Probabilidad para la predicción.
#' @param pa1 Practica de aula 1.
#' @param pa2 Practica de aula 2.
#' @param pa3 Practica de aula 3.
#' @param pa4 Practica de aula 4.
#' @param pi1 Practica integrada 1.
#' @param pi2 Practica integrada 2.
#' @param ep Examen parcial.
#' @author Raúl Eyzaguirre.
#' @details No es necesario introducir todos los parámetros, el modelo solo
#' considera los que son introducidos.
#' @return Devuelve la nota final estimada con un intervalo de predicción,
#' y el coeficiente de determinación del modelo.
#' @examples
#' minota(curso = 1, pa1 = 12)
#' @importFrom stats as.formula lm predict
#' @export

minota <- function(curso = NULL, vez = NULL, pp = NULL, prob = 0.95,
                    pa1= NULL, pa2= NULL, pa3 = NULL, pa4 = NULL,
                    pi1 = NULL, pi2 = NULL, ep = NULL) {

  # Lista de notas

  notas <- c(pa1 = pa1, pa2 = pa2, pa3 = pa3, pa4 = pa4, pi1 = pi1, pi2 = pi2, ep = ep)

  # Mensajes de error

  if (curso != 1 & curso != 2)
    stop("Ingrese valor correcto para curso: 1 o 2.")

  if (!is.null(vez))
    if (vez != 1 & vez != 2 & vez != 3)
      stop("Ingrese valor correcto para numero de veces que lleva el curso: 1, 2 o 3.")

  if (!is.null(pp))
    if (pp < 0 | pp > 20)
      stop("Ingrese valor correcto para promedio ponderado: entre 0 y 20.")

  if (length(notas) > 0) {
    for (i in 1:length(notas)) {
      if (notas[[i]] < 0 | notas[[i]] > 20)
        stop(paste("Ingrese valor correcto para ", names(notas)[i], ": entre 0 y 20.", sep = ""))
    }
  }

  if (length(notas) == 0 & is.null(vez) & is.null(pp))
    stop("Ingrese al menos un predictor.")

  # Datos

  if (curso == 1)
    subdata <- subset(grades, substring(grades$seccion, 1, 1) == "4")

  if (curso == 2)
    subdata <- subset(grades, substring(grades$seccion, 1, 1) == "5")

  # Modelo solo con notas

  if (is.null(vez) & is.null(pp)) {
    formula <- as.formula(paste("final ~ ", paste(names(notas), collapse = "+")))
    new <- data.frame(t(notas))
  }

  # Modelo con pp y opcional notas

  if (is.null(vez) & !is.null(pp)) {
    if (length(notas) > 0) {
      formula <- as.formula(paste("final ~ ", paste(names(notas), collapse = "+"), "+ pp"))
      new <- data.frame(t(notas), pp = pp)
    } else {
      formula <- as.formula("final ~ pp")
      new <- data.frame(pp = pp)
    }
  }

  # Modelo con vez y opcional notas

  if (!is.null(vez) & is.null(pp)) {
    if (length(notas) > 0) {
      formula <- as.formula(paste("final ~ ", paste(names(notas), collapse = "+"), "+ factor(vez)"))
      new <- data.frame(t(notas), vez = vez)
    } else {
      formula <- as.formula("final ~ factor(vez)")
      new <- data.frame(vez = vez)
    }
  }

  # Modelo con vez y pp y opcional notas

  if (!is.null(vez) & !is.null(pp)) {
    if (length(notas) > 0) {
      formula <- as.formula(paste("final ~ ", paste(names(notas), collapse = "+"), "+ factor(vez) + pp"))
      new <- data.frame(t(notas), vez = vez, pp = pp)
    } else {
      formula <- as.formula("final ~ factor(vez) + pp")
      new <- data.frame(vez = vez, pp = pp)
    }
  }

  # Correr modelo

  model <- lm(formula, data = subdata)

  # Prediccion

  pfinal <- predict(model, new, interval = "p", level = prob)
  pfinal <- round(pfinal, 1)

  if (pfinal[1] <  0) pfinal[1] <-  0
  if (pfinal[1] > 20) pfinal[1] <- 20
  if (pfinal[2] <  0) pfinal[2] <-  0
  if (pfinal[3] > 20) pfinal[3] <- 20

  pfinal <- cbind(pfinal, prob)

  colnames(pfinal) <- c("Prediccion", "Minima", "Maxima", "Probabilidad")

  # Output

  list(Nota_final = pfinal,
       Explicacion_del_modelo = paste(round(summary(model)$r.squared * 100, 0), "%", sep = ""))
}
