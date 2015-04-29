#' Predice la nota final del curso EP1 y EP2
#'
#' Esta función predice la nota final del curso basado en datos históricos y un
#' modelo de regresión lineal.
#' @param curso 1 o 2 (corresponde a EP1 o EP2).
#' @param vez Número de veces que se lleva el curso (1, 2 o 3).
#' @param pp Promedio ponderado.
#' @param prob Probabilidad para la predicción.
#' @param pa1 Práctica de aula 1.
#' @param pa2 Práctica de aula 2.
#' @param pa3 Práctica de aula 3.
#' @param pa4 Práctica de aula 4.
#' @param pi1 Práctica integrada 1.
#' @param pi2 Práctica integrada 2.
#' @param ep Examen parcial.
#' @author Raúl Eyzaguirre.
#' @details No es necesario introducir todos los parámetros, el modelo solo
#' considera los que son introducidos.
#' @return Devuelve la nota final estimada con un intervalo de predicción,
#' y el coeficiente de determinación del modelo.
#' @examples
#' minota(curso = 1, pa1 = 12)
#' @export

minota <- function(curso = NULL, vez = NULL, pp = NULL, prob = 0.95,
                    pa1= NULL, pa2= NULL, pa3 = NULL, pa4 = NULL,
                    pi1 = NULL, pi2 = NULL, ep = NULL){

  # Lista de notas

  notas <- c(pa1 = pa1, pa2 = pa2, pa3 = pa3, pa4 = pa4, pi1 = pi1, pi2 = pi2, ep = ep)

  # Mensajes de error

  if (curso != 1 & curso != 2)
    stop("Ingrese valor correcto para curso: 1 o 2.")

  if (is.null(vez) == 0)
    if (vez != 1 & vez != 2 & vez != 3)
      stop("Ingrese valor correcto para número de veces que lleva el curso: 1, 2 o 3.")

  if (is.null(pp) == 0)
    if (pp < 0 | pp > 20)
      stop("Ingrese valor correcto para promedio ponderado: entre 0 y 20.")

  if (length(notas) > 0){
    for (i in 1:length(notas)){
      if (notas[[i]] < 0 | notas[[i]] > 20)
        stop(paste("Ingrese valor correcto para ", names(notas)[i], ": entre 0 y 20.", sep=""))
    }
  }

  if (length(notas) == 0 & is.null(vez) == 1 & is.null(pp) == 1)
    stop("Ingrese al menos un predictor.")

  # Datos

  if (curso == 1)
    subdata <- subset(grades, substring(grades$cs, 1, 1) == "4")

  if (curso == 2)
    subdata <- subset(grades, substring(grades$cs, 1, 1) == "5")

  # Modelo solo con notas

  if (is.null(vez) == 1 & is.null(pp) == 1){
    formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+")))
    new <- data.frame(t(notas))
  }

  # Modelo con pp y opcional notas

  if (is.null(vez) == 1 & is.null(pp) == 0){
    if (length(notas) > 0){
      formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+"), "+ pp"))
      new <- data.frame(t(notas), pp = pp)
    } else {
      formula <- as.formula("fg ~ pp")
      new <- data.frame(pp = pp)
    }
  }

  # Modelo con vez y opcional notas

  if (is.null(vez) == 0 & is.null(pp) == 1){
    if (length(notas) > 0){
      formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+"), "+ factor(nt)"))
      new <- data.frame(t(notas), nt = vez)
    } else {
      formula <- as.formula("fg ~ factor(nt)")
      new <- data.frame(nt = vez)
    }
  }

  # Modelo con vez y pp y opcional notas

  if (is.null(vez) == 0 & is.null(pp) == 0){
    if (length(notas) > 0){
      formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+"), "+ factor(nt) + pp"))
      new <- data.frame(t(notas), nt = vez, pp = pp)
    } else {
      formula <- as.formula("fg ~ factor(nt) + pp")
      new <- data.frame(nt = vez, pp = pp)
    }
  }

  model <- lm(formula, data = subdata)

  # Prediccion

  pfg <- predict(model, new, interval = "p", level = prob)
  pfg <- round(pfg, 1)

  if (pfg[1] <  0) pfg[1] <-  0
  if (pfg[1] > 20) pfg[1] <- 20
  if (pfg[2] <  0) pfg[2] <-  0
  if (pfg[3] > 20) pfg[3] <- 20

  pfg <- cbind(pfg, prob)

  colnames(pfg) <- c("Predicción", "Mínima", "Máxima", "Probabilidad")

  # Output

  list(Nota_final = pfg,
       Explicacion_del_modelo = paste(round(summary(model)$r.squared*100, 0), "%", sep=""))
}
