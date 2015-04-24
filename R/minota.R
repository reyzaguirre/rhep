#' Predice la nota final del curso EP1 y EP2
#'
#' Esta funcion predice la nota final del curso basado en datos historicos y un
#' modelo de regresion lineal.
#' @param curso 1 o 2 (corresponde a EP1 o EP2).
#' @param vez Numero de veces que se lleva el curso (1, 2 o 3).
#' @param pp Promedio ponderado.
#' @param prob Probabilidad para la prediccion.
#' @param pa1 Practica de aula 1.
#' @param pa2 Practica de aula 2.
#' @param pa3 Practica de aula 3.
#' @param pa4 Practica de aula 4.
#' @param pi1 Practica integrada 1.
#' @param pi2 Practica integrada 2.
#' @param ep Examen parcial.
#' @author Raul Eyzaguirre.
#' @details No es necesario introducir todos los parametros, el modelo solo
#' considera los que son introducidos.
#' @return Devuelve la nota final estimada con un intervalo de prediccion,
#' y el coeficiente de determinacion del modelo.
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
      stop("Ingrese valor correcto para numero de veces que lleva el curso: 1, 2 o 3.")

  if (is.null(pp) == 0)
    if (pp < 0 | pp > 20)
      stop("Ingrese valor correcto para promedio ponderado: entre 0 y 20.")

  if (length(notas) == 0)
    stop("Debe ingresar al menos una nota.")

  for (i in 1:length(notas)){
    if (notas[[i]] < 0 | notas[[i]] > 20)
      stop(paste("Ingrese valor correcto para ", names(notas)[i], ": entre 0 y 20.", sep=""))
    }

  # Datos

  if (curso == 1)
    subdata <- subset(grades, substring(grades$cs, 1, 1) == "4")

  if (curso == 2)
    subdata <- subset(grades, substring(grades$cs, 1, 1) == "5")

  # Modelos

  if (is.null(vez) == 1 & is.null(pp) == 1){
    formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+")))
    new <- data.frame(t(notas))
  }

  if (is.null(vez) == 1 & is.null(pp) == 0){
    formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+"), "+ pp"))
    new <- data.frame(t(notas), pp = pp)
  }

  if (is.null(vez) == 0 & is.null(pp) == 1){
    formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+"), "+ factor(nt)"))
    new <- data.frame(t(notas), nt = vez)
  }

  if (is.null(vez) == 0 & is.null(pp) == 0){
    formula <- as.formula(paste("fg ~ ", paste(names(notas), collapse = "+"), "+ factor(nt) + pp"))
    new <- data.frame(t(notas), nt = vez, pp = pp)
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

  colnames(pfg) <- c("Prediccion", "Minima", "Maxima", "Probabilidad")

  # Output

  list(Nota_final = pfg,
       Explicacion_del_modelo = paste(round(summary(model)$r.squared*100, 0), "%", sep=""))

}
