# Función para convertir una letra a número para interpretar de mejor manera
# las columnas de exce.

library(tidyverse)
letranum <- function(letra){
x <- function(letra){
  letra = str_to_upper(letra)
  salida <- switch(letra,
    'A' = 1,
  'B' = 2,
  'C' = 3,
  'D' = 4,
  'E' = 5,
  'F' = 6,
  'G' = 7,
  'H' = 8,
  'I' = 9,
  'J' = 10,
  'K' = 11,
  'L' = 12,
  'M' = 13,
  'N' = 14,
  'O' = 15,
  'P' = 16,
  'Q' = 17,
  'R' = 18,
  'S' = 19,
  'T' = 20,
  'U' = 21,
  'V' = 22,
  'W' = 23,
  'X' = 24,
  'Y' = 25,
  'Z' = 26
  )
 return(salida)
}

salida=0
j=nchar(letra)
for (i in 1:nchar(letra)) {
  if (i == nchar(letra)) {
   salida = salida + x(substring(letra,i,i))
  } else {
  salida = x(substring(letra,i,i)) * ((j-1)*26) + salida
  }
  j=j-1
}

return(salida)

}


letranum("AB")
