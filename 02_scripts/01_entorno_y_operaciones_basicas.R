############################################
# 1. Hola mundo
############################################

print("Hola mundo")


############################################
# 2. Crear objetos
############################################

x <- 10
y <- 5

x
y


############################################
# 3. Aritmética básica
############################################

x + y     # suma
x - y     # resta
x * y     # multiplicación
x / y     # división
x ^ 2     # potencia
x %% y    # módulo (residuo)
x %/% y   # división entera


############################################
# 4. Objetos y vectores
############################################

# Vector numérico
numeros <- c(1, 2, 3, 4, 5)
numeros

# Vector de texto
nombres <- c("Jesus", "Abel", "Maria")
nombres

# Longitud del vector
length(nombres)

# Acceso por posición
nombres[1]       # primer elemento
numeros[3]       # tercer elemento

# Operaciones elemento a elemento
numeros * 2

# Filtrar usando lógica
numeros[numeros > 3]   # devuelve 4 y 5


############################################
# 5. Tipos básicos de objetos
############################################

class(numeros)   # numeric
class(nombres)   # character
class(TRUE)      # logical

