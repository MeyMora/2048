# 🎮 Proyecto 2048 - Programación Funcional en Racket

## Descripción

Este proyecto consiste en el desarrollo del juego **2048**, implementado en el lenguaje Racket utilizando el paradigma de programación funcional.

El juego se basa en una cuadrícula donde el usuario debe combinar baldosas con el mismo valor hasta alcanzar el número 2048.

---

##  Objetivo General

Desarrollar una aplicación que permita reafirmar el conocimiento del paradigma de programación funcional, recursividad y estructuras de datos.

---

##  Objetivos Específicos

- Implementar el juego 2048 en Racket.
- Aplicar programación funcional sin uso de estructuras imperativas.
- Utilizar listas como estructura de datos principal.

---

## Funcionalidades

- Generación automática de un tablero MxN (mínimo 4x4, máximo 10x10).
- Inicialización con dos baldosas con valor 2 en posiciones aleatorias.
- Movimiento del tablero en cuatro direcciones:
  - Izquierda
  - Derecha
  - Arriba
  - Abajo
- Combinación de baldosas con el mismo valor.
- Generación de nuevas baldosas (2 o 4).
- Detección de victoria al alcanzar 2048.

---

## 🧩 Estructura del Proyecto


```text
Proyecto2048/
├── README.md
├── interfaz.rkt
└── tablero_base.rkt

### Descripción de archivos

- **README.md**  
  Contiene la descripción general del proyecto, objetivos, funcionalidades y estructura.

- **interfaz.rkt**  
  Maneja la interfaz gráfica del juego.  
  Aquí se definen:
  - El dibujo del tablero y las fichas.
  - Los colores y dimensiones de la ventana.
  - La visualización del puntaje.
  - La interacción con el usuario.

- **tablero_base.rkt**  
  Contiene la lógica principal del juego.  
  Aquí se implementan funciones como:
  - Creación del tablero.
  - Acceso y reemplazo de celdas.
  - Movimiento a la izquierda, derecha, arriba y abajo.
  - Combinación de fichas.
  - Verificación de victoria o derrota.
  - Generación de fichas aleatorias.
