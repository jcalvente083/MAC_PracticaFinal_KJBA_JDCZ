# MAC_PracticaFinal_KJBA_JDCZ

## Descripción

Este repositorio contiene la solución a la práctica final de la asignatura **Modelos Avanzados de Computación (MAC)**. El proyecto consiste en una implementación del juego **Conecta4** que integra un jugador con inteligencia artificial (IA) desarrollada en **Haskell**. La solución ha sido desarrollada por los estudiantes **Kevin Jesús Banda Azogil** y **Jesús David Calvente Zapata**.

## Estructura del repositorio

El repositorio incluye los siguientes archivos y directorios:

- `Board.hs`: Módulo que define la estructura y las funciones relacionadas con el tablero del juego.
- `Computer.hs`: Módulo que implementa la lógica de la IA para el jugador.
- `Conecta4.hs`: Archivo principal que coordina la ejecución del juego.
- `aa_compilarHaskell.py`: Script en Python para compilar los archivos Haskell.
- `conecta4.py`: Script en Python para ejecutar el juego.
- `main.exe`: Ejecutable del juego para Windows.
- `main.hs`: Código fuente principal en Haskell.
- `mainTerminal.exe`: Ejecutable del juego en modo terminal para Windows.
- `mainTerminal.hs`: Código fuente principal en Haskell para la versión en terminal.

## Requisitos

Para ejecutar el proyecto en tu entorno local, asegúrate de tener instalados los siguientes programas:

- **GHC (Glasgow Haskell Compiler)**: Para compilar y ejecutar el código Haskell.
- **Python 3.9**: Necesario para ejecutar los scripts en Python.

## Instalación y ejecución

### En Windows

1. **Compilar y ejecutar desde Haskell**:

   - Abre una terminal en el directorio del proyecto.
   - Compila el código Haskell:

     ```bash
     ghc -o main main.hs
     ```

   - Ejecuta el juego:

     ```bash
     ./main.exe
     ```

2. **Ejecutar utilizando el script en Python**:

   - Abre una terminal en el directorio del proyecto.
   - Ejecuta el script:

     ```bash
     python conecta4.py
     ```

   - Sigue las instrucciones en pantalla para jugar.

### En Linux/macOS

1. **Compilar y ejecutar desde Haskell**:

   - Abre una terminal en el directorio del proyecto.
   - Compila el código Haskell:

     ```bash
     ghc -o main main.hs
     ```

   - Ejecuta el juego:

     ```bash
     ./main
     ```

2. **Ejecutar utilizando el script en Python**:

   - Abre una terminal en el directorio del proyecto.
   - Ejecuta el script:

     ```bash
     python3 conecta4.py
     ```

   - Sigue las instrucciones en pantalla para jugar.


## Contacto

Para más información o consultas, puedes contactar a los autores:

- Kevin Jesús Banda Azogil: kevinjesus.banda@alu.uhu.es
- Jesús David Calvente Zapata: jesusdavid.calvente083@alu.uhu.es
