# Plantilla de pruebas — TP1 MonaLambda (Scala 3)

Repositorio mínimo para el intérprete de cálculo lambda del TP1. El programa debe leer **líneas por la entrada estándar** (definiciones `NOMBRE=expr`, `calc_vars`, `reduce cbn|cbv`, etiquetas `!NOMBRE`, etc.), según el enunciado.

## Requisitos

- [sbt](https://www.scala-sbt.org/) (Scala Build Tool)
- Java compatible con Scala 3
- Unix/macOS o entorno con `bash` para `./test_runner.sh`

## Estructura del proyecto

```
.
├── src/main/scala/Main.scala   # Punto de entrada (stdin)
├── ejemplos/
│   ├── inputs/                 # Casos de prueba: una línea = una orden al intérprete
│   └── outputs/                # Salida esperada en stdout (.res con el mismo nombre base)
├── test_runner.sh
├── build.sbt
└── README.md
```

## stdin y sbt (`fork` + `connectInput`)

El proyecto declara en `build.sbt`:

```scala
Compile / run / fork := true
Compile / run / connectInput := true
```

**Por qué:** si `fork` es `false` (valor histórico de sbt), `run` ejecuta `Main` **dentro de la JVM de sbt**. En ese modo la entrada estándar **no tiene por qué** ser la del proceso que invocaste (en la práctica viene vacía o se comporta mal con `run < archivo`, WSL y algunos IDEs).

Con **`fork := true`** el programa corre en otro proceso Java y **`connectInput := true`** le enchufa ese mismo stdin que recibe sbt desde la terminal o la redirección `< archivo`.

`Source.stdin.getLines()` y `StdIn.readLine()` son válidos cuando está bien conectado el stdin; cualquiera de los dos debería servir junto con la configuración anterior.

## Cómo correr el intérprete

Desde la raíz del proyecto, compilá y ejecutá **sin argumentos**; escribí órdenes por stdin (o redirigí un archivo):
Desde la **raíz del repo**:

```bash
sbt compile
sbt run < mi_entrada.txt
```

En una sola invocación:
Ejemplo contra un caso de ejemplo:

```bash
sbt run < ejemplos/inputs/omega_cbn.txt
```

En una sola invocación (útil para scripts; no bloquea en el prompt interactivo de sbt):

```bash
sbt -batch run < mi_entrada.txt
```

`-batch` solo evita el modo shell interactivo de sbt; **no** “roba” líneas del archivo pasado por `<`; el problema que suelen tener los grupos sin `fork`/`connectInput` es otro.

## IntelliJ / Cursor / otros IDEs

- **Pruebas automatizadas:** ejecutá `./test_runner.sh` desde una **terminal** del IDE en la carpeta raíz del proyecto (no suele estar ligada al problema de `-batch`).
- **Ejecutar `Main` con archivo de entrada:** el botón “Run” típico **no** redirige un `.txt` a stdin salvo que lo configures en la corrida.

En **IntelliJ IDEA** con plugin Scala:

1. *Run → Edit Configurations…* → tu configuración de `Main`.
2. *Modify options → Redirect input from* y elegís el archivo `.txt`.

Si no aparece esa opción, usá una terminal dentro del IDE y el comando `sbt run < ejemplos/inputs/omega_cbn.txt`.

Asegurate de tener **Working directory** = raíz del proyecto (donde está `build.sbt`).

## Cómo correr las pruebas de la cátedra

```bash
chmod +x test_runner.sh
./test_runner.sh
```

El script exporta `SBT_OPTS=-Dsbt.color=false` (para logs sin ANSI), compila con `sbt -batch compile`, ejecuta `sbt -batch run` redirigiendo cada `ejemplos/inputs/<caso>.txt` a stdin y compara la salida filtrada (líneas `[info]` / `[success]` de sbt, etc.) con `ejemplos/outputs/<caso>.res`.

## Formato de los archivos de ejemplo

- **`ejemplos/inputs/<caso>.txt`**: texto UTF-8, una instrucción por línea (sin prompts).
- **`ejemplos/outputs/<caso>.res`**: lo que debe imprimirse a **stdout**, línea a línea, en el mismo orden que las respuestas a cada instrucción.

Los nombres deben coincidir: `foo.txt` ↔ `foo.res`.

## Salida del script

- Pruebas exitosas o fallidas por caso
- Resumen de cantidad de exitosos y fallidos
- Código de salida distinto de cero si hubo fallos