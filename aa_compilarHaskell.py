## KEVIN JESÚS BANDA AZOGIL
## JESÚS DAVID CALVENTE ZAPATA
import os

directorio_objetivo = "./"  
extensiones_a_borrar = [".o", ".hi", ".txt"]

def borrar_archivos(directorio=directorio_objetivo, extensiones=extensiones_a_borrar, exe=False):

    extensionesUsar = extensiones.copy()
    if exe:
        extensionesUsar.append(".exe")

    print(f"Borrando archivos con extensiones {extensionesUsar} en el directorio...")

    if not os.path.exists(directorio):
        print(f"Error: El directorio '{directorio}' no existe.")
        return
    
    # Recorrer todos los archivos en el directorio
    for archivo in os.listdir(directorio):
        ruta_completa = os.path.join(directorio, archivo)

        # Verificar si es un archivo y si su extensión está en la lista
        if os.path.isfile(ruta_completa) and any(archivo.endswith(ext) for ext in extensionesUsar):
            try:
                os.remove(ruta_completa)
                print(f"Archivo eliminado: {ruta_completa}")
            except Exception as e:
                print(f"No se pudo eliminar {ruta_completa}: {e}")

def compilarHaskell():
    print("Ejecutando Compilado de Haskell...")
    try:
        os.system("ghc main.hs -o main.exe") 
        os.system("ghc mainTerminal.hs -o mainTerminal.exe") 
        
    except Exception as e:
        print(f"Error al ejecutar Haskell: {e}")

if __name__ == "__main__":
    borrar_archivos(exe=True)
    compilarHaskell()
    borrar_archivos()
