## KEVIN JESÚS BANDA AZOGIL
## JESÚS DAVID CALVENTE ZAPATA
import tkinter as tk
import os
import time
import subprocess  # Para ejecutar el archivo .exe
from tkinter import messagebox

class Conecta4App:
    def __init__(self, root):
        # Ejecutar el archivo .exe en un subproceso al iniciar la app
        self.root = root
        self.root.title("Conecta 4")
        self.board = [[0 for _ in range(7)] for _ in range(6)]
        self.create_widgets()
        self.update_board()
        self.save_board_state()
        self.player_turn = True
        self.last_mod_time = 0
        self.run_exe_on_start()

    def run_exe_on_start(self):
        exe_path = "./main.exe" 
        if os.path.exists(exe_path):
            try:
                subprocess.Popen([exe_path]) 
                print("Archivo .exe ejecutado correctamente.")
            except Exception as e:
                print(f"Error al ejecutar el archivo .exe: {e}")
        else:
            print(f"El archivo .exe no se encontró en la ruta: {exe_path}")

    def create_widgets(self):
        self.canvas = tk.Canvas(self.root, width=740, height=640, bg="blue")
        self.canvas.pack(padx=20, pady=20)
        self.canvas.bind("<Button-1>", self.handle_click)

    def handle_click(self, event):
        if not self.player_turn: 
            return
        
        col = (event.x - 20) // 100
        if 0 <= col < 7:
            self.make_move(col, 1)
            self.update_board()
            if self.check_winner(1):
                messagebox.showinfo("Conecta 4", "¡Jugador 1 gana!")
                self.reset_board()
                return

            if self.check_draw():  # Verificamos si hay empate
                messagebox.showinfo("Conecta 4", "¡Empate!")
                self.reset_board()
                return

            self.save_board_state()
            self.player_turn = False  
            self.canvas.unbind("<Button-1>")  
            self.root.after(1000, self.wait_for_player2_move)



    def make_move(self, col, player):
        for row in range(6):  
            if self.board[row][col] == 0:  
                self.board[row][col] = player
                break

    def update_board(self):
        self.canvas.delete("all")
        for row in range(6):
            for col in range(7):
                # Invertimos el índice de la fila para mostrar de abajo hacia arriba
                inverted_row = 5 - row  
                x0 = col * 100 + 20
                y0 = inverted_row * 100 + 20  # Usamos inverted_row en lugar de row
                x1 = x0 + 100
                y1 = y0 + 100
                color = "white" if self.board[row][col] == 0 else ("red" if self.board[row][col] == 1 else "yellow")
                self.canvas.create_oval(x0, y0, x1, y1, fill=color)


    def wait_for_player2_move(self):
        changed = False
        while not changed:
            time.sleep(0.5)
            new_mod_time = os.path.getmtime("board_state.txt")
            changed = new_mod_time != self.last_mod_time
            if changed:
                self.load_board_state()
                self.update_board()
                self.last_mod_time = new_mod_time 
                if self.check_winner(2):
                    messagebox.showinfo("Conecta 4", "¡Jugador 2 gana!")
                    self.reset_board()
                self.player_turn = True  
                self.canvas.bind("<Button-1>", self.handle_click)  
                break

    def load_board_state(self):
        with open("board_state.txt", "r") as file:
            lines = [line.strip().split() for line in file if line.strip()]  # Leer y limpiar líneas vacías

        if len(lines) != 6 or any(len(line) != 7 for line in lines):  # Validar dimensiones
            raise ValueError("El archivo board_state.txt no tiene el formato correcto.")

        self.board = [[int(cell) for cell in line] for line in lines]

        
    def save_board_state(self):
        with open("board_state.txt", "w") as file:
            for row in self.board:
                file.write(" ".join(map(str, row)) + "\n")

        self.last_mod_time = os.path.getmtime("board_state.txt")

        

    def check_winner(self, player):
        for row in range(6):
            for col in range(7):
                if self.check_line(player, row, col, 1, 0) or \
                   self.check_line(player, row, col, 0, 1) or \
                   self.check_line(player, row, col, 1, 1) or \
                   self.check_line(player, row, col, 1, -1):
                    return True
        return False
    
    def check_draw(self):
        for row in self.board:
            if 0 in row:  
                return False
        return True  


    def check_line(self, player, row, col, delta_row, delta_col):
        count = 0
        for i in range(4):
            r = row + i * delta_row
            c = col + i * delta_col
            if 0 <= r < 6 and 0 <= c < 7 and self.board[r][c] == player:
                count += 1
            else:
                break
        return count == 4

    def reset_board(self):
        self.board = [[0 for _ in range(7)] for _ in range(6)]
        self.update_board()
        self.save_board_state()
        self.player_turn = True
        self.canvas.bind("<Button-1>", self.handle_click)  
        self.run_exe_on_start()

if __name__ == "__main__":
    root = tk.Tk()
    app = Conecta4App(root)
    root.mainloop()
