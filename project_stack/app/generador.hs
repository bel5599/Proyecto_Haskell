module Generador where

import System.Random
import System.IO.Unsafe
import Solucionador


generador :: Int -> Int -> [[Int]]
generador cant_filas cant_columnas = let cant_obt=(cant_filas * cant_columnas) * random_obstaculos/100
                                         matrix= matrix_con_obstaculos (crear_Matrix cant_filas cant_columnas) cant_obt
                                      in generar_matrix_aleatoria matrix cant_obt
                                    


generar_matrix_aleatoria :: [[Int]] -> Int -> [[Int]]
generar_matrix_aleatoria matrix obst= let ((pos_f_ini,pos_c_ini),(pos_f_fin,pos_c_fin),matrix_act)=annadir_posicion_inicial_final matrix obstaculos
                                          hidato = rellenar_matrix_hidato (pos_f_ini,pos_c_ini) (pos_f_fin,pos_c_fin) matrix_act
                                       in if length hidato == 0 then
                                        generar_matrix_aleatoria matrix obst
                                        else remover_casillas 0 0 (pos_f_ini,pos_c_ini) (pos_f_fin,pos_c_fin) hidato!!0

rellenar_matrix_hidato :: (Int,Int) -> (Int,Int) -> [[Int]] -> [[[Int]]]
rellenar_matrix_hidato (fila_inicial,columna_inicial) (fila_final,columna_final) matrix = dfs 1 (fila_inicial,columna_inicial) (fila_final,columna_final) matrix []
                                                                                          

annadir_posicion_inicial_final :: [[Int]] -> ((Int,Int),(Int,Int),[[Int]])
annadir_posicion_inicial_final matrix@(x:xs) obst = let pos_fila_inicial=getRandomNumber 0 (length matrix)
                                                        pos_col_inicial=getRandomNumber 0 (length x)
                                                        pos_fila_final = getRandomNumber 0 (length matrix)
                                                        pos_col_final=getRandomNumber 0 (length x)
                                                    in if matrix!!pos_fila_inicial!!pos_col_inicial == 0 && matrix!!pos_fila_inicial!!pos_col_final == 0 then
                                                        let matrix_Act=modificar_hidato 1 (pos_fila_inicial,pos_col_inicial) 0 matrix
                                                            matrix_final=modificar_hidato ((length x) + (length xs) - obst) (pos_fila_final,pos_col_final) 0 matrix_Act
                                                        in ((pos_fila_inicial,pos_col_inicial),(pos_fila_final,pos_col_final), matrix_final)
                                                        else annadir_posicion_inicial_final matrix obst

crear_Matrix :: Int -> Int -> [[Int]]
crear_Matrix 0 _=[]
crear_Matrix filas columnas = (llenar_filas columnas) : crear_Matrix (filas-1) columnas

llenar_filas :: Int -> [Int]
llenar_filas 0=[]
llenar_filas n = 0:llenar_filas (n-1)

matrix_con_obstaculos :: [[Int]] -> Int ->[[Int]]
matrix_con_obstaculos matrix obstaculos = annadir_obstaculos_matrix matrix (annadir_obstaculos matrix) obstaculos

annadir_obstaculos_matrix :: [[Int]] ->[(Int,Int)] -> Int -> [[Int]]
annadir_obstaculos_matrix matrix [] _=matrix
annadir_obstaculos_matrix matrix lista obstaculos = let matrix_Act=modificar_hidato (-1) (head lista) 0 matrix
                                                    in annadir_obstaculos_matrix matrix_Act (tail lista)

annadir_obstaculos :: [[Int]] ->[(Int,Int)]
annadir_obstaculos matrix@(x:xs) = lista_casillas_obst cant_casillas_obst matrix

lista_casillas_obst :: Int -> [[Int]] -> [(Int,Int)]
lista_casillas_obst 0 _ =[]
lista_casillas_obst n matrix@(x:xs) = let fila= getRandomNumber 0 (length matrix)
                                          columna=getRandomNumber 0 (length x)
                                      in (fila,columna) : lista_casillas_obst (n-1) matrix 

random_obstaculos :: Int
random_obstaculos = let r= getRandomNumber 1 3
                     in if r==1 then 50
                        else if r==2 then 20
                        else 0

remover_casillas :: Int -> Int ->(Int,Int) -> [[Int]] -> [[Int]]
remover_casillas posf posc (f_ini,c_ini) (f_fin,c_fin) m@(x:xs)= if posf < (length m) && posc < (length x) && posf/=f_ini && posc/=c_ini && posf/=f_fin && posc/=c_fin then 
                                                                let matrix_act=modificar_hidato 0 (posf,posc) 0 m
                                                                    sol = dfs 2 (f_ini,c_ini) (f_fin,c_fin) matrix_act []
                                                                in if length sol >1 then
                                                                    remover_casillas posf (posc+1) (f_ini,c_ini) (f_fin,c_fin) m
                                                                   else remover_casillas posf (posc+1) (f_ini,c_ini) (f_fin,c_fin) matrix_act
                                                                else if posf < (length m) && posc < (length x) then remover_casillas posf (posc+1) (f_ini,c_ini) (f_fin,c_fin) m
                                                                else if posf < (length m) then remover_casillas (posf+1) (0) (f_ini,c_ini) (f_fin,c_fin) m
                                                                else m

getRandomNumber :: Int -> Int -> Int
getRandomNumber a b = unsafePerformIO (randomRIO (a,b):: IO Int)
