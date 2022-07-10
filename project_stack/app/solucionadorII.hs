

solucionador :: IO ()
solucionador = print (dfs 2 (1,0) (3,1) [[2,3,4,5],[1,0,0,-1],[-1,0,0,9],[13,14,11,10]] [])
-- -- (2,3) (0,3) [[5,0,0,12],[0,0,0,0],[0,0,0,1]] []
-- -- (4,6) (3,4) [[0,33,35,0,0,-1,-1,-1],[0,0,24,22,0,-1,-1,-1],[0,0,0,21,0,0,-1,-1],[0,26,0,13,40,11,-1,-1],[27,0,0,0,9,0,1,-1],[-1,-1,0,0,18,0,0,-1],[-1,-1,-1,-1,0,7,0,0],[-1,-1,-1,-1,-1,-1,5,0]]
-- -- (0,7) (5,0) [[-1,-1,-1,-1,-1,-1,0,1,-1,-1],[0,0,-1,-1,4,0,0,38,0,35],[73,0,75,0,0,41,0,0,34,0],[0,78,0,0,6,0,8,0,31,0],[0,0,68,0,0,0,44,0,0,32],[80,50,64,0,0,0,11,21,0,0],[0,51,49,0,47,0,0,20,23,0],[53,0,0,48,14,16,18,0,27,0],[55,0,0,58,0,0,-1,-1,25,0],[-1,-1,57,59,-1,-1,-1,-1,-1,-1]]
-- -- (1,0) (3,1) [[0,0,4,0],[1,0,0,-1],[-1,0,0,9],[0,14,0,0]]


dfs :: Int -> (Int,Int) -> (Int,Int) -> [[Int]] ->[[[Int]]] ->[[[Int]]]
dfs sol (fila_inicial,columna_inicial) (fila_final,columna_final) hidato soluciones | fila_inicial==fila_final && columna_inicial == columna_final = soluciones ++ [hidato]
                                                                                    | otherwise = dfs_recursivo sol (fila_inicial,columna_inicial) (fila_final,columna_final) hidato 0 soluciones


dfs_recursivo :: Int -> (Int,Int) -> (Int,Int) -> [[Int]] -> Int -> [[[Int]]] ->[[[Int]]]
dfs_recursivo _ _ _ _ 8 soluciones= soluciones
dfs_recursivo sol (f_actual,c_actual) (f_final,c_final) hidato pos soluciones = let (sucesor_ady,(x,y))=sucesor_adyacente (f_actual,c_actual) hidato
                                                                                 in if sucesor_ady then
                                                                                    let sol_act=dfs sol (x,y) (f_final,c_final) hidato soluciones
                                                                                    in if length sol_act >=sol then sol_act
                                                                                       else sol_act
                                                                                    else if x/=(-1) && y/=(-1) 
                                                                                       then soluciones
                                                                                    else 
                                                                                    let (f_act,c_act)=dir_mov (f_actual,c_actual) pos
                                                                                    in if casilla_valida (f_act,c_act) hidato then
                                                                                       let sol_act=dfs sol (f_act,c_act) (f_final,c_final) (modificar_hidato (valor_sucesor (f_actual,c_actual) hidato) (f_act,c_act) 0 hidato) soluciones
                                                                                       in if length sol_act >=sol then sol_act
                                                                                          else dfs_recursivo sol (f_actual,c_actual) (f_final,c_final) hidato (pos+1) sol_act
                                                                                       else 
                                                                                       dfs_recursivo sol (f_actual,c_actual) (f_final,c_final) hidato (pos+1) soluciones

modificar_hidato :: Int -> (Int,Int)-> Int -> [[Int]] -> [[Int]]
modificar_hidato valor (f_act,c_act) pos (x:xs) |pos==f_act = [(modificar_fila c_act x valor 0)] ++ xs
                                                |otherwise = [x] ++ (modificar_hidato valor (f_act,c_act) (pos+1) xs)

modificar_fila :: Int-> [Int] -> Int -> Int -> [Int]
modificar_fila pos (x:xs) valor pos_actual |pos==pos_actual = [valor] ++ xs
                                           |otherwise = [x] ++ (modificar_fila pos xs valor (pos_actual+1))

dir_mov :: (Int,Int) -> Int -> (Int,Int)
dir_mov (x,y) pos = let array_mov = [[-1,1,-1,1,0,-1,1,0], [0,-1,1,1,-1,-1,0,1]]
                     in (x + array_mov!!0!!pos, y + array_mov!!1!!pos)

sucesor_adyacente :: (Int,Int) -> [[Int]] -> (Bool,(Int,Int))
sucesor_adyacente (f_act,c_act) hidato = let (se_encuentra,(x,y)) =se_encuentra_sucesor (f_act,c_act) hidato
                                             (dif_f,dif_c) = (f_act-x, c_act-y)
                                          in if se_encuentra
                                             then 
                                                if (dif_f == 1 || dif_f == 0 || dif_f == (-1)) && (dif_c == 1 || dif_c == 0 || dif_c == (-1))
                                                then 
                                                   (True,(x,y))
                                                else 
                                                   (False,(x,y))
                                             else (False,(x,y))


se_encuentra_sucesor :: (Int,Int) -> [[Int]] -> (Bool,(Int,Int))
se_encuentra_sucesor (f_act,c_act) hidato = let (x,y) = buscar_sucesor (valor_sucesor (f_act,c_act) hidato) hidato
                                             in if (x,y)==(-1,-1) then (False,(x,y))
                                                else (True,(x,y))

valor_sucesor :: (Int,Int) -> [[Int]] -> Int
valor_sucesor (f_act,c_act) hidato = let valor = hidato!!f_act!!c_act
                                      in valor+1

buscar_sucesor :: Int -> [[Int]] ->(Int,Int)
buscar_sucesor valor hidato@(firts:resto) = let length_filas = (length hidato) -1
                                                length_columnas=(length firts) -1
                                                lista=[(x,y) | x <-[0..length_filas],
                                                               y <-[0..length_columnas],
                                                               hidato!!x!!y == valor
                                                      ]
                                             in if lista == [] then (-1,-1)
                                                else head lista

casilla_valida :: (Int,Int) -> [[Int]] -> Bool
casilla_valida (x,y) hidato |(posicion_valida (x,y) hidato) && (casilla_vacia (x,y) hidato) = True
                            |otherwise =  False


posicion_valida :: (Int, Int) -> [[Int]] -> Bool
posicion_valida (x,y) hidato@(inicio:resto) | x>= 0 && x < (length hidato) && y>=0 && y < length inicio = True
                                     |otherwise = False

casilla_vacia :: (Int,Int) -> [[Int]] -> Bool
casilla_vacia (x_actual,y_actual) hidato = let valor = hidato!!x_actual!!y_actual
                                             in if valor == 0 then True
                                                else False
