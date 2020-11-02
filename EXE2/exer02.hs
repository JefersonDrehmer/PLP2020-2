data Carro = Modelo String
            |CorLinha String String
            |Comprador Pessoa Bool
            deriving Show

data Pessoa = Pessoa String String Genero
            deriving Show            

data Genero = Masculino | Feminino | Outro
            deriving Show   

nomeComprador :: Carro -> String
nomeComprador c = case c of
                     Modelo mod                     -> mod 
                     CorLinha cor li                -> cor ++ " " ++ li
                     Comprador p quitado                   ->
                        case p of
                            Pessoa pnome snome genero          -> pnome ++ " " ++ snome  

nomeComprador' :: Carro -> String
nomeComprador'  (Modelo mod)                    = mod 
nomeComprador'  (CorLinha cor li)              = cor ++ " " ++ li
nomeComprador'  (Comprador(Pessoa pnome snome _) quitado)    =  pnome ++ " " ++ snome