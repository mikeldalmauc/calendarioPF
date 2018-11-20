module Hoja6 where 
    
-----------------------------------------------------------------------------------------
---  EJERCICIO 1
-----------------------------------------------------------------------------------------
    class FiguraPlana a where 
        perimetro :: a -> Double
        area :: a -> Double
        
    type Radio = Double 
    type Lado = Double
    type Altura = Double 
    type Base = Double 

    newtype Circulo = Cir Radio
    data TrianguloRect = Tri Base Altura -- Isosceles o equilatero
    data Cuadrilatero = Cuad Lado | Rect Base Altura
    
    instance FiguraPlana Circulo where
        perimetro (Cir r) = 2 * r * pi
        area (Cir r) = pi * r * r

    instance FiguraPlana TrianguloRect where
        perimetro (Tri base altura) = 2.0 * sqrt (altura^2 +(base/2.0)^2) + base 
        area (Tri base altura) = base * altura / 2.0

    instance FiguraPlana Cuadrilatero where
        perimetro (Cuad lado) = lado * 4
        perimetro ( Rect base altura) = base * 2 + altura * 2
        area (Cuad lado) = lado * lado 
        area (Rect base altura) = base * altura 
-----------------------------------------------------------------------------------------
---  EJERCICIO 2
-----------------------------------------------------------------------------------------
    
    data CatLista a = Nil | Unit a | Conc (CatLista a) (CatLista a) 

    instance (Eq a) => Eq (CatLista a) where
        l1 == l2  = aplanarCatLista l1 == aplanarCatLista l2

    aplanarCatLista :: CatLista a -> [a]
    aplanarCatLista Nil         = []
    aplanarCatLista (Unit u)      = [u]
    aplanarCatLista (Conc iz de) = aplanarCatLista iz ++ aplanarCatLista de

    primero :: CatLista a -> Maybe a
    primero Nil          = Nothing
    primero (Unit u)     = Just u
    primero (Conc iz de) = 
                case pIz of
                    Nothing -> primero de
                    _  -> pIz
                where pIz = primero iz
