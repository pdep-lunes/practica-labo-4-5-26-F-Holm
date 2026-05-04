module Parcial where
import Text.Show.Functions()

type Ejercicio = Perro -> Perro

data Actividad = UnaActividad {
    ejercicio :: Ejercicio,
    tiempo :: Number
} deriving (Show, Eq)

data Perro = UnPerro { 
    raza :: String,
    juguetesFavoritos :: [String],
    tiempoDePermanencia :: Number, -- minutos
    energia :: Number
} deriving (Show, Eq)

modificarEnergia :: (Number -> Number) -> Perro -> Perro
modificarEnergia unaFuncion unPerro = unPerro { energia = max 0.unaFuncion.energia $ unPerro }

jugar :: Ejercicio
jugar unPerro = modificarEnergia (substract 10) unPerro

ladrar :: Int -> Ejercicio
ladrar cantidadDeLadridos unPerro = modificarEnergia (+ (cantidadDeLadridos / 2)) unPerro

regalar :: String -> Ejercicio
regalar unJuguete unPerro = unPerro { juguetesFavoritos = juguetesFavoritos : unJuguete }

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro = raza unPerro == "dálmata" || raza unPerro == "pomerania"

diaDeSpa :: Ejercicio
diaDeSpa unPerro 
  | tiempoDePermanencia unPerro >= 50 || esDeRazaExtravagante unPerro = regalar "peine de goma" unPerro { energia = 100 }
  | otherwise = unPerro

diaDeCampo :: Ejercicio
diaDeCampo unPerro = unPerro { juguetesFavoritos = drop 1 juguetesFavoritos unPerro }

puedeEstar :: Actividad -> Perro -> Bool
puedeEstar unaActividad unPerro = tiempoDePermanencia unPerro >= tiempo unaActividad

restarTiempoDePermanencia :: Actividad -> Perro -> Perro
restarTiempoDePermanencia unaActividad unPerro = unPerro { tiempoDePermanencia = (substract tiempo unaActividad).tiempoDePermanencia $ unPerro }

realizarActividad :: Actividad -> Perro -> Perro
realizarActividad unaActividad unPerro
  | puedeEstar unaActividad unPerro = restarTiempoDePermanencia unaActividad.(ejercicio unaActividad) $ unPerro
  | otherwise = unPerro

perroZara :: Perro
perroZara = UnPerro "dálmata" ["pelota", "mantita"] 60 80

actividadJugar :: Actividad
actividadJugar = UnaActividad (jugar) 30

actividadLadrar :: Actividad
actividadLadrar = UnaActividad (ladrar 18) 20

actividadRegalar :: Actividad
actividadRegalar = UnaActividad (regalar "pelota") 0

actividadSpa :: Actividad
actividadSpa = UnaActividad (diaDeSpa) 120

actividadCampo :: Actividad
actividadCampo = UnaActividad (diaDeCampo) 720
