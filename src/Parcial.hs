module Parcial where
import Text.Show.Functions()

type Ejercicio = Perro -> Perro

data Actividad = UnaActividad {
    ejercicio :: Ejercicio,
    tiempo :: Number
}

data Perro = UnPerro { 
    raza :: String,
    juguetesFavoritos :: [String],
    tiempoDePermanencia :: Number, -- minutos
    energia :: Number
} deriving (Show, Eq)

type Rutina = [Actividad]

modificarEnergia :: (Number -> Number) -> Perro -> Perro
modificarEnergia unaFuncion unPerro = unPerro { energia = max 0.unaFuncion.energia $ unPerro }

jugar :: Ejercicio
jugar unPerro = modificarEnergia (subtract 10) unPerro

ladrar :: Number -> Ejercicio
ladrar cantidadDeLadridos unPerro = modificarEnergia (+ (cantidadDeLadridos / 2)) unPerro

regalar :: String -> Ejercicio
regalar unJuguete unPerro = unPerro { juguetesFavoritos = unJuguete : juguetesFavoritos unPerro }

esDeRaza :: Perro -> String -> Bool
esDeRaza unPerro unaRaza = raza unPerro == unaRaza

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro = esDeRaza unPerro "dálmata" || esDeRaza unPerro "pomerania"

diaDeSpa :: Ejercicio
diaDeSpa unPerro 
  | tiempoDePermanencia unPerro >= 50 || esDeRazaExtravagante unPerro = regalar "peine de goma" unPerro { energia = 100 }
  | otherwise = unPerro

diaDeCampo :: Ejercicio
diaDeCampo unPerro = unPerro { juguetesFavoritos = drop 1.juguetesFavoritos $ unPerro }

puedeEstar :: Actividad -> Perro -> Bool
puedeEstar unaActividad unPerro = tiempoDePermanencia unPerro >= tiempo unaActividad

realizarActividad :: Perro -> Actividad -> Perro
realizarActividad unPerro unaActividad = ejercicio unaActividad unPerro

tiempoRutina :: Rutina -> Number
tiempoRutina unaRutina = sum.map tiempo $ unaRutina

puedeHacerRutina :: Rutina -> Perro -> Bool
puedeHacerRutina unaRutina unPerro = tiempoDePermanencia unPerro >= tiempoRutina unaRutina

realizarRutina :: Rutina -> Perro -> Perro
realizarRutina unaRutina unPerro
  | puedeHacerRutina unaRutina unPerro = foldl (realizarActividad) unPerro unaRutina
  | otherwise = unPerro

perroZara :: Perro
perroZara = UnPerro "dálmata" ["pelota", "mantita"] 90 80

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

rutina :: Rutina
rutina = [actividadJugar, actividadLadrar, actividadRegalar, actividadSpa, actividadCampo]
