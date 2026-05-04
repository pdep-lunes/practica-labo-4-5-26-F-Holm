import Test.Hspec
import Parcial

main :: IO ()
main = hspec $ do

  describe "Tests de Perros - Actividades" $ do

    describe "jugar" $ do
      it "Disminuye la energía del perro en 10" $ do
        energia (jugar perroZara) `shouldBe` 70
      it "La energía no baja de 0" $ do
        let perroCansado = perroZara { energia = 5 }
        energia (jugar perroCansado) `shouldBe` 0

    describe "ladrar" $ do
      it "Aumenta la energía la mitad de los ladridos (18 ladridos -> +9 energía)" $ do
        energia (ladrar 18 perroZara) `shouldBe` 89

    describe "regalar" $ do
      it "Agrega un nuevo juguete a la lista del perro" $ do
        juguetesFavoritos (regalar "hueso" perroZara) `shouldBe` ["hueso", "pelota", "mantita"]

    describe "diaDeSpa" $ do
      it "Si es de raza extravagante, sube energía a 100 y gana un peine de goma" $ do
        let perroSpa = diaDeSpa perroZara
        energia perroSpa `shouldBe` 100
        juguetesFavoritos perroSpa `shouldContain` ["peine de goma"]
      it "Si no cumple condiciones, el perro se queda igual" $ do
        let perroComun = UnPerro "Ovejero" ["soga"] 10 50
        diaDeSpa perroComun `shouldBe` perroComun

    describe "diaDeCampo" $ do
      it "Elimina el primer juguete de la lista" $ do
        juguetesFavoritos (diaDeCampo perroZara) `shouldBe` ["mantita"]

  describe "Tests de Rutinas y Lógica" $ do

    describe "puedeHacerRutina" $ do
      it "Un perro con mucho tiempo puede hacer una rutina corta" $ do
        let rutinaCorta = [actividadJugar, actividadLadrar] -- 30 + 20 = 50 min
        puedeHacerRutina rutinaCorta perroZara `shouldBe` True
      it "Un perro no puede hacer una rutina que excede su tiempo de permanencia" $ do
        let rutinaLarga = [actividadCampo] -- 720 min
        puedeHacerRutina rutinaLarga perroZara `shouldBe` False

    describe "realizarRutina" $ do
      it "Al realizar una rutina válida, se aplican todos los efectos en orden" $ do
        -- Rutina: Jugar (-10) y Ladrar 18 (+9). Energía final: 80 - 10 + 9 = 79
        let rutinaMix = [actividadJugar, actividadLadrar]
        energia (realizarRutina rutinaMix perroZara) `shouldBe` 79
      it "Si no puede hacer la rutina, el perro queda intacto" $ do
        let rutinaImposible = [actividadCampo]
        realizarRutina rutinaImposible perroZara `shouldBe` perroZara
