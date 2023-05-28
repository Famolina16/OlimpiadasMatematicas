package EjercicioCasilleros

/**
 * Consigna:
 *  Se tienen 5 casilleros uno al lado del otro y se deben pintar los casilleros con los colores ROJO, VERDE y AZUL;
 *  respetando las siguientes reglas:
 *    - Solo un casillero debe ser pintado de azul
 *    - No puede haber dos casilleros adyacentes pintados de rojo
 *    - Se tienen que usar los 3 colores.
 *
 *   ¿De cuantas formas distintas se pueden pintar los casilleros?
 *
 * */

object EjercicioCasilleros extends App {

  def isBlue(s: String): Boolean = s.equals("BLUE")
  def isOneBlue(c: Seq[String]): Boolean = c.count(isBlue) == 1

  def containsRequiredColors(s: Seq[String], requiredColors: Seq[String]): Boolean =
    requiredColors.forall(s.contains)

  def noAdjacentRed(s: Seq[String]) : Boolean =
    !s.sliding(2).exists(_.containsSlice(Seq("RED", "RED")))

  val baseElements1: Seq[String] = Seq("BLUE","RED","RED","RED","GREEN")

  val baseElements2: Seq[String] = Seq("BLUE","RED","RED","GREEN","GREEN")

  val baseElements3: Seq[String] = Seq("BLUE","RED","GREEN","GREEN","GREEN")

  val combinations1: Seq[Seq[String]] = baseElements1.permutations.toSeq
  val combinations2: Seq[Seq[String]] = baseElements2.permutations.toSeq
  val combinations3: Seq[Seq[String]] = baseElements3.permutations.toSeq

  val allCombinations = combinations1 ++ combinations2 ++ combinations3

  val allFilteredSeq =
    allCombinations
      .filter(noAdjacentRed)
      .filter(isOneBlue)
      .filter(containsRequiredColors(_, Seq("RED", "GREEN", "BLUE")))

  allFilteredSeq.foreach(println)

  println(s"Cantidad de soluciones posibles: ${allFilteredSeq.size}")

}


