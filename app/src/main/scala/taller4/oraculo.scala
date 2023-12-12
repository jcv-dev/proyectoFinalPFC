package taller4

class oraculo(tam: Int){
  val n: Int = tam
  val S: String = generarCadena(n)
  def EsSubcadena(sub: String): Boolean = {
    S.contains(sub)
  }
  def EsLaCadena(cad: String): String = {
    if (S == cad) S else ""
  }
  private def generarCadena (n: Int): String={
    (for(i <- 1 to n) yield scala.util.Random.nextInt(4) match {
      case 0 => 'A'
      case 1 => 'C'
      case 2 => 'T'
      case 3 => 'G'
    }).mkString
      
  }
}
