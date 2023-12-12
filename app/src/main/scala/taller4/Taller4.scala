/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4{

  def main(args: Array[String]): Unit = {
      
      val s: List[String] = List("A","C","T","G")
      val oraculo = new oraculo(3)
      println(oraculo.S)
      println(PRC_ingenuo(s, 3, oraculo))
     // println(cerradura(s, 3))
  }

  def cerradura(l: List[String], n: Int): List[String] = {
    require(n >= 0)
    def concatenar(c: List[String], m: Int): List[String] = {
      if (m == 0) List("") 
      else
        for {
          cadena <- c
          resto <- concatenar(c, m - 1)
        } yield cadena + resto
    }

    concatenar(l, n)
  }


  def PRC_ingenuo(L: List[String],n: Int,ora: oraculo): String = {
    require(n > 0)
    val s = cerradura(L,n)
    val res = s.filter(ora.EsSubcadena(_))
    res.mkString
  }
}
