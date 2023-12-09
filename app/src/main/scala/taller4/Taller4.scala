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
      println(PRC_ingenuo(s,13,oraculo))
  }

  def oraculo(sub: String,cad: String): Boolean = {
    cad.contains(sub)
  }

  def cerradura(lenguaje: List[String], n: Int): List[String] = {
    require(n >= 0)


    def concatenar(c: List[String], m: Int): List[String] = {
      if (m == 0) List("") 
      else
        for {
          cadena <- c
          resto <- concatenar(c, m - 1)
        } yield cadena + resto
    }

    concatenar(lenguaje, n)
  }

   def PRC_ingenuo(L: List[String],n: Int,f:(String,String) => Boolean): String = {

    var s = ""
    for (i <- cerradura(L,n)){
      if (f(i,"ATTTGATGACTAG")) s += i
    }
    s
  }
}
