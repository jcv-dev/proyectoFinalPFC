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
      
      val s: LazyList[String] = LazyList("A","C","T","G")
      //println(PRC_ingenuo(s,13,oraculo))
      val w = PRC_ingenuo(s,3,oraculo)
      for (z <- w) println(z)
        
    
  }

  def oraculo(sub: String,cad: String): Boolean = {
    cad.contains(sub)
  }

  def cerradura(l: LazyList[String], n: Int): LazyList[String] = {
    require(n >= 0)
    def generarCerraduraKleene(c: LazyList[String], m: Int): LazyList[String] = {
      if (m > n) LazyList.empty
      else c #::: generarCerraduraKleene(c.flatMap(c => c.map(c + _)), m + 1)
    }

    generarCerraduraKleene(l, 1)
  }


  def PRC_ingenuo(L: LazyList[String],n: Int,f:(String,String) => Boolean): LazyList[String] = {
    require(n > 0)
    val s = cerradura(L,n)
    val g = s.takeWhile(f(_,"AAA"))
    g
  }
}
