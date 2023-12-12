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
      println(cerradura(s, 1))
      println(oraculo.S)
      //println(PRC_ingenuo(s, 10, oraculo))
      //println(PRC_mejorado(s, 10, oraculo))
      val k = withWarmer(new Warmer.Default) measure {
        PRC_ingenuo(s, 3, oraculo)
      }
      val w = withWarmer(new Warmer.Default) measure {
        PRC_mejorado(s, 3, oraculo)
      }
      println(k)
      println(w)
        
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
  
  def PRC_mejorado(L: List[String],n: Int, ora: oraculo): String = {
    require(n > 0)
    def PRC_mejorado_aux(L: List[String],res: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(L,tam)
        val filtrado = s.filter(ora.EsSubcadena(_))
        if (tam > n) res
        else PRC_mejorado_aux(L,res++filtrado,n,ora,tam+1)
    }
    val w = PRC_mejorado_aux(L,List(),n,ora,1)
    val z = w.length - 1
    w(z)
      
  }
}
