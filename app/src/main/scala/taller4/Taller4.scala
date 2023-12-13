/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.annotation.tailrec

object Taller4{

  def main(args: Array[String]): Unit = {
      
      val s: List[String] = List("A","C","T","G")
      val oraculo = new oraculo(16)
      println(cerradura(s, 1))
      println(oraculo.S)
      //println(PRC_ingenuo(s, 8, oraculo))
     // println(PRC_mejorado(s, 16, oraculo))
      println(PRC_Turbomejorado(s, 16, oraculo))
      //val k = withWarmer(new Warmer.Default) measure {
        //PRC_ingenuo(s, 10, oraculo)
      //}
      //val w = withWarmer(new Warmer.Default) measure {
       // PRC_mejorado(s, 16, oraculo)
      //}
      val z = withWarmer(new Warmer.Default) measure {
        PRC_Turbomejorado(s, 16, oraculo)
      }
      //println(k)
      //println(w)
      println(z)
        
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
  def cerraduraCompleta(l: List[String], n: Int): List[String] = {
    @tailrec
    def generarCerraduraRec(c: List[String], acc: List[String], m: Int, n: Int): List[String] = {
      if (m > n-1) acc
      else generarCerraduraRec(c.flatMap(s => l.map(s + _)), acc ::: c, m + 1, n)
    }
    generarCerraduraRec(l, List.empty, 0, n)
  }


  def PRC_ingenuo(L: List[String],n: Int,ora: oraculo): String = {
    require(n > 0)
    val s = cerraduraCompleta(L,n)
    val res = s.filter(ora.EsLaCadena(_) )
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
    val w = PRC_mejorado_aux(L,List(),n,ora,0)
    val z = w.length - 1
    w(z) 
  }
  def PRC_Turbomejorado(L: List[String],n: Int, ora: oraculo): String = {
    require(n > 0)
    def PRC_mejorado_aux(L: List[String],res: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(L,tam)
        val filtrado = s.filter(ora.EsSubcadena(_))
        if (tam > n) res
        else PRC_mejorado_aux(L,res++filtrado,n,ora,tam+2)
    }
    val w = PRC_mejorado_aux(L.filter(ora.EsSubcadena(_)),List(),n,ora,2)
    val z = w.length - 1
    w(z) 
  }

}
