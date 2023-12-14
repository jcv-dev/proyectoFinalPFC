/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import common._
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.annotation.tailrec

object Taller4{

  def main(args: Array[String]): Unit = {
      
      val s: List[String] = List("A","C","T","G")
        val n = 8
      val oraculo = new oraculo(n)
     
      println(oraculo.S)
        println(PRC_Turbomejorado(s, n,oraculo))
      //println(PRC_ingenuo(s, n, oraculo))
      //println(PRC_ingenuoParalelo(s, n, oraculo))
      //println(PRC_mejorado(s, n, oraculo))
      //println(PRC_TurboSolucion(s, n, oraculo))
      val k = withWarmer(new Warmer.Default) measure {
        PRC_ingenuo(s, n, oraculo)
      }
      val a = withWarmer(new Warmer.Default) measure {
        PRC_ingenuoParalelo(s, n, oraculo)
      }
      val h = withWarmer(new Warmer.Default) measure {
        PRC_mejorado(s, n, oraculo)
      }
      val z = withWarmer(new Warmer.Default) measure {
        PRC_TurboSolucion(s, n, oraculo)
      }
      val w = withWarmer(new Warmer.Default) measure {
        PRC_Turbomejorado(s, n,oraculo)
      }
      println("Solución turboMejorada: " + w)
      println("Solución ingenua: " + k)
      println("solución mejorada: " + h)
      println("Solución Turbosolución: "+ z)
      println("Solución ingenua paralela: "+ a)
        
  }

  def cerradura(l: List[String], n: Int): List[String] = {
    require(n >= 0)
    @tailrec
    def concatenarRec(c: List[String], m: Int, acc: List[String]): List[String] = {
      if (m == 0) acc
      else concatenarRec(c, m - 1, for { cadena <- c; resto <- acc } yield cadena + resto)
    }

    concatenarRec(l, n, List(""))
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

  def PRC_ingenuoParalelo(L: List[String],n: Int,ora: oraculo): String = {
    require(n > 0)
    val s = cerraduraCompleta(L,n)
    val s1 = s.drop(s.length/2)
    val s2 = s.take(s.length/2)
    val (res1,res2) = parallel(s1.filter(ora.EsLaCadena(_)),s2.filter(ora.EsLaCadena(_)))
    val res = res1:::res2
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
  def PRC_TurboSolucion(L: List[String],n: Int, ora: oraculo): String = {
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

 
 
  def PRC_Turbomejorado(L: List[String], n: Int, ora: oraculo): String = {
  require(n > 0)
    def PRC_mejorado_aux(L: List[String],res: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(L,tam)
        val filtrado = s.filter(ora.EsSubcadena(_))
        if (tam > n) res
        else {
          val h = (res++filtrado).filter(ora.EsSubcadena(_))
          PRC_mejorado_aux(L,h,n,ora,tam+2)
        }
    }
    val w = PRC_mejorado_aux(L.filter(ora.EsSubcadena(_)),List(),n,ora,2)
    val z = w.length - 1
    w(z) 
}



}
