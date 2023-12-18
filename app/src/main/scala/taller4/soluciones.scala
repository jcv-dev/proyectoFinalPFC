package taller4

import common._
import scala.annotation.tailrec

class soluciones() {
  def PRC_ingenuo(L: List[String],n: Int,ora: oraculo): String = {
    require(n > 0)
    val res = cerraduraCompleta(L,n).filter(ora.EsLaCadena(_))
    res.mkString
  }
  
  def PRC_ingenuoParalelo(L: List[String],n: Int,ora: oraculo): String = {
    require(n > 0)
    val s = cerraduraCompleta(L,n)
    val size = s.length / 4
    val (s1, rest1) = s.splitAt(size)
    val (s2, rest2) = rest1.splitAt(size)
    val (s3, s4) = rest2.splitAt(size)
    val filter = parallel(s1.filter(ora.EsLaCadena(_)),s2.filter(ora.EsLaCadena(_)),s3.filter(ora.EsLaCadena(_)),s4.filter(ora.EsLaCadena(_)))
    val res = filter._1 ++ filter._2 ++ filter._3 ++ filter._4
    res.mkString
  }

  def PRC_mejorado(L: List[String],n: Int, ora: oraculo): List[String] = {
    require(n > 0)
    def PRC_mejorado_aux(L: List[String],res: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(L,tam)
        val filtrado = s.filter(ora.EsSubcadena(_))
        if (tam >= n || (!res.isEmpty && res.head.length==n)) filtrado
        else PRC_mejorado_aux(L,filtrado,n,ora,tam+1)
    }
    PRC_mejorado_aux(L,Nil,n,ora,1)
  }  
  def PRC_mejoradoParalelo(L: List[String],n: Int, ora: oraculo): List[String] = {
    require(n > 0)
    def PRC_mejorado_aux(L: List[String],res: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(L,tam)
        val size = s.length / 4
        val (s1, rest1) = s.splitAt(size)
        val (s2, rest2) = rest1.splitAt(size)
        val (s3, s4) = rest2.splitAt(size)
        val res = parallel(s1.filter(ora.EsSubcadena(_)),s2.filter(ora.EsSubcadena(_)),s3.filter(ora.EsSubcadena(_)),s4.filter(ora.EsSubcadena(_)))
        val filtrado = res._1 ++ res._2 ++ res._3 ++ res._4
        if (tam >= n || (!filtrado.isEmpty && filtrado.head.length==n)) filtrado
        else PRC_mejorado_aux(L,filtrado,n,ora,tam+1)
    }
    PRC_mejorado_aux(L,Nil,n,ora,1)
  }

  def PRC_TurboSolucion(L: List[String],n: Int, ora: oraculo): List[String] = {
    require(n > 0)
    def PRC_mejorado_aux(listaFil: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(listaFil,2)
        val filtrado = s.filter(ora.EsSubcadena(_))
        if ((!listaFil.isEmpty && listaFil.head.length==n)) listaFil
        else PRC_mejorado_aux(filtrado,n,ora,tam+2)
    }
    PRC_mejorado_aux(L.filter(ora.EsSubcadena(_)),n,ora,2)
  }

  def PRC_TurboSolucionParalelo(L: List[String],n: Int, ora: oraculo): List[String] = {
    require(n > 0)
    def PRC_mejorado_aux(listaFil: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(listaFil,2)
        val size = s.length / 4
        val (s1, rest1) = s.splitAt(size)
        val (s2, rest2) = rest1.splitAt(size)
        val (s3, s4) = rest2.splitAt(size)
        val res = parallel(s1.filter(ora.EsSubcadena(_)),s2.filter(ora.EsSubcadena(_)),s3.filter(ora.EsSubcadena(_)),s4.filter(ora.EsSubcadena(_)))
        val filtrado = res._1 ++ res._2 ++ res._3 ++ res._4
        if ((!listaFil.isEmpty && listaFil.head.length==n)) listaFil
        else PRC_mejorado_aux(filtrado,n,ora,tam+2)
    }
    PRC_mejorado_aux(L.filter(ora.EsSubcadena(_)),n,ora,2)
  }

  def PRC_Turbomejorado(L: List[String], n: Int, ora: oraculo): List[String] = {
  require(n > 0)
    def filtrar(listaAnterior: List[String], Elemento: String): Boolean = {
      val z = Elemento.sliding(Elemento.length/2).toList
      z.forall(listaAnterior.contains)
    }
    def PRC_mejorado_aux(listaFil: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(listaFil,2)
        val filtro: List[String] = if (tam < 4) s else s.filter(filtrar(listaFil,_))
        val filtrado = filtro.filter(ora.EsSubcadena(_))
        if ((!listaFil.isEmpty && listaFil.head.length==n)) listaFil
        else PRC_mejorado_aux(filtrado,n,ora,tam+2) 
    }
    PRC_mejorado_aux(L.filter(ora.EsSubcadena(_)),n,ora,2)
   }

  def PRC_TurbomejoradoParalelo(L: List[String], n: Int, ora: oraculo): List[String] = {
  require(n > 0)
    def filtrar(listaAnterior: List[String], Elemento: String): Boolean = {
      val z = Elemento.sliding(Elemento.length/2).toList
      z.forall(listaAnterior.contains)
    }
    def PRC_mejorado_aux(listaFil: List[String],n: Int, ora: oraculo, tam: Int): List[String] = {
        val s = cerradura(listaFil,2)
        val filtro: List[String] =
          if (tam == 2) s
          else {
            val size = s.length / 4
            val (s1, rest1) = s.splitAt(size)
            val (s2, rest2) = rest1.splitAt(size)
            val (s3, s4) = rest2.splitAt(size)
            val temp = parallel(s1.filter(filtrar(listaFil,_)),s2.filter(filtrar(listaFil,_)),s3.filter(filtrar(listaFil,_)),s4.filter(filtrar(listaFil,_)))
            temp._1 ++ temp._2 ++ temp._3 ++ temp._4
          }
        val size = filtro.length / 4
        val (s1, rest1) = filtro.splitAt(size)
        val (s2, rest2) = rest1.splitAt(size)
        val (s3, s4) = rest2.splitAt(size)
        val res = parallel(s1.filter(ora.EsSubcadena(_)),s2.filter(ora.EsSubcadena(_)),s3.filter(ora.EsSubcadena(_)),s4.filter(ora.EsSubcadena(_)))
        val filtrado = res._1 ++ res._2 ++ res._3 ++ res._4
        if ((!listaFil.isEmpty && listaFil.head.length==n)) listaFil
        else PRC_mejorado_aux(filtrado.toList,n,ora,tam+2) 
    }
    PRC_mejorado_aux(L.filter(ora.EsSubcadena(_)),n,ora,2)
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
      else generarCerraduraRec(c.flatMap(s => l.map(s + _)), acc ++ c, m + 1, n)
    }
    generarCerraduraRec(l, List.empty, 0, n)
  }
}
