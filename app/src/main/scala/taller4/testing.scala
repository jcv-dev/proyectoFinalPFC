package taller4
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import org.scalameter.Quantity


class testing(n:Int){
  private val s = List("A","C","G","T")
  private val tam = Math.pow(2,n).toInt
  private val soluciones = new soluciones()
  private val oraculo = new oraculo(tam)
  def pruebaIngenua(): Quantity[Double] = {
      val time = withWarmer(new Warmer.Default) measure {
      soluciones.PRC_ingenuo(s, tam, oraculo)
    }
    time
  }
  def puebaIngenuaParalelo(): Quantity[Double] = {
      val time = withWarmer(new Warmer.Default) measure {
      soluciones.PRC_ingenuoParalelo(s, tam, oraculo)
    }
    time
  }
  def pruebaMejorado():Quantity[Double]= {
     val time = withWarmer(new Warmer.Default) measure {
     soluciones.PRC_mejorado(s, tam, oraculo)
    }
    time
  }
  def pruebaMejoradoParalelo():Quantity[Double] = {
     val time = withWarmer(new Warmer.Default) measure {
     soluciones.PRC_mejoradoParalelo(s, tam, oraculo)
    }
    time
  }
  def pruebaTurboSolucion():Quantity[Double] = {
     val time = withWarmer(new Warmer.Default) measure {
     soluciones.PRC_TurboSolucion(s, tam, oraculo)
    }
    time
  }
  def pruebaTurboSolucionParalelo():Quantity[Double]= {
     val time = withWarmer(new Warmer.Default) measure {
     soluciones.PRC_TurboSolucionParalelo(s, tam, oraculo)
    }
    time
  }
  def pruebaTurbomejorado():Quantity[Double] = {
     val time = withWarmer(new Warmer.Default) measure {
      soluciones.PRC_Turbomejorado(s, tam, oraculo)
    }
    time
  }
  def pruebaTurbomejoradoParalelo():Quantity[Double] = {
     val time = withWarmer(new Warmer.Default) measure {
     soluciones.PRC_TurbomejoradoParalelo(s, tam, oraculo)
    }
    time
  }
}
