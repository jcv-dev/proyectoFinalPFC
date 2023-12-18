/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
  val soluciones = new soluciones()
  val tam = Math.pow(2,3).toInt
  val oraculo = new oraculo(tam)
  val s = List("A","C","G","T")
    test("Test ingenua"){
        assert(oraculo.EsLaCadena(soluciones.PRC_ingenuo(s, tam, oraculo)))
    }
    test("Test ingenua paralela"){
      assert(oraculo.EsLaCadena(soluciones.PRC_ingenuoParalelo(s, tam, oraculo)))
    }
    test("Test mejorada"){
      assert(oraculo.EsLaCadena(soluciones.PRC_mejorado(s, tam, oraculo).head))
    }
    test("Test mejorada paralela"){
      assert(oraculo.EsLaCadena(soluciones.PRC_mejoradoParalelo(s, tam, oraculo).head))
    }
    test("Test turbo solucion"){
      assert(oraculo.EsLaCadena(soluciones.PRC_TurboSolucion(s, tam, oraculo).head))
    }
    test("Test turbo solucion paralela"){
      assert(oraculo.EsLaCadena(soluciones.PRC_TurboSolucionParalelo(s, tam, oraculo).head))
    }
    test("Test turbomejorado"){
      assert(oraculo.EsLaCadena(soluciones.PRC_Turbomejorado(s, tam, oraculo).head))
    }
    test("Test turbomejorado paralelo"){
      assert(oraculo.EsLaCadena(soluciones.PRC_TurbomejoradoParalelo(s, tam, oraculo).head))
    }
}
