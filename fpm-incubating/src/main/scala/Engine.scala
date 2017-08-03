package org.example.fpgrowth

import org.apache.predictionio.controller.{Engine, EngineFactory}

case class Query(items: Set[String], num: Int) extends Serializable

case class PredictedResult(consequentItems: Array[ConsequentItem]) extends Serializable

case class ConsequentItem(items: Array[String], confidence: Double) extends Serializable

object FPGrowthEngine extends EngineFactory {
  def apply() = {
    new Engine(
      classOf[DataSource],
      classOf[Preparator],
      Map("algo" -> classOf[Algorithm]),
      classOf[Serving])
  }
}
