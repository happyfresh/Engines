package org.example.fpgrowth

import grizzled.slf4j.Logger
import org.apache.predictionio.controller.{P2LAlgorithm, Params}
import org.apache.spark.SparkContext
import org.apache.spark.mllib.fpm.FPGrowth

case class AlgorithmParams(minSupport: Double, minConfidence: Double) extends Params

class Algorithm(val ap: AlgorithmParams)
  extends P2LAlgorithm[PreparedData, FPGModel, Query, PredictedResult] {

  @transient lazy val logger: Logger = Logger[this.type]

  def train(sc: SparkContext, data: PreparedData): FPGModel = {

    val transactions = data.events
      .map(event => ((event.user, event.t), event.item))
      .groupByKey()
      .map(_._2.toArray).cache()

    val fpg = new FPGrowth().setMinSupport(ap.minSupport)
    val model = fpg.run(transactions)

    val resultList = model.generateAssociationRules(ap.minConfidence)
      .map(rule => (rule.antecedent.mkString(" "), rule.consequent, rule.confidence))
      .collect.toList

    new FPGModel(resultList)
  }

  def predict(model: FPGModel, query: Query): PredictedResult = {
    val items = query.items.toList.sorted.mkString(" ")
    val result = model.resultList
      .filter(x => {
        x._1 == items
      })
      .sortBy(_._3)
      .map(x => {
        new ConsequentItem(x._2, x._3)
      })

    PredictedResult(result.toArray)
  }
}

class FPGModel(val resultList: List[(String, Array[String], Double)]) extends Serializable {}