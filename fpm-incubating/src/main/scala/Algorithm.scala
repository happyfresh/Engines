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
      .map(_._2.toSet.toArray).cache()

    val fpg = new FPGrowth().setMinSupport(ap.minSupport).setNumPartitions(8)
    val model = fpg.run(transactions)

    val resultList = model.generateAssociationRules(ap.minConfidence)
      .map(rule => (rule.antecedent.sorted.mkString(" "), rule.consequent, rule.confidence))
      .collect.toList

    new FPGModel(resultList)
  }

  def predict(model: FPGModel, query: Query): PredictedResult = {
    val variantIds = query.items.toList.sorted
    val keys = (1 to 3).map(i => {
      variantIds.combinations(i).toList.map(_.sorted.mkString(" "))
    }).flatten

    val result = model.resultList
      .filter(item => keys.contains(item._1))
      .map(item => new ConsequentItem(item._2, item._3))
      .groupBy(_.items.mkString).values.map(_.maxBy(_.confidence))
      .toList.sortBy(-_.confidence)
      .take(query.num)

    PredictedResult(result.toArray)
  }
}

class FPGModel(val resultList: List[(String, Array[String], Double)]) extends Serializable {}
