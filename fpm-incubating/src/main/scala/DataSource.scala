package org.example.fpgrowth

import grizzled.slf4j.Logger
import org.apache.predictionio.controller.{EmptyActualResult, EmptyEvaluationInfo, PDataSource, Params}
import org.apache.predictionio.data.store.PEventStore
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

case class DataSourceParams(appName: String) extends Params

class DataSource(val dsp: DataSourceParams)
  extends PDataSource[TrainingData, EmptyEvaluationInfo, Query, EmptyActualResult] {

  @transient lazy val logger: Logger = Logger[this.type]

  override
  def readTraining(sc: SparkContext): TrainingData = {

    val eventsRDD: RDD[BuyEvent] = PEventStore.find(
      appName = dsp.appName,
      entityType = Some("user"),
      eventNames = Some(List("buy")),
      targetEntityType = Some(Some("item")))(sc)
      .map { event =>
        try {
          new BuyEvent(
            user = event.entityId,
            item = event.targetEntityId.get,
            t = event.eventTime.getMillis / 1000
          )
        } catch {
          case e: Exception => {
            logger.error(s"Cannot convert $event to BuyEvent. $e")
            throw e
          }
        }
      }.cache()

    new TrainingData(eventsRDD)
  }
}

case class BuyEvent(user: String, item: String, t: Long)

class TrainingData(val events: RDD[BuyEvent]) extends Serializable {
  override def toString: String = {
    s"events: [${events.count()}] (${events.take(2).toList}...)"
  }
}
