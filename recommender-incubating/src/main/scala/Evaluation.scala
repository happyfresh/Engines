package org.example.recommendation

import org.apache.predictionio.controller._

// Usage:
// $ pio eval org.example.recommendation.RecommendationEvaluation \
//   org.example.recommendation.EngineParamsList

case class PrecisionAtK(k: Int, ratingThreshold: Double = 0.1) extends OptionAverageMetric[EmptyEvaluationInfo, Query, PredictedResult, ActualResult] {
  require(k > 0, "k must be greater than 0")

  override def header = s"Precision@K (k=$k, threshold=$ratingThreshold)"

  def calculate(q: Query, p: PredictedResult, a: ActualResult): Option[Double] = {
    val positives: Set[String] = a.ratings.filter(_.rating >= ratingThreshold).map(_.item).toSet

    // If there is no positive results, Precision is undefined. We don't consider this case in the
    // metrics, hence we return None.
    if (positives.isEmpty) {
      None
    }
    val tpCount: Int = p.itemScores.take(k).count(is => positives(is.item))
    Some(tpCount.toDouble / math.min(k, positives.size))
  }
}

object RecommendationEvaluation extends Evaluation {
  engineEvaluator = (
    RecommendationEngine(),
    MetricEvaluator(metric = PrecisionAtK(k = 10))
  )
}

trait BaseEngineParamsList extends EngineParamsGenerator {
  protected val baseEP = EngineParams(
    dataSourceParams = DataSourceParams(
      appName = "RecommenderStaging",
      evalParams = Some(DataSourceEvalParams(kFold = 5, queryNum = 10))))
}

object EngineParamsList extends BaseEngineParamsList {
  engineParamsList = for (
    rank <- Seq(10);
    numIterations <- Seq(20))
    yield baseEP.copy(
      algorithmParamsList = Seq(
        ("als", ALSAlgorithmParams(rank, numIterations, 0.01, Some(3)))))
}
