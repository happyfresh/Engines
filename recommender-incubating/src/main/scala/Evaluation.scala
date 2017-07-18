package org.example.recommendation

import org.apache.predictionio.controller.Evaluation
import org.apache.predictionio.controller.OptionAverageMetric
import org.apache.predictionio.controller.AverageMetric
import org.apache.predictionio.controller.EmptyEvaluationInfo
import org.apache.predictionio.controller.EngineParamsGenerator
import org.apache.predictionio.controller.EngineParams
import org.apache.predictionio.controller.MetricEvaluator

// Usage:
// $ pio eval org.example.recommendation.RecommendationEvaluation \
//   org.example.recommendation.EngineParamsList

case class PrecisionAtK(k: Int)
    extends OptionAverageMetric[EmptyEvaluationInfo, Query, PredictedResult, ActualResult] {
  require(k > 0, "k must be greater than 0")

  override def header = s"Precision@K (k=$k)"

  def calculate(q: Query, p: PredictedResult, a: ActualResult): Option[Double] = {
    val positives: Set[String] = a.ratings.map(_.item).toSet

    // If there is no positive results, Precision is undefined. We don't consider this case in the
    // metrics, hence we return None.
    if (positives.size == 0) {
      None
    }
    val tpCount: Int = p.itemScores.take(k).filter(is => positives(is.item)).size
    Some(tpCount.toDouble / math.min(k, positives.size))
  }
}

case class PositiveCount()
    extends AverageMetric[EmptyEvaluationInfo, Query, PredictedResult, ActualResult] {
  override def header = s"PositiveCount"

  def calculate(q: Query, p: PredictedResult, a: ActualResult): Double = {
    a.ratings.size
  }
}

object RecommendationEvaluation extends Evaluation {
  engineEvaluator = (
    RecommendationEngine(),
    MetricEvaluator(
      metric = PrecisionAtK(k = 10, ratingThreshold = 4.0),
      otherMetrics = Seq(
        PositiveCount(),
      )))
}


object ComprehensiveRecommendationEvaluation extends Evaluation {
  val ratingThresholds = Seq(0)
  val ks = Seq(1, 3, 10)

  engineEvaluator = (
    RecommendationEngine(),
    MetricEvaluator(
      metric = PrecisionAtK(k = 3),
      otherMetrics = (
        (for (r <- ratingThresholds) yield PositiveCount()) ++
        (for (r <- ratingThresholds; k <- ks) yield PrecisionAtK(k = k))
      )))
}


trait BaseEngineParamsList extends EngineParamsGenerator {
  protected val baseEP = EngineParams(
    dataSourceParams = DataSourceParams(
      appName = "RecommenderStaging",
      evalParams = Some(DataSourceEvalParams(kFold = 5, queryNum = 10))))
}

object EngineParamsList extends BaseEngineParamsList {
  engineParamsList = for(
    rank <- Seq(5, 10, 20);
    numIterations <- Seq(1, 5, 10))
    yield baseEP.copy(
      algorithmParamsList = Seq(
        ("als", ALSAlgorithmParams(rank, numIterations, 0.01, Some(3)))))
}
