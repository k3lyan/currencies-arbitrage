import $ivy.`io.circe::circe-parser:0.14.1`

import Controller.{findNegativeCycles, getRates, transformToExchanges}
import Model.ExchangeGraph.buildFromExchanges
import Model.{Currency, Cycle, Exchange, ExchangeGraph}
import View.{decodeRawData, getRawData, getResult, verifyRatePositivity}

import cats.syntax.either._
import cats.syntax.option._
import io.circe.parser.decode


import scala.annotation.tailrec
import scala.util.Try



sealed trait InvalidData {
  val errorMessage: String
}
case object NoArbitrageFound extends InvalidData {
  override val errorMessage: String = "No arbitrage loop have been found"
}
case class InvalidRequest(message: String) extends InvalidData {
  override val errorMessage: String = s"Could not get the data requested correctly: $message."
}
case class InvalidResponse(message: String) extends InvalidData {
  override val errorMessage: String = s"Could not decode correctly the content of the request response: $message."
}
case object InvalidRateValue extends InvalidData {
  override val errorMessage: String = s"Rates values should be positive double."
}
case class InvalidTransform(message: String) extends InvalidData {
  override val errorMessage: String = s"Could not transform correctly the extracted data to the exchanges: $message."
}
case class InvalidGraphConstruction(message: String) extends InvalidData {
  override val errorMessage: String = s"An error occurred while building the Exchange graph from the exchanges values: $message"
}
type ValidateData[A] = Either[InvalidData, A]

object View {

  def getRawData(url: String): ValidateData[String] =
    Try(requests.get(url).text()).toEither.leftMap(e => InvalidRequest(e.getMessage))

  def decodeRawData(data: String): ValidateData[Map[String, String]] =
    decode[Map[String, String]](data).leftMap(e => InvalidResponse(e.getMessage))

  def verifyRatePositivity(data: Map[String, String]): ValidateData[Map[String, String]] =
    Either.cond(
      !data.values.exists(r => r.toDouble <= 0),
      data,
      InvalidRateValue
    )

  def getResult(arbitrages: Set[Cycle], exchanges: Set[Exchange]): ValidateData[Set[String]] = {
    Either.cond(
      arbitrages.nonEmpty,
      arbitrages.map {
        cycle =>
          val rates = getRates(cycle.currencies, exchanges)
          s"""----- Arbitrage -----
                          | ${(cycle.currencies :+ cycle.currencies.head).mkString(" -> ")}
                          | Profits: ${rates.mkString(" * ")} = ${rates.product} => + ${(rates.product - 1.0) * 100.0} %
          """.stripMargin
      },
      NoArbitrageFound
    )
  }
}

object Controller {

  def convert(swap: String): Map[String, String] = (Array("from", "to") zip swap.split("_")).toMap

  def transformToExchanges(rates: Map[String, String]): ValidateData[Set[Exchange]] = Try {
    rates
      .foldLeft(Set.empty[Exchange])((exchanges: Set[Exchange], rate: (String, String)) => {
        val nodes: Map[String, String] = convert(rate._1)
        exchanges + Exchange(Currency(nodes("from")), Currency(nodes("to")), rate._2.toDouble)
      })}
    .toEither
    .leftMap(e => InvalidTransform(e.getMessage))

  def getRates(currencies: Vector[Currency], exchanges: Set[Exchange]): Vector[Double] =
    (currencies.tail :+ currencies.head)
      .foldLeft(currencies.head, Vector.empty[Double])((ratesBeforeNextNode: (Currency, Vector[Double]), nextNode: Currency) => {
        (nextNode, ratesBeforeNextNode._2 :+ exchanges.filter(exch => exch.source == ratesBeforeNextNode._1 && exch.destination == nextNode).head.rate)
      })._2

  type Distances = Map[Currency, Double]
  type Predecessors = Map[Currency, Currency]

  def init(source: Currency): (Distances, Predecessors) =
    (
      Map.empty[Currency, Double].withDefaultValue(Double.PositiveInfinity) + (source -> 0.0),
      Map.empty[Currency, Currency]
    )

  @tailrec
  def relaxation(i: Int,
                 exchanges: Set[Exchange],
                 distances: Distances,
                 predecessors: Predecessors): (Distances, Predecessors) = {
    if (i <= 0) (distances, predecessors)
    else {
      val (updatedDistances, updatedPredecessors) = {
        exchanges.foldLeft((distances, predecessors)) {
          case ((previousDistances, previousPredecessors), exchange) =>
            val cumulatedDistanceThroughSource = distances(exchange.source) + exchange.rate
            val currentDistToDest = distances(exchange.destination)
            if (cumulatedDistanceThroughSource < currentDistToDest) {
              (previousDistances + (exchange.destination -> cumulatedDistanceThroughSource),
                previousPredecessors + (exchange.destination -> exchange.source))
            } else (previousDistances, previousPredecessors)
        }
      }
      relaxation(i - 1, exchanges, updatedDistances, updatedPredecessors)
    }
  }

  def findNegativeCycles(source: Currency, exchanges: ExchangeGraph): Vector[Cycle] = {
    val (initDistances, initPredecessors) = init(source)
    val negLogEdges = exchanges.edges
    val (distances, predecessors) = relaxation(exchanges.size - 1, negLogEdges, initDistances, initPredecessors)
    negLogEdges
      .filter(edge => distances(edge.source) + edge.rate < distances(edge.destination))
      .flatMap {
        exchange => {
          Set.empty[Cycle] ++ negativeCycleRetracing(exchange, predecessors, exchange.source) ++ negativeCycleRetracing(exchange, predecessors, exchange.destination)
        }
      }.toVector
  }

  def negativeCycleRetracing(exchange: Exchange, predecessors: Predecessors, from: Currency): Option[Cycle] =
    from match {
      case exchange.source if exchange.source == exchange.destination =>
        // Should not happen because the exchange rate for a same currency is always one
        Cycle(Vector(exchange.source, exchange.destination)).some
      case exchange.source =>
        if (predecessors.contains(exchange.source)) {
          buildCycle(
            predecessors,
            2,
            predecessors(exchange.source),
            Map(exchange.source -> 1, exchange.destination -> 0),
            Vector(exchange.source, exchange.destination)
          )
        } else None
      case exchange.destination =>
        if (predecessors.contains(exchange.destination)) {
          buildCycle(
            predecessors,
            1,
            predecessors(exchange.destination),
            Map(exchange.destination -> 0),
            Vector(exchange.destination))
        } else None
    }

  @tailrec
  def buildCycle(predecessors: Map[Currency, Currency],
                 i: Int,
                 current: Currency,
                 visited: Map[Currency, Int],
                 result: Vector[Currency]): Option[Cycle] =
    if (visited.contains(current)) {
      val size = i - visited(current)
      Cycle(current +: result.take(size - 1)).some
    } else if (predecessors.contains(current)) {
      buildCycle(predecessors, i + 1, predecessors(current), visited + (current -> i), current +: result)
    } else None
}

object Model {

  final case class Currency(code: String) {
    override def toString: String = this.code
  }
  case class Exchange(source: Currency, destination: Currency, rate: Double) {
    val negLogRate: Double = -math.log(rate)
  }

    sealed trait Graph[N, E] {
      def addEdge(edge: E): Graph[N, E]
      def addNode(node: N): Graph[N, E]
      val nodes: Set[N]
      val edges: Set[E]
      val size: Int
    }

  final case class ExchangeGraph(exchanges: Set[Exchange], currencies: Set[Currency] = Set.empty[Currency]) extends Graph[Currency, Exchange]{
    def addEdge(exchange: Exchange): ExchangeGraph = ExchangeGraph(this.exchanges + exchange, currencies ++ Set(exchange.source, exchange.destination))
    def addNode(currency: Currency): ExchangeGraph = ExchangeGraph(this.exchanges, currencies + currency)
    val nodes: Set[Currency] = this.currencies
    val edges: Set[Exchange] = this.exchanges.map(exch => Exchange(exch.source, exch.destination, exch.negLogRate))
    val size: Int = currencies.size
  }

  object ExchangeGraph {
    def buildFromExchanges(exchanges: Set[Exchange]): ValidateData[ExchangeGraph] =
      Try(ExchangeGraph(exchanges, exchanges.flatMap(exch => Set(exch.source, exch.destination))))
        .toEither
        .leftMap(e => InvalidGraphConstruction(e.getMessage))
  }

  case class Cycle(currencies: Vector[Currency])
}

@main
def main(): Unit = {

  val url = "https://fx.priceonomics.com/v1/rates/"

  val output = for {
    rawData <- getRawData(url)
    dataDecoded <- decodeRawData(rawData)
    correctRates <- verifyRatePositivity(dataDecoded)
    exchanges <- transformToExchanges(correctRates)
    graph <- buildFromExchanges(exchanges)
    arbitrages <- getResult(graph.currencies.flatMap(findNegativeCycles(_, graph)), exchanges)
  } yield arbitrages

  output match {
    case Right(arbitrages) => arbitrages.foreach(println)
    case Left(error) => println(error.errorMessage)
  }
}
