package parser

sealed trait Fee                                                        extends Product with Serializable
case class TechnicalFee(technical: Double)                              extends Fee
case class ExplorationFee(exploration: Double, postExploration: Double) extends Fee
