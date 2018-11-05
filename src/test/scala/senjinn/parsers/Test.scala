package senjinn.parsers

object Test extends App 
{
	val hat  = "hat[^a]+".r
	val hathaway = "hathatthattthatttt"
	val hats = hat.findAllIn(hathaway).toList
	val pos  = hat.findAllMatchIn(hathaway).map(_.matched).toList
	
	val madhatter = "(h)(?=(at[^a]+))".r
  val madhats   = madhatter.findAllMatchIn(hathaway).map(_.matched).toList
	
	println(madhats)
}