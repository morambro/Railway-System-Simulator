import scala.actors._
import scala.util.parsing.json._

object Main extends App {

	val json = scala.io.Source.fromFile("Node_1-topology.json").mkString
	
	val json_elements = JSON.parseFull(json)
	
	var res : Map[Int,List[(Int,Int)]] = Map()
	
	json_elements match {
		case Some(e) 	=> e match { 
				case map : Map[String,List[_]] 	=> {
					map("topology") match {
						case el : List[Any] => el.foreach(
							node => {
								node match {
									case nodeMap : Map[String,_] => {
										var neigh : List[Int] = List()
										var distance : List[Int] = List()
										nodeMap("neighbours") match {
											case l : List[_] => l foreach( n => {
												n match {
													case a : Double => neigh = a.toInt :: neigh
												}
											})
										}
										nodeMap("distance") match {
											case l : List[_] => l foreach( n => {
												n match {
													case a : Double => distance = a.toInt :: distance
												}
											})
										}
										nodeMap("station") match {
											case a : Double => res = res + Tuple2(a.toInt,neigh.zip(distance))
										}
										
									}
									case _ => println("Error")
								}
							}
						)
					}
				}
				case _				=> println("Parse Error")
		}
		case None		=> println("Parse Error")
	}
	
	
	
	def dijkstra(sorgente : Int) : Array[Int] = {
		
		var dist = new Array[Int](res.keys.size+1)
	
		var prec =	 new Array[Int](res.keys.size+1)
		
		var Q : List[Int] = List()
	
		res.keys.foreach(
			k => {
				dist(k) = 100000000
				prec(k) = 0 
				Q = k :: Q
			}
		)
	
		dist(sorgente) = 0
	
		def findMin(Q : List[Int]) : Int = {
			Q match {
				case List() 		=> -1
				case a :: List() 	=> a
				case a :: l 		=> {
					val m = findMin(l)
					if (dist(m) < dist(a)) 
						m
					else
						a
				}
			}
		}
	
	
		while (Q.size > 0) {
			val u = findMin(Q)
			Q = Q filterNot (el => el == u)
		
//			dist.foreach(el => print(el+","));println;
//			println("\n" + Q + "  min = "+u+" \n")
		
			res(u).foreach( v => {
				if ((Q contains v._1) && (v._1 != u)) {
				
					val d = if (dist(u)== 100000000) 0 else dist(u)
					val alt =  d + v._2
//					println("alt = " + alt)
					if (alt < dist(v._1)) {
						dist(v._1) = alt
//						println("dist("+(v._1) +") = "+alt)
				        prec(v._1) = u ;
//				        println("prec("+(v._1) +") = "+u)
					}
				}
			})
		}
	
//		prec.foreach(e => print(e + ","));println
		prec
	}
	
	for ( i <- 1 to res.keys.size) {
		val prec = dijkstra(i)
		
		for ( j <- 1 to res.keys.size) {
			var s : List[Int] = List()
			var d = j
			while (prec(d) != 0) {
				s = d :: s
				d = prec(d)
			}
			s = i :: s
			println("From " + i + " : ") 
			print(j + " => ");println(s)
		}
	}
	
}

