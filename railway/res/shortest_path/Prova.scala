import scala.actors._
import scala.util.parsing.json._

object Main extends App {
	
	var res : Map[Int,List[(Int,Int)]] = null
	
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
	
			res(u).foreach( v => {
				if ((Q contains v._1) && (v._1 != u)) {
			
					val d = if (dist(u)== 100000000) 0 else dist(u)
					val alt =  d + v._2
					if (alt < dist(v._1)) {
						dist(v._1) = alt
					    prec(v._1) = u ;
					}
				}
			})
		}

		prec
	}
	
	def load(fileName : String) {
		
		val json = scala.io.Source.fromFile(fileName).mkString
	
		val json_elements = JSON.parseFull(json)
	
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
	} 

	override def main(argv : Array[String]) {
		
		if (argv.size < 2) {
			println("ERROR: Specify :")
			println("   1) Input file")
			println("   2) Node Name")
			return
		}
		
		val input 	= argv(0)
		val nodeName = argv(1)
		
		res = Map()
		
		load(input)
	
		var totMap : Map[Int,Map[Int,List[Int]]] = Map()
	
		for ( i <- 1 to res.keys.size) {
			val prec = dijkstra(i)
			var innerMap : Map[Int,List[Int]] = Map()
			for ( j <- 1 to res.keys.size) {
				var s : List[Int] = List()
				var d = j
				while (prec(d) != 0) {
					s = d :: s
					d = prec(d)
				}
				s = i :: s
				innerMap += Tuple2(j,s)
			}
			totMap += Tuple2(i,innerMap)
		}
		
		var json = ""
		
		json += ("{" + "\n")
		json += (""" "paths" : [""" + "\n")
		var i = 0
		totMap.keys.foreach( k => {
			json += ("    {" + "\n")
			json += ("""        "from"         : """ + k + "," + "\n")
			json += ("""        "destinations" : [""" + "\n")
			var j = 0
			totMap(k).keys.foreach ( d => {
				json += ("            {" + "\n")
				json += ("""                "dest" : """ + d + "," + "\n")
				json += ("""                "path" : [""")
					var f = 0
					totMap(k)(d).foreach(e => {
						json += (e)
						if (f < totMap(k)(d).size-1) json += (",")
						f += 1
					})
				json += ("]\n")
				json += ("            }")
				if (j == totMap(k).keys.size-1) json += "\n"
				else json += (",\n")
				j += 1
			})
			json += ("        ]\n")
			json += ("    }")
			if (i == totMap.keys.size-1) json += "\n"
			else json += (",\n")
			i+=1
		})
		json += ("  ]\n")
		json += ("}\n")
		
		val out = new java.io.FileWriter("../../" + nodeName + "-paths.json")
		out.write(json)
		out.close
		
		println("\nWrote on file : " + nodeName + "-paths.json")
		
	}
}

