package com.main

case class City  ( code: String, name: String,  country : String,continent : String,
                  timezone: Int,  coordinates: Map[String,Int], population: Int, region: Int ){ 
  override def toString : String = code + ", " + name 

  def fullInfo : String = 
  List( "Code " + code       , "Name " + name       , "Country " + country       ,
    "Continent " + continent , "Timezone "+timezone , "Coordinates "+coordinates ,
    "Population "+population , "Region "+region     , "" ).mkString ("\n")
}

case class Route ( var ports: List[String], distance : Int ) {
  def pretty   = ports.mkString("-")
  def fullInfo = pretty + " Distance " + distance + " km"

  def isDirectedRoute ( from:String, to:String ):Boolean =
    ports.first == from && ports.last == to

  // change the code of a route. 
  // allows edit info
  def changeCode ( oldCode:String, newCode:String ) {
    if ( ports.first == oldCode ){
      ports = List( newCode, ports.last )
    } else {
      ports = List( ports.first, newCode )
    }
  }
}

/* nodes in the graph. contains the city and the edge */
class Node  ( val city: City, var routes: List[Route] ) {
  // prints all of the info of a city
  def fullInfo: String = city.fullInfo + "Routes to: " + city.name+ " \n" +
                         routes.map( _.fullInfo ).mkString("\n")

  // used for Dijkstra
  var distance: Int  = Int.MaxValue
  var previous: Node = this

  def getAdjecent = List.flatten( routes.map( _.ports) ).filter( _ != city.code )
  def routeTo( to:Node ):Option[Route]  = routes.find( x=> x.ports.first == to.city.code || x.ports.last == to.city.code )
  def distanceTo( to:Node ):Int = routeTo(to) match {
      case Some(n) => n.distance
      case _ => throw new Error("Route not found")
    }
}


class Graph ( var cities: List[City], var routes : List[Route]  ) {
  // create new nodes with cities and empty routes
  var nodes: List[Node] = cities.map ( new Node( _, List[Route]()) )
  addRoutes ( routes )

  /*** GRAPH MODIFICATIONS ***/
  def addRoutes ( routes : List[Route]) = {
    // initialize the routes, by adding them to the nodes
    routes.foreach ( x => addToNodes( x, x.ports.first ))
    routes.foreach ( x => addToNodes( x, x.ports.last  ))
  }

  // make sure list is not empty
  def addToNodes ( route: Route, cityCode: String ) = {
    nodes.find ( _.city.code == cityCode ) match {
      case Some( x ) => x.routes ::= route 
      case _ => None
    }
  }

  // add the two graph, by recreating the previous graph
  def merge( graph: Graph): Graph = {
    this.cities ++=  graph.cities
    this.routes ++=  graph.routes
    nodes = cities.map ( new Node( _, List[Route]()) )
    addRoutes ( routes )
    this // return the obeject
  }
    
  // getUrl = go throught all the routes, and join them with a blac character. this way,
  // i dont have to traverse the graph
  // openUrl= ask the shell to open the url 
  def getUrl   = routes.map ( _.pretty).mkString( "%0D%0A")
  def printUrl = "http://www.gcmap.com/mapui?P=" + getUrl
  def openUrl  = scala.tools.nsc.interpreter.ProcessResult( "open " + printUrl)

  /*** DATA ACCESS FUNCTIONS ***/
  def findNode ( code: String ):Option[Node] = nodes.find ( _.city.code == code )

  def neighbor ( node: Node, nodesLeft: List[Node] ) :List[Node] = {
    val adjNodes = node.getAdjecent
    nodesLeft.filter ( x => adjNodes.indexOf( x.city.code ) != -1 )
  }

  /*** DIJKSTA ***/
  def findPath ( from:String, to :String ):List[Route] = 
    shortestDistance( o(findNode( from )), o(findNode(to))) 

  /* apply DIJKSTA algorthm */
  def shortestDistance ( from: Node, to: Node):List[Route] =  {
    initalizeNodeToDijksta
    from.distance = 0
    findBest ( from, this.nodes )
    findSequence ( from, to ).reverse
  }

  /* reconstruct the shortest path */
  def findSequence ( from:Node,  to:Node ):List[Route] = {
    if ( from == to )
      List()
    else if ( to.previous != to )
      o[Route](to.routeTo(to.previous)) :: findSequence( from, to.previous )
    else
      throw new Error("Path not found")
  }
  /* default contions */
  def initalizeNodeToDijksta = {
    for ( n <- this.nodes ) {
      n. distance = Int.MaxValue
      n. previous = n
    }
  }
  /*shortcut for bypassing error handling*/
  def o[T]( o:Option[T]): T = o match {
      case Some(x) => x
      case _       => throw new Error ("Could not find node")
    }

  /*body of dijksta. applies the algoripthm according to wikipedia. */
  def findBest ( currBest: Node, nodesLeft: List[Node] ): List[Node] = {
    if ( currBest.distance == Int.MaxValue || nodesLeft.length <= 0 ) {
      nodesLeft
    } else {
      for ( v <- neighbor( currBest, nodesLeft) ) {
        val alt = currBest. distance + v.distanceTo( currBest )
        if ( alt < v.distance ) {
          v.distance = alt
          v.previous = currBest
        }
      }
      findBest ( nodesLeft.minBy( _.distance ) , nodesLeft.filter( _ != currBest ).sortBy(_.distance) )
    }
  }

  val COST     = 0.35
  val DISCOUNT = 0.05
  def indexCost ( idx:Int ):Double = COST - idx * DISCOUNT match {
    case k if k > 0 => k
    case _          => 0
  }

  /*** ROUTE INFORMATION ***/
  def getAllInfoRoute (  from:String, to:String ): List[String] = {
    val path = findPath( from, to )
    var info:List[String] = path.zipWithIndex.map( x => x._2 + ") " + x._1.fullInfo )
    info ++= List("Total distance: " + path.map( _.distance ).sum ) 
    info ++= List("Total cost: "     + routeCost( path ) + "$" ) 
    info
  }
  def routeCost( route:List[Route] ):Int = {
    route.zipWithIndex.map( x => indexCost(x._2)*x._1.distance ).sum.toInt
  }

  /***  MODIFY GRAPH ***/
  def deleteRoute ( from:String, to:String ) = {
    for ( n <- nodes ) { // first filter the routes in the nodes
      n.routes = n.routes.filter ( !  _.isDirectedRoute( from, to ))
    } // now filter all the routes
    routes = routes.filter ( ! _.isDirectedRoute( from, to ))
  }
  // delete a city from the graph
  def deleteCity  ( code:String ) =  {
    nodes = nodes.filter( _.city.code != code )
    for ( n <- nodes ) { // remove that city from every route
      n.routes = n.routes.filter( _.ports.indexOf(code) == -1 )
    }
    routes = routes.filter( _.ports.indexOf(code) == -1 )
  }

  def addCity ( city:City ) = nodes ::= new Node( city, List[Route]() )

  /* update the existing codes */
  def updateCodes ( oldCode: String, newCode:String ) = {
    routes.filter( _.ports.indexOf ( oldCode ) != -1 ).map( _.changeCode( oldCode, newCode ) )
  }

  /*changes the old city code and route with the new one.*/
  /*allows the edit interface*/
  def modifyCity ( oldCode:String, newCity:City ) = {
    val newCode = newCity.code
    if ( oldCode != newCode ) {
      updateCodes( oldCode, newCode )
    }
    val routesToAddBack = routes.filter( _.ports.indexOf ( newCode ) != -1 )
    // delete the old city
    deleteCity ( oldCode )
    addCity    ( newCity )
    // insert the routes back
    val nodeAdded = o(findNode(newCode) )
    nodeAdded.routes ++= routesToAddBack
  }
}
