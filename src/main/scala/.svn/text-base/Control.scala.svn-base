package com.main

import scala.util.parsing.json._
import com.codahale.jerkson.Json._
import scala.io.Source
import java.io._

/*  abstract method for the interface
    all the other interfaces inherit from this,
    so I don't have to keep writing boilerplate code
*/
trait Interface {

  def printInterface
  /* pattern matching used in the interfaces */
  val DigitPattern = "([0-9]+)".r
  val  CityPattern = "([A-Z]{3})".r
  val RoutePattern = ("([A-Z]{3}[-][A-Z]{3})").r

  /* all the classes will overwrite this class
     so they can define their input behaviour
  */

  def parseLine ( line:String ) 

  /* main function that starts the interfaces */
  def start {
    printInterface
    manageLine ( readLine() )
  }

  /* loop to handle a request*/
  def manageLine ( line:String ) {
    if ( line != "q")  {
      parseLine( line )
      printInterface
      manageLine( readLine() )
    }
  }

  def printMenu ( info:List[String] ) {
    println("\n\n Enter a number for a command" )
    info.zipWithIndex.map ( x => println (x._2 + ". " + x._1) )
  }

  def errorMessage ( message:String = "Please, select a valid comand" ) = println( message )
}

/* pretty simple interface, its the first screenshot */
class MainInterface ( val graph: Graph ) extends Interface {
  
  val Switches   = List(  "Edit Comands" , "Information Comands" )
  start

  def printInterface = printMenu ( Switches )
  def parseLine ( line:String ) {
      val ret = line match {
        case "0" => new EditInterface ( graph )
        case "1" => new InfoInterface ( graph )
        case _   => errorMessage()
      }
  }
}

/* allows a user to do edit some aspect of the graph. */
class EditInterface ( val graph: Graph ) extends Interface {
  val stringInput = "STR"
  val intInput    = "INT"
  val coordInput  = "COORDS"
  val listInput   = "PORTS"

  var cityInfo = Seq(  
      ("code"        , stringInput),
			("name"        , stringInput),
			("country"     , stringInput),
			("continent"   , stringInput),
			("timezone"    , intInput   ),
			("population"  , intInput   ),
			("region"      , intInput   ),
			("coordinates" , coordInput )
    )

  val routeInfo = Seq(
      ("ports"   , listInput ),
      ("distance", intInput  )
    )

  val coords = Seq(
    ("S"             , intInput)    ,
    ("N"             , intInput)
  )

  val EditSwitch = List(  "Remove Route" , "Remove City", "Add City", "Add Route", "Edit City" )
  start // the interface

  def createClass[T:Manifest] ( info:Seq[ (String,Any) ] ) = parse[T](generate( Map( info:_*) ) )

  def printInterface = printMenu ( EditSwitch )
  def parseLine ( line:String ) = {
      val ret = line match {
        case "0" => removeRoute 
        case "1" => removeCity
        case "2" => graph.addCity( getClassFromInterface[City](cityInfo) )
        case "3" => graph.addRoutes ( List( getClassFromInterface[Route]( routeInfo ) ) )
        case "4" => new EditCityInterface ( graph, getInputClassCity )
        case _   => errorMessage("Select a number from 0 to 4.")
      }
  }

  val Word  = "([a-z][A-Z]+)".r
  def getStringInput ( line:String ):String = line

  def getDigitInput  ( line:String):Int  = line match {
    case DigitPattern(s) => s.toInt
    case _               => getDigitInput( readLine())
  }

  def editInterface ( info:Tuple2[String,String] ):Tuple2[ String,Any ] = {
    println("Enter " + info._1  )
    val ret = info._2 match {
      case x if x == stringInput => getStringInput ( readLine() )
      case x if x == intInput    => getDigitInput  ( readLine() )
      case x if x == coordInput  => Map(coords.map ( editInterface _ ):_*)
      case x if x == listInput   => List( getInputCity, getInputCity )
    }
    info._1 -> ret
  }

  def getClassFromInterface[T:Manifest] ( formatData:Seq[(String,String)] ) :T = {
    val info = formatData.map ( editInterface _ )
    createClass[T]( info )
  }

  def getInputRoute: Array[String] = readLine() match {
    case RoutePattern(s) => s.split("-")
    case _ => { 
      errorMessage("Not a valid route")
      getInputRoute
    }
  }

  def getInputCity:  String = {
      println("Enter city code")
      readLine() match {
      case CityPattern(s) => s
      case _ => {
        errorMessage("Enter 3 uppercase letters")
        getInputCity
      }
    }
  }

  def getInputClassCity: City = graph.findNode(getInputCity) match {
    case Some(x) => x.city
    case _       => {
      errorMessage("Not a valid city")
      getInputClassCity
    }
  }

  def removeRoute = {
    val route = getInputRoute
    graph.deleteRoute ( route.first, route.last )
  }

  def removeCity = graph.deleteCity ( getInputCity )
}

class EditCityInterface ( override val graph: Graph, var city:City ) extends EditInterface(graph) {

  override def printInterface = {
    println("Select attribute")
    printMenu (  cityInfo.map(_._1).toList )
  }

  /* make sure to get a valid input
     and get the desired type of input  */
  override def parseLine ( input:String ) = input match {
      case DigitPattern(input) if input.toInt < cityInfo.length => {
        val toAdd = editInterface ( cityInfo( input.toInt ) )
        val newCity = codeNewCity ( city, toAdd )
        graph.modifyCity( city.code, newCity)
        city = newCity // so that we can modify the city again
      }
      case _  => {
        errorMessage()
        parseLine( readLine() )
      }
  }

  def codeNewCity ( city:City, toAdd: Tuple2[String,Any] ): City = {
    val dump:Map[String,Any] = parse[Map[String,Any]]( generate ( city ) ) map {
      case(k, v) => if( k == toAdd._1) (k -> toAdd._2) else (k ->v)
    }
    parse[City]( generate(dump) )
  }
}

class InfoInterface ( val graph: Graph ) extends Interface {
  /* List of all possible comands we can launch */
  val Routes = graph.routes
  val Cities = graph.cities
  /* declare all possible inputs that the user will ask for */
  lazy val Comands = List(
                      ("All cities"           , Cities ),
                      ("Shortest Route"       , List(Routes.minBy( _.distance ).fullInfo)),
                      ("Longest Route"        , List(Routes.maxBy( _.distance ).fullInfo)),
                      ("Average Distance"     , List(Routes.map  ( _.distance ).sum / Routes.length )), 
                      ("Smallest City"        , List(Cities.minBy( _.population ).fullInfo)),
                      ("Largest City  "       , List(Cities.maxBy( _.population ).fullInfo)),
                      ("Average Size"         , List(Cities.map  ( _.population ).sum / Cities.length )), 
                      ("Continent and Cities" , Cities.groupBy   ( _.continent )),
                      ("Hub Cities"           , 
                        graph.nodes .sortBy( _.routes.length ).reverse.
                        map ( x => x.city + " Connections: " + x.routes.length ))
                    )

  start // the interface

  def parseLine ( line:String ) =  line match {
    case DigitPattern (d) => launchDigitComand (  d.toInt )
    case CityPattern ( c) => launchCityComand  (  c       )
    case RoutePattern( c) => launchRouteComand (  c       )
    case _ => errorMessage()
  }

  def printInterface = {
    printMenu ( Comands. map( _._1 ) )
    println("Or a city code or route")
  }

  // used for debugging 
  def printAllCommands = {
    for ( c <- Comands) {
      println( c._1 )
      c._2.map ( println(_) )
    }
  }

  def launchCityComand  ( code: String ) = {
    graph.findNode( code ) match {
      case Some(x) => println( x.fullInfo )
      case _       => println("Please select a valid city")
    }
  }
  // print the comand that we want
  def launchDigitComand ( d: Int ) = 
    if (d < Comands.length) {
      println( Comands(d) ._1)
      Comands( d )._2.map ( println(_) )
    }
    else errorMessage()

  def launchRouteComand ( codes:String ) = {
    val split = codes.split("-")
    graph.getAllInfoRoute( split.first, split.last ).map( println(_) )
  }
}

object JsonInterface {

  val CITIES = "metros"
  val ROUTES = "routes"

  val CityDump  = "data/cityDump.json"
  val RouteDump = "data/routeDump.json"

  def openFile ( path: String ) = Source.fromFile( path ) mkString
  def openJson ( path: String ) = JSON.parseFull ( openFile( path ) )
  def parseToClass[T :Manifest] ( partial : Option[Any], switch: String ): T = partial match {
      case Some( m: Map[String, T]) => parse[T]( generate( m(switch)) )
      case _ => throw new Exception("Could not parse Json")
  }

  def parseToSingleClass[T :Manifest] ( path:String ): T = parse[T](  Source.fromFile( path ) mkString  )

  def dumpGraph ( graph:Graph ) = {
    saveJson[List[City]] ( CityDump  , graph.cities)
    saveJson[List[Route]]( RouteDump , graph.routes)
  }

  def saveJson[T] ( path:String , obj:T )  = {
    val out = new java.io.FileWriter ( path )
    out.write (  generate( obj ) )
    out.close
  }

  def createGraph ( path:String ): Graph = {
    val rawJson = openJson ( path )
    /* parses the json file into classes, that is, a list of cities and a list of routes */
    val cities  = parseToClass[List[City]] ( rawJson, CITIES )
    val routes  = parseToClass[List[Route]]( rawJson, ROUTES )
    new Graph ( cities, routes )
  }

  def loadFromDump = new Graph ( parse[List[City]]( openFile( CityDump )), parse[List[Route]]( openFile( RouteDump )) )
  def addFromJson ( path: String, oldGraph: Graph ) = oldGraph.merge( createGraph( path ) )
}

object Classes {
    
  val PATH    = "data/map_data.json"
  val ADDPATH = "data/cmi_hub.json"

  def main(args: Array[String]) {

    /* create the graph */
    /*val graph    = JsonInterface.createGraph( PATH )*/
    val addGraph = JsonInterface.loadFromDump
    /*graph openUrl*/
  
    JsonInterface.dumpGraph ( addGraph )

    /* start user interaction */
    val console = new MainInterface ( addGraph )
  }
}
