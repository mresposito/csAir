package com.example.test
 
import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import com.main._
 
class GraphTest extends FunSuite {

  val graph     = JsonInterface. createGraph( Classes.PATH )
  val addGraph  = JsonInterface. addFromJson ( Classes.ADDPATH, JsonInterface.createGraph( Classes.PATH ))

  test("Json IO") {
    JsonInterface.dumpGraph ( graph )
    val newGraph  =  JsonInterface.loadFromDump

    assertEquals ( newGraph.cities.length, graph.cities .length )
    assertEquals ( newGraph.nodes .length, graph. nodes .length )
    assertEquals ( newGraph.routes.length, graph. routes.length )
  }

  test("Add New Data") {
    assertEquals ( addGraph.nodes .length, graph. nodes .length + 1 )
    assertEquals ( addGraph.routes.length, graph. routes.length + 9 )
  }

  test("Dijkstra") {
    testAndPrint( "CMI","LAX", 1 )
    testAndPrint( "CMI","BOG", 2 )
    testAndPrint( "CMI","SAO", 3 )

    testAndPrint( "MIL","PAR", 1 )
    testAndPrint( "MIL","SAO", 3 )
  }

  test("RouteCost") {
    assertEquals( addGraph.routeCost(addGraph.findPath("CMI","LAX")), 598 )
    assertEquals( addGraph.routeCost(addGraph.findPath("CMI","BOG")), 380 + 727)
  }

  test("Delete Route") {
    addGraph.deleteRoute( "CMI", "LAX" )
    assertTrue( addGraph.routeCost(addGraph.findPath("CMI","LAX")) != 598 )
  }

  test("Delete City") {
    val cmiRoutes   = addGraph.findNode("CMI").map( _.routes.length).first
    val totalRoutes = addGraph.routes.length
    addGraph.deleteCity( "CMI" )
    assertEquals( addGraph.routes.length, totalRoutes - cmiRoutes  )
  }

  test("Update City") {
    val newCode = "ZZZ"
    val milan = new City ( newCode , "Milan" , "IT" , "Europe" ,
			 1 , Map("N" -> 45, "E" -> 9) , 3575000 , 3 )
    graph.modifyCity( "MIL", milan )
    val newMilan = graph.findNode ( newCode ) match {
      case Some(x) => x
      case _       => {
        assertEquals(1,0)
        new Node( milan, List() )
      }
    }
    assertEquals ( newMilan.city.code, newCode )
    assertEquals ( newMilan.routes.length, 3 )
    /* test the routes again */
    assertEquals ( graph.findPath( newCode, "SAO" ).length, 3 )
  }

  def testAndPrint( from:String, to:String, steps:Int ) = {
    println( addGraph.findPath( from, to).map( _.pretty ).mkString(" ") )
    assertEquals ( addGraph.findPath( from, to ).length, steps )
  }

}
