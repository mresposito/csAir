package com.example.test
 
import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import com.main._
 
class ParseTest extends FunSuite {

  val cls = JsonInterface
  val cityJson  = JsonInterface.openJson ("data/singleCity.json")
  val routeJson = JsonInterface.openJson ("data/singleRoute.json")
  val cities    = JsonInterface.parseToClass[List[City]] ( cityJson ,cls.CITIES )
  val city      = cities.first
  val route     = JsonInterface.parseToClass[List[Route]]( routeJson, cls.ROUTES )
  val graph     = new Graph ( cities, route ) 

  test("Test City Parse") {
    assertEquals( city.name  , "Santiago" )
    assertEquals( city.region, 1 )
  }

  test("Test Route Parse") {
    assertEquals( route.first.ports.first   , "SCL" )
    assertEquals( route.first.distance, 2453  )
  }

  test("Graph") {
    assertEquals ( graph.nodes  length, 2 )
    assertEquals ( graph.routes length, 1 )
    graph.nodes.foreach ( x => assertEquals ( x.routes length, 1 ))
  }
}
