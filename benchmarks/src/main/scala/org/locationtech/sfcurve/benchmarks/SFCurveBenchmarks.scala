package org.locationtech.sfcurve.benchmarks

import org.locationtech.sfcurve.zorder._

import com.google.caliper.Param

object SFCurveBenchmarks extends BenchmarkRunner(classOf[SFCurveBenchmarks])
class SFCurveBenchmarks extends CurveBenchmark {

  val pts = (0 until 300).toArray

  def timeZ2IndexCreate(reps: Int) = run(reps)(z2IndexCreation)
  def z2IndexCreation = {

    var res = 2
    while(res < 24){
        new ZCurve2D(res)
        res += 1
    }
  }

  def timeZ2BadCase(reps: Int) = run(reps)(Z2BadCase)
  def Z2BadCase = {
      var t1 = Z2(0,90)
      var t2 = Z2(180,0)
      println("t1: " + t1.z + " t2: " + t2.z)
      var i = 2 //resolution bits
      while (i < 20){
          val sfc = new ZCurve2D(i)
          val range = sfc.toRanges(-178.123456, -86.398493, 179.3211113, 87.393483)
          i += 1
      }

  }


  def timeZ3IndexCreate(reps: Int) = run(reps)(z3IndexCreation)
  def z3IndexCreation = {

    var x = 0
    var y = 0
    var z = 0

    while(x < 200) {
      while(y < 200) {
        while(z < 200) {
          Z3(pts(x), pts(y), pts(z))
          z += 1
        }
        y += 1
      }
      x += 1
    }
  }

  def timeZ3ZRanges(reps: Int) = run(reps)(z3ZRangesCreation)
  def z3ZRangesCreation = {
    var x = 0
    var y = 0
    var z = 0

    while(x < 100){
        while(y < 100){
            while(z < 100){
                var z31 = Z3(x-100, y-100, z-100)
                var z32 = Z3(x, y, z)
                Z3.zranges(z31, z32)
                z += 1
            }
            y += 1
        }
        x += 1
    }
}

}
