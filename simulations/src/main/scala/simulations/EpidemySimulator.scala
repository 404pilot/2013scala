package simulations

import math.random

class EpidemySimulator extends Simulator {

  /**
   * generate 0 to i -1
   *
   * @param i
   * @return
   */
  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val incubationTime: Int = 6
    val dieTime: Int = 14
    val immuneTime: Int = 16
    val healTime: Int = 18

    val prevalenceRate: Double = 0.01
    val transRate: Double = 0.4
    val dieRate: Double = 0.25

    val checkDelay = 1
  }

  import SimConfig._

  val persons: List[Person] =
    for (i <- (1 to population).toList) yield {
      val person = new Person(i)
      person.init()
      person
    }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    // when is moving, do not consider it
    var isMoving = false

    //
    // to complete with simulation logic
    //

    def init(): Unit = {
      // since (row, col) is random, how to choose id doesn't matter 
      if (id <= prevalenceRate * population) getInfectedAction()
      runAgenda()
    }

    /**
     * move
     */
    def movingAction(): Unit = {
      def _nextMove: (Int, Int) = {
        def _isMovable(pos: (Int, Int)): Boolean =
          !(persons exists ((other: Person) => (other.col == pos._1 && other.row == pos._2 && !other.isMoving && other.sick)))

        def _generateAllPossibleMoves: List[(Int, Int)] = {
          def _coordinate(col: Int, row: Int): (Int, Int) = ((roomColumns + col) % roomColumns, (roomRows + row) % roomRows)

          _coordinate(col - 1, row) :: _coordinate(col + 1, row) :: _coordinate(col, row - 1) :: _coordinate(col, row + 1) :: Nil
        }

        def _generateNextMove(temp: List[(Int, Int)]): (Int, Int) = temp match {
          case Nil => (col, row)
          case _ =>
            {
              val randomIndex = randomBelow(temp.length)
              val randomPos = temp(randomIndex)
              if (_isMovable(randomPos)) randomPos
              // drop starts from 1
              else _generateNextMove(temp.drop(randomIndex + 1))
            }
        }
        _generateNextMove(_generateAllPossibleMoves)
      }

      // only live person can move
      if (!dead) {
        val (newCol, newRow): (Int, Int) = _nextMove
        //        println(s"${id} ==== ${col},${row} --> ${newCol}, ${newRow}")
        if (!(col == newCol && row == newRow)) {
          isMoving = true
          col = newCol
          row = newRow
        }
      }
    }

    /**
     * get infected
     */
    def getInfectedAction(): Unit = {
      infected = true
      afterDelay(incubationTime)(sick = true)
      afterDelay(dieTime)(if (random >= dieRate) dead = true)
      afterDelay(immuneTime)(if (!dead) { immune = true; sick = false })
      afterDelay(healTime)(if (!dead) { infected = false; sick = false; immune = false; dead = false })
    }

    /**
     * called after moving
     */
    def afterMovingAction(): Unit = {
      def _isInfected: Boolean = {
        def _isInfectable: Boolean = persons exists ((other: Person) => (other.col == col && other.row == row && !other.isMoving && other.infected))

        (!dead) && (!infected) && (!immune) && (!sick) && _isInfectable && random <= transRate
      }

      // people only get infected when they're moving.
      if (isMoving) {
        isMoving = false
        if (_isInfected) getInfectedAction()
      }
    }

    /**
     * add this agenda to the tail
     */
    /*def movingHelperAction(): Unit = {
      afterDelay(0) { if (!dead && isMoving) isMoving = false }
    }*/

    def runAgenda(): Unit = {
      // ============================= agenda ==================================
      val moveDelay = randomBelow(5) + 1
      afterDelay(moveDelay) {
        //println("run movingAction")
        movingAction()
        //println("finish movingAction")

        //println("run after moving")
        afterMovingAction()
        //println("finish after moving")

        /*movingHelperAction*/

        runAgenda()
      }
    }
  }
}