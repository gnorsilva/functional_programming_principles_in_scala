package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait EasyLevel extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ST
        |oo
        |oo""".stripMargin

    val optsolution = List(Down, Right, Up)
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function easy level") {
    new EasyLevel {
      assert(!terrain(Pos(-2, -2)), "-2,-2")
      assert(!terrain(Pos(-2, -1)), "-2,-1")
      assert(!terrain(Pos(-2, 0)), "-2,0")
      assert(!terrain(Pos(-1, -2)), "-1,-2")
      assert(!terrain(Pos(-1, -1)), "-1,-1")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 0)), "1,0")
      assert(terrain(Pos(2, 0)), "2,0")
      assert(terrain(Pos(0, 1)), "0,1")
      assert(terrain(Pos(1, 1)), "1,1")
      assert(terrain(Pos(2, 1)), "2,1")
      assert(!terrain(Pos(0, 2)), "0,2")
      assert(!terrain(Pos(1, 2)), "1,2")
      assert(!terrain(Pos(2, 2)), "2,2")
      assert(!terrain(Pos(3, 0)), "3,0")
      assert(!terrain(Pos(3, 1)), "3,1")
      assert(!terrain(Pos(3, 2)), "3,2")
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("find easy level's start position ") {
    new EasyLevel {
      assert(startPos == Pos(0, 0))
    }
  }

  test("find easy level's goal position ") {
    new EasyLevel {
      assert(goal == Pos(0, 1))
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("a standing block") {
    new EasyLevel {
      assert(Block(Pos(0, 0), Pos(0, 0)).isStanding)
    }
  }

  test("a block on its side") {
    new EasyLevel {
      assert(!Block(Pos(1, 0), Pos(2, 0)).isStanding)
    }
  }

  test("a block in a legal position") {
    new EasyLevel {
      assert(Block(Pos(0, 0), Pos(0, 0)).isLegal)
    }
  }

  test("a block in a illegal position") {
    new EasyLevel {
      assert(!Block(Pos(0, 1), Pos(0, 2)).isLegal)
    }
  }

  test("easy level's initial block") {
    new EasyLevel {
      assert(startBlock == Block(Pos(0, 0), Pos(0, 0)))
    }
  }

  test("level 1 initial block") {
    new Level1 {
      assert(startBlock == Block(Pos(1, 1), Pos(1, 1)))
    }
  }

  test("easy level's initial block neighbours and moves") {
    new EasyLevel {
      val expectedNeighbours = List(
        (Block(Pos(0, -2), Pos(0, -1)), Left),
        (Block(Pos(0, 1), Pos(0, 2)), Right),
        (Block(Pos(-2, 0), Pos(-1, 0)), Up),
        (Block(Pos(1, 0), Pos(2, 0)), Down)
      )

      assert(startBlock.neighbors == expectedNeighbours)
    }
  }

  test("easy level's initial block legal neighbours and moves") {
    new EasyLevel {
      val expectedLegalNeighbours = List((Block(Pos(1, 0), Pos(2, 0)), Down))

      assert(startBlock.legalNeighbors == expectedLegalNeighbours)
    }
  }

  test("easy level is solved when block is at goal") {
    new EasyLevel {
      assert(done(Block(goal, goal)))
      assert(!done(startBlock))
    }
  }

  test("level1 is solved when block is at goal") {
    new Level1 {
      assert(done(Block(goal, goal)))
      assert(!done(startBlock))
    }
  }

  //    """ST
  //      |oo
  //      |oo"""

  test("neighbours with history for easy end point") {
    new EasyLevel {
      val moves = List(Up, Right, Down)
      val block = Block(goal, goal)
      val expectedNeighboursHistory = Set((block.down, Down :: moves))

      assert(neighborsWithHistory(block, moves).toSet === expectedNeighboursHistory)
    }
  }

  test("neighbours with history for level 1 start point") {
    new Level1 {
      val moves = List(Left, Up)
      val block = Block(Pos(1, 1), Pos(1, 1))
      val expectedNeighboursHistory = Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))

      assert(neighborsWithHistory(block, moves).toSet === expectedNeighboursHistory)
    }
  }

  test("new neighbours only level 1 start point") {
    new Level1 {
      val neighbours = Stream((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
                           (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))

      val explored = Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))


      val expectedNewNeighbours = Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))

      assert(newNeighborsOnly(neighbours, explored).toSet === expectedNewNeighbours)
    }
  }


//       01
//    """ST     0
//      |oo     1
//      |oo"""  2


  test("'from' for easy level"){
    new EasyLevel {
      val expectedResult = Stream((Block(Pos(1,0),Pos(2,0)), List(Down)),
                                  (Block(Pos(1,1),Pos(2,1)), List(Right, Down)),
                                  (Block(Pos(0,1),Pos(0,1)), List(Up,Right, Down)))

      assert(from(Stream((startBlock, List())), Set()).toSet === expectedResult.toSet)
    }
  }

//       0123456789
//    """ooo-------     0
//      |oSoooo----     1
//      |ooooooooo-     2
//      |-ooooooooo     3
//      |-----ooToo     4
//      |------ooo-"""  5


  test("'from' for level 1"){
    new Level1 {
      val initial = Stream((Block(Pos(1,2),Pos(1,3)), List(Right)),
                           (Block(Pos(2,2),Pos(2,3)), List(Down, Right)),
                           (Block(Pos(2,4),Pos(2,4)), List(Right, Down, Right)),
                           (Block(Pos(2,5),Pos(2,6)), List(Right,Right, Down, Right)))

      val explored = List(Block(Pos(1,2),Pos(1,3)),
                          Block(Pos(2,2),Pos(2,3)),
                          Block(Pos(2,4),Pos(2,4)),
                          Block(Pos(2,5),Pos(2,6)))


      assert(from(Stream((startBlock, List())), Set()).toSet.size === 148)
    }
  }

  test("paths from the start"){
    new Level1 {
      assert(pathsToGoal.toSet.size === 3)
    }
  }

  test("optimal solution for easy level") {
    new EasyLevel {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for easy level") {
    new EasyLevel {
      assert(solution.length === optsolution.length)
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }

}

