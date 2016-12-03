package com.untrackedfiles.misc.sudoku

import org.specs2.mutable.Specification

class ArrayBasedBoardSpec extends Specification{

  "Board.parse" should {

    "parse complete board from string" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.cellIterator.filter(_.value.isEmpty).isEmpty mustEqual (true)
      board.toString mustEqual (str)
    }

    "parse empty board from string" in {
      val exp = "." * 81
      val str = "0" * 81
      val board = ArrayBasedBoard.parse(str)

      board.cellIterator.filter(_.value.isDefined).isEmpty mustEqual (true)
      board.toString mustEqual (exp)
    }
  }

  "block" should {

    def test(board: ArrayBasedBoard, xy: (Int, Int), blockXY: (Int, Int)) = {
      board.block(xy._1, xy._2).size mustEqual (9)
      board.block(xy._1, xy._2) mustEqual (board.block(blockXY._1, blockXY._2))
    }

    /**
      * +-----------------------+
      * | 1 2 3 | 4 5 6 | 7 8 9 |
      * | 4 5 6 | 7 8 9 | 1 2 3 |
      * | 7 8 9 | 1 2 3 | 4 5 6 |
      * |-------+-------+-------|
      * | 2 3 4 | 5 6 7 | 8 9 1 |
      * | 5 6 7 | 8 9 1 | 2 3 4 |
      * | 8 9 1 | 2 3 4 | 5 6 7 |
      * |-------+-------+-------|
      * | 3 4 5 | 6 7 8 | 9 1 2 |
      * | 6 7 8 | 9 1 2 | 3 4 5 |
      * | 9 1 2 | 3 4 5 | 6 7 8 |
      * +-----------------------+
      */
    "return associated box in complete board" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      List(
        // upper left block
        ((1, 0), (0, 0)),
        ((2, 0), (0, 0)),
        ((0, 1), (0, 0)),
        ((1, 1), (0, 0)),
        ((2, 1), (0, 0)),
        ((0, 2), (0, 0)),
        ((1, 2), (0, 0)),
        ((2, 2), (0, 0)),
        // upper middle block
        ((4, 0), (3, 0)),
        ((5, 0), (3, 0)),
        ((3, 1), (3, 0)),
        ((4, 1), (3, 0)),
        ((5, 1), (3, 0)),
        ((3, 2), (3, 0)),
        ((4, 2), (3, 0)),
        ((5, 2), (3, 0)),
        // upper right block
        ((7, 0), (6, 0)),
        ((8, 0), (6, 0)),
        ((6, 1), (6, 0)),
        ((7, 1), (6, 0)),
        ((8, 1), (6, 0)),
        ((6, 2), (6, 0)),
        ((7, 2), (6, 0)),
        ((8, 2), (6, 0)),
        // upper right block
        ((7, 0), (6, 0)),
        ((8, 0), (6, 0)),
        ((6, 1), (6, 0)),
        ((7, 1), (6, 0)),
        ((8, 1), (6, 0)),
        ((6, 2), (6, 0)),
        ((7, 2), (6, 0)),
        ((8, 2), (6, 0)),
        // middle left block
        ((1, 3), (0, 3)),
        ((2, 3), (0, 3)),
        ((0, 4), (0, 3)),
        ((1, 4), (0, 3)),
        ((2, 4), (0, 3)),
        ((0, 5), (0, 3)),
        ((1, 5), (0, 3)),
        ((2, 5), (0, 3)),
        // middle middle block
        ((4, 3), (3, 3)),
        ((5, 3), (3, 3)),
        ((3, 4), (3, 3)),
        ((4, 4), (3, 3)),
        ((5, 4), (3, 3)),
        ((3, 5), (3, 3)),
        ((4, 5), (3, 3)),
        ((5, 5), (3, 3)),
        // middle right block
        ((7, 3), (6, 3)),
        ((8, 3), (6, 3)),
        ((6, 4), (6, 3)),
        ((7, 4), (6, 3)),
        ((8, 4), (6, 3)),
        ((6, 5), (6, 3)),
        ((7, 5), (6, 3)),
        ((8, 5), (6, 3)),
        // lower left block
        ((1, 6), (0, 6)),
        ((2, 6), (0, 6)),
        ((0, 7), (0, 6)),
        ((1, 7), (0, 6)),
        ((2, 7), (0, 6)),
        ((0, 8), (0, 6)),
        ((1, 8), (0, 6)),
        ((2, 8), (0, 6)),
        // lower middle block
        ((4, 6), (3, 6)),
        ((5, 6), (3, 6)),
        ((3, 7), (3, 6)),
        ((4, 7), (3, 6)),
        ((5, 7), (3, 6)),
        ((3, 8), (3, 6)),
        ((4, 8), (3, 6)),
        ((5, 8), (3, 6)),
        // lower right block
        ((7, 6), (6, 6)),
        ((8, 6), (6, 6)),
        ((6, 7), (6, 6)),
        ((7, 7), (6, 6)),
        ((8, 7), (6, 6)),
        ((6, 8), (6, 6)),
        ((7, 8), (6, 6)),
        ((8, 8), (6, 6))
      ).map(p => test(board, p._1, p._2))
    }
  }

  "neighborhood" should {
    /**
      * +-----------------------+
      * | 1 2 3 | 4 5 6 | 7 8 9 |
      * | 4 5 6 | 7 8 9 | 1 2 3 |
      * | 7 8 9 | 1 2 3 | 4 5 6 |
      * |-------+-------+-------|
      * | 2 3 4 | 5 6 7 | 8 9 1 |
      * | 5 6 7 | 8 9 1 | 2 3 4 |
      * | 8 9 1 | 2 3 4 | 5 6 7 |
      * |-------+-------+-------|
      * | 3 4 5 | 6 7 8 | 9 1 2 |
      * | 6 7 8 | 9 1 2 | 3 4 5 |
      * | 9 1 2 | 3 4 5 | 6 7 8 |
      * +-----------------------+
      */
    "return associated neighborhood in a complete board" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.neighborhood(3, 0).filter(_.value.nonEmpty).flatMap(_.value).toSet mustEqual (Set[Int](1,2,3,4,5,6,7,8,9))
      board.neighborhood(6, 7).filter(_.value.nonEmpty).flatMap(_.value).toSet mustEqual (Set[Int](1,2,3,4,5,6,7,8,9))
    }

    /**
      * +-------+-------+-------+
      * | 8 5 . | . . 2 | 4 . . |
      * | 7 2 . | . . . | . . 9 |
      * | . . 4 | . . . | . . . |
      * +-------+-------+-------+
      * | . . . | 1 . 7 | . . 2 |
      * | 3 . 5 | . . . | 9 . . |
      * | . 4 . | . . . | . . . |
      * +-------+-------+-------+
      * | . . . | . 8 . | . 7 . |
      * | . 1 7 | . . . | . . . |
      * | . . . | . 3 6 | . 4 . |
      * +-------+-------+-------+
      */
    "return associated neighborhood in an incomplete board" in {
      val str = "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4."
      val board = ArrayBasedBoard.parse(str)

      board.neighborhood(0, 0).filter(_.value.nonEmpty).flatMap(_.value).toSet mustEqual (Set[Int](8, 7, 5, 2, 4, 3))
      board.neighborhood(1, 5).filter(_.value.nonEmpty).flatMap(_.value).toSet mustEqual (Set[Int](1, 3, 5, 2, 4))
    }
  }

  "isFinished" should {

    "recognize complete board as finished" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isFinished mustEqual (true)
    }

    "recognize incomplete board as not finished" in {
      val str = "12345678945678912378912345623.567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isFinished mustEqual (false)
    }

  }

  "isCellValuesOk" should {

    "recognize cells of an empty board as valid" in {
      val str = "." * 81
      val board = ArrayBasedBoard.parse(str)

      board.isCellValuesOk mustEqual (true)
    }

    "recognize cells of a complete board as valid" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isCellValuesOk mustEqual (true)
    }

    "recognize an invalid cell" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str) + Cell(0, 0, Set.empty[Int])

      board.isCellValuesOk mustEqual (false)
    }

  }


  "isValid" should {

    "recognize empty board as valid" in {
      val str = "." * 81
      val board = ArrayBasedBoard.parse(str)

      board.isValid mustEqual (true)
    }

    "recognize valid and complete board as valid" in {
      val str = "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isValid mustEqual (true)
    }

    "recognize invalid board when there are duplicate values on a row" in {
      val str = "113456789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isValid mustEqual (false)
    }

    "recognize invalid board when there are duplicate values on a column" in {
      val str = "423156789456789123789123456234567891567891234891234567345678912678912345912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isValid mustEqual (false)
    }

    "recognize invalid board when there are duplicate values in a block" in {
      val str = "123456789234567891345678912456789123567891234678912345789123456891234567912345678"
      val board = ArrayBasedBoard.parse(str)

      board.isValid mustEqual (false)
    }
  }


  "+" should {

    "add cell to an empty board" in {
      val str = "." * 81
      val board = ArrayBasedBoard.parse(str) +
        Cell(0, 0, Set[Int](9)) +
        Cell(4, 2, Set[Int](3, 5, 1))

      board.cellIterator.length mustEqual (81)
      board.cell(0, 0).value.getOrElse(0) mustEqual (9)
      board.cell(0, 0).cands mustEqual (Set[Int](9))
      board.neighborhood(0, 0).filter(_.value.isEmpty).flatMap(_.cands).contains(9) mustEqual (true)

      board.cell(4, 2).value mustEqual (None)
      board.cell(4, 2).cands mustEqual (Set[Int](1, 3, 5))
      board.neighborhood(4, 2).filter(_.value.isEmpty).flatMap(_.cands).contains(5) mustEqual (true)
    }

  }

}
