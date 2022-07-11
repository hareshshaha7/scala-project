/**
 * N-Queens
 * The eight queens problem is to place eight queens on a chessboard so that no queen is threatened by another.
 * In other words, there can't be two queens in the same row, column, or diagonal.
 * We now develop a solution for a chessboard of any size, not just 8.
 * One way to solve the problem is to place a queen on each row.
 * Once we have placed k - 1 queens, one must place the kth queen in a column where it's not "in check" with any other queen on the board.
 *
 *
 * We can solve this problem with a recursive algorithm:
 * Suppose that we have already generated all the Solutions consisting of placing k-1 queens on a board of size n.
 * Each solution is represented by a list (of length k-1 numbers of columns (between 0 and n-1).
 * The column number of the queen in the k-1th row comes first in the list, followed by the column number of the queen in row k-2 etc.
 * The solution set is thus represented as a set of lists, with one element for each solution
 * Now, to place kth queen, we generate all posible extensions of each solution preceded by a new queen:
 */

def check(col: Int, delta: Int, queens: List[Int]): Boolean =
  queens match
    case Nil => false
    case qcol :: others =>
      qcol == col || (qcol - col) == delta || check(col, delta + 1, others)

def isSafe(col: Int, queens: List[Int]): Boolean =
  !check(col, 1, queens)

def queens(n: Int) =
  def placeQueen(k: Int): Set[List[Int]] =
    if k == 0 then Set(List())
    else
      for
        queens <- placeQueen(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      yield col :: queens

  placeQueen(n)

queens(3)