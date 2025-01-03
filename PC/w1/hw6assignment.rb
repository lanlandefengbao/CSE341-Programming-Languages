# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces +
               [
                 rotations([[0, 0], [1, 0], [2, 0], [0, 1], [1, 1]]),
                 [
                   [[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],
                   [[0, -2], [0, -1], [0, 0], [0, 2], [0, 1]],
                 ],
                 rotations([[0, 0], [1, 0], [0, 1]]),
                 [[0,0]] # cheat
               ]

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces[0..-2].sample, board) #without cheating
  end

  def self.next_piece_c (board)
     MyPiece.new(All_My_Pieces[-1], board) #cheating
  end
  
end

class MyBoard < Board
  
  # your enhancements here
  
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_cur = false
    @cheat_nxt = false
  end
  
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    @cheat_cur = @cheat_nxt
    @cheat_nxt = false
    if @cheat_cur 
      @current_block = MyPiece.next_piece_c(self)
      @current_pos = nil
      @cheat_cur = false
    else
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    end
  end

  def cheat
    if @score >= 100 and !@cheat_nxt
      @score -= 100
      @cheat_nxt = true
    end
  end
  

  def store_current
    locations = @current_block.current_rotation # current piece of specific rotation : p
    displacement = @current_block.position # p's base_position
    # draw current piece to the board, square by square
    (0..(locations.length-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
    
end

class MyTetris < Tetris
  # your enhancements here    
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def key_bindings 
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end

end

