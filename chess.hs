import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import Data.List
data PieceColor = White | Black deriving (Eq)
data PieceType = Pawn | PawnCanDoubleStep | PawnEnPassant
               | Knight | Bishop | Rook | Queen | King deriving (Eq)
data Piece = Piece { pieceColor :: PieceColor, pieceType  :: PieceType } deriving (Eq)
data Square = Square { piece :: Piece } | EmptySquare deriving (Eq)
data Pos = Pos { col :: Int, row :: Int } deriving (Eq)
type Board = [[Square]]
data GameState = GameState { board :: Board, turn :: PieceColor }

instance Show Piece where
  show (Piece White King)              = "?"
  show (Piece White Queen)             = "?"
  show (Piece White Rook)              = "?"
  show (Piece White Bishop)            = "?"
  show (Piece White Knight)            = "?"
  show (Piece White Pawn)              = "?"
  show (Piece White PawnCanDoubleStep) = "?"
  show (Piece White PawnEnPassant)     = "?"
  show (Piece Black King)              = "?"
  show (Piece Black Queen)             = "?"
  show (Piece Black Rook)              = "?"
  show (Piece Black Bishop)            = "?"
  show (Piece Black Knight)            = "?"
  show (Piece Black Pawn)              = "?"
  show (Piece Black PawnCanDoubleStep) = "?"
  show (Piece Black PawnEnPassant)     = "?"
instance Show Square where
  show (Square piece) = show piece
  show (EmptySquare)  = " "
instance Show Pos where
  show (Pos col row) = ['a'..'h'] !! col : show (8 - row)
prettyBoard :: Board -> String
prettyBoard board = intercalate "\n" [unwords [show square | square <- row] | row <- board]

getSquare :: Board -> Pos -> Square
getSquare board (Pos x y) = board !! y !! x

offsetPos :: Int -> Int -> Pos -> Pos
offsetPos dx dy (Pos x y) = Pos (x + dx) (y + dy)

initialBoard :: Board
initialBoard = [[ Square $ Piece Black Rook
                , Square $ Piece Black Knight
                , Square $ Piece Black Bishop
                , Square $ Piece Black Queen
                , Square $ Piece Black King
                , Square $ Piece Black Bishop
                , Square $ Piece Black Knight
                , Square $ Piece Black Rook
                ]
               ,[ Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                , Square $ Piece Black PawnCanDoubleStep
                ]
               ,[ EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                ]
               ,[ EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                ]
               ,[ EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                ]
               ,[ EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                , EmptySquare
                ]
               ,[ Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                , Square $ Piece White PawnCanDoubleStep
                ]
               ,[ Square $ Piece White Rook
                , Square $ Piece White Knight
                , Square $ Piece White Bishop
                , Square $ Piece White Queen
                , Square $ Piece White King
                , Square $ Piece White Bishop
                , Square $ Piece White Knight
                , Square $ Piece White Rook
               ]]

--moves :: Board -> Piece ->

--moves a =

pieceValue :: PieceColor -> Piece -> Int
pieceValue player (Piece pieceColor pieceType) =
  case pieceType of
    Pawn          -> 1
    PawnEnPassant -> 1
    Knight        -> 3
    Bishop        -> 3
    Rook          -> 5
    Queen         -> 9
    King          -> 1000
  * if player == pieceColor then 1 else -1

evalGameState :: PieceColor -> GameState -> Int
evalGameState player (GameState board turn) =
  sum [if square == EmptySquare then 0
       else (pieceValue turn (piece square)) | row <- board, square <- row]

-- region Moving pieces
validMoves :: Board -> Pos -> [[Maybe Board]]
validMoves board pos
  | square == EmptySquare              = noMoves
  | type_ == Knight                    = combineMoves basicKnightMoves color pos board
  | type_ == Bishop                    = combineMoves basicBishopMoves color pos board
  | type_ == Rook                      = combineMoves basicRookMoves color pos board
  | type_ == Queen                     = combineMoves (basicBishopMoves ++ basicRookMoves) color pos board
  | type_ == Rook                      = combineMoves (basicPawnMoves pawnMoveY) color pos board
  | type_ == PawnCanDoubleStep         = combineMoves (pawnDoubleStepMoves pawnMoveY) color pos board
  | type_ `elem` [Pawn, PawnEnPassant] = combineMoves (basicPawnMoves pawnMoveY) color pos board
  where noMoves = [[Nothing | _ <- [0..7]] | _ <- [0..7]]
        square = getSquare board pos
        piece_ = piece square
        type_ = pieceType piece_
        color = pieceColor piece_
        pawnMoveY = if color == White then -1 else 1

-- color is piece of CAPTURING piece (eg. If White is moving, then color = White)
type PieceMoveRule = PieceColor -> Pos -> Pos -> Board -> Maybe Board

--region Movement rule helper functions
combineMoves :: [PieceMoveRule] -> PieceColor -> Pos -> Board -> [[Maybe Board]]
combineMoves moveRules color pos board = [[
  firstValidMove [moveRule color pos (Pos x y) board | moveRule <- moveRules]
  | x <- [0..7]] | y <- [0..7]]
  where firstValidMove [] = Nothing
        firstValidMove (Nothing : maybeBoards) = firstValidMove maybeBoards
        firstValidMove (Just board : maybeBoards) = Just [[
          if square /= EmptySquare && pieceType (piece square) == PawnEnPassant
          then Square $ Piece (pieceColor $ piece square) Pawn else square
          | square <- row] | row <- board]

replaceSquare :: Pos -> Square -> Board -> Board
replaceSquare pos square board = [[
    if pos == Pos x y
      then square
      else getSquare board (Pos x y)
    | x <- [0..7]
  ] | y <- [0..7]]

moveSquare :: Pos -> Pos -> Board -> Board
moveSquare startPos endPos board =
  replaceSquare startPos EmptySquare $ replaceSquare endPos
  (if square /= EmptySquare && pieceType (piece square) `elem` [PawnCanDoubleStep, PawnEnPassant]
   then Square $ Piece (pieceColor $ piece square) Pawn else square)
  board
    where square = getSquare board startPos

simpleMoveTo :: Pos -> Pos -> Board -> Maybe Board
simpleMoveTo startPos endPos board = Just $ moveSquare startPos endPos board

pieceMoveGlide :: (Pos -> Pos) -> PieceMoveRule
pieceMoveGlide offset color startPos testPos board =
  if testPos `elem` (glide startPos) then simpleMoveTo startPos testPos board else Nothing
    where glide pos = if isInBounds pos && isNotColor color board pos
                      then pos : glide (offset pos) else []

pieceMoveStep :: (Pos -> Pos) -> PieceMoveRule
pieceMoveStep offset color pos testPos board =
  if newPos == testPos && isNotColor color board pos
  then simpleMoveTo pos newPos board else Nothing
    where newPos = offset pos

pieceMoveStepIfCapture :: (Pos -> Pos) -> PieceMoveRule
pieceMoveStepIfCapture offset color pos testPos board =
  if newPos == testPos &&
     not (isEmpty board newPos) &&
     isNotColor color board newPos
  then simpleMoveTo pos newPos board else Nothing
    where newPos = offset pos

pieceMoveStepIfNotCapture :: (Pos -> Pos) -> PieceMoveRule
pieceMoveStepIfNotCapture offset color pos testPos board =
  if newPos == testPos && isEmpty board pos
  then simpleMoveTo pos newPos board else Nothing
    where newPos = offset pos

pieceMoveStepIfEnPassant :: (Pos -> Pos) -> PieceMoveRule
pieceMoveStepIfEnPassant offset color pos testPos board =
  if newPos == testPos &&
     isEmpty board newPos &&
     pieceType (piece (getSquare board capturePos)) == PawnEnPassant
  then simpleMoveTo pos newPos board else Nothing
    where newPos = offset pos
          capturePos = Pos (col testPos) (row pos)

pieceMoveStepIfNotCaptureThrough :: (Pos -> Pos) -> (Pos -> Pos) -> PieceMoveRule
pieceMoveStepIfNotCaptureThrough offsetEnd offsetVia color pos testPos board =
  if newPos == testPos &&
     isEmpty board newPos &&
     isEmpty board (offsetVia pos)
  then simpleMoveTo pos newPos board else Nothing
    where newPos = offsetEnd pos

isInBounds :: Pos -> Bool
isInBounds (Pos x y) = 0 <= x && x < 8 && 0 <= y && y < 8

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = EmptySquare == getSquare board pos

isColor :: PieceColor -> Board -> Pos -> Bool
isColor color board pos = not $ isEmpty board pos
                       && pieceColor (piece $ getSquare board pos) == color

isNotColor :: PieceColor -> Board -> Pos -> Bool
isNotColor color board pos = isEmpty board pos
                          || pieceColor (piece $ getSquare board pos) /= color
-- endregion

--region Movement rules
basicKnightMoves :: [PieceMoveRule]
basicKnightMoves = [
                     pieceMoveStep $ offsetPos 1 2,
                     pieceMoveStep $ offsetPos 1 (-2),
                     pieceMoveStep $ offsetPos (-1) 2,
                     pieceMoveStep $ offsetPos (-1) (-2),
                     pieceMoveStep $ offsetPos 2 1,
                     pieceMoveStep $ offsetPos 2 (-1),
                     pieceMoveStep $ offsetPos (-2) 1,
                     pieceMoveStep $ offsetPos (-2) (-1)
                   ]
basicBishopMoves :: [PieceMoveRule]
basicBishopMoves = [
                     pieceMoveGlide $ offsetPos 1 1,
                     pieceMoveGlide $ offsetPos 1 (-1),
                     pieceMoveGlide $ offsetPos (-1) 1,
                     pieceMoveGlide $ offsetPos (-1) (-1)
                   ]
basicRookMoves :: [PieceMoveRule]
basicRookMoves = [
                   pieceMoveGlide $ offsetPos 0 1,
                   pieceMoveGlide $ offsetPos 0 (-1),
                   pieceMoveGlide $ offsetPos 1 0,
                   pieceMoveGlide $ offsetPos (-1) 0
                 ]
pawnDoubleStepMoves :: Int -> [PieceMoveRule]
pawnDoubleStepMoves pawnMoveY = pieceMoveStepIfNotCaptureThrough
                        (offsetPos 0 $ 2*pawnMoveY) (offsetPos 0 pawnMoveY)
                        : basicPawnMoves pawnMoveY
basicPawnMoves :: Int -> [PieceMoveRule]
basicPawnMoves pawnMoveY = [
                   pieceMoveStepIfNotCapture $ offsetPos 0 pawnMoveY,
                   pieceMoveStepIfCapture $ offsetPos (-1) pawnMoveY,
                   pieceMoveStepIfCapture $ offsetPos 1 pawnMoveY
                 ]
--endregion

--endregion

main = do
  putStr . fromString . prettyBoard $ initialBoard
