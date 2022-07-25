||| https://en.wikipedia.org/wiki/Template_method_pattern#PHP_example
module TemplateMethod

public export
record Game where
   constructor MkGame
   initialize: IO ()
   startPlay:  IO ()
   endPlay:    IO ()

export
(.play): Game -> IO ()
(.play) game = game.initialize >> game.startPlay >> game.endPlay

export
mario: Game
mario = MkGame {
   initialize = putStr "Mario Game Initialized! Press START\n" ,
   startPlay  = putStr "Mario Game Started. Enjoy the game!\n" ,
   endPlay    = putStr "Mario Game Finished!\n" }

export
tankfight: Game
tankfight = MkGame {
   initialize = putStr "Tankfight Game Initialized! Press START\n" ,
   startPlay  = putStr "Tankfight Game Started. Enjoy the game!\n" ,
   endPlay    = putStr "Tankfight Game Finished!\n" }

export game1, game2, game3: IO ()
game1 = tankfight.play
game2 = mario.play
game3 = .play$
   { startPlay $= (>> putStr "Episode 2: The Retankfightening\n")
   } tankfight
