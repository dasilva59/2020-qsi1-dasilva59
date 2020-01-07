type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty;

type pointsData = {
  playerOne: point,
  playerTwo: point
};
type fortyData = {
  player: player, /* The player who have forty points */
  otherPlayerPoint: point
};

type score =
| Points(pointsData)
| Forty(fortyData)
| Deuce
| Advantage(player)
| Game(player);


let scoreWhenDeuce: player => score = winner => Advantage(winner);
let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) => Game(winner);
  let other = player =>
  switch player {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };
  let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) =>
    advantagedPlayer == winner ? Game(winner) : Deuce;
    let scoreWhenForty = (current, winner) => Game(winner);
    let incrementPoint: point => option(point) =
  point =>
    switch point {
    | Love => Some(Fifteen)
    | Fifteen => Some(Thirty)
    | Thirty => None
    };
    let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );
    let pointTo = (player, point, current) =>
  switch player {
  | PlayerOne => {...current, playerOne: point}
  | PlayerTwo => {...current, playerTwo: point}
  };

let pointFor = (player, current) =>
  switch player {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };

let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None =>
    Forty({
      player: winner,
      otherPlayerPoint: current |> pointFor(other(winner))
    })
  };
  let scoreWhenGame = winner => Game(winner);
  let newGame = Points({playerOne: Love, playerTwo: Love});
  let score = (current, winner) =>
  switch current {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };

let string_of_player : (player) => string = p=>
switch p {
| PlayerOne => "Je suis le PlayerOne"
| PlayerTwo => "Je suis le PlayerTwo"
};

let string_of_point : (point)=>string = s=>
switch s {
  | Love => "Love"
  | Fifteen => "Fifteen points"
  | Thirty => "Thirty points";

};

let string_of_fortyData: (fortyData)=>string = s=>
switch s.player {
  | PlayerOne=> "Je suis player one avec 40 points, " ++ "Je suis player 2 avec " ++ string_of_point(s.otherPlayerPoint);
  | PlayerTwo=>"Je suis player two avec 40 points, " ++ "Je suis player 1 avec " ++ string_of_point(s.otherPlayerPoint);

};


 
let string_of_score : (score)=>string = s=>
switch s {
| Points(pointsData) => "Je suis le player one avec " ++ string_of_point(pointsData.playerOne) ++ "Je suis le player 2 avec " ++  string_of_point(pointsData.playerTwo)
| Forty(fortyData) => string_of_fortyData(fortyData)
| Deuce => "Egalité player 1 et player 2 avec 40 points."
| Advantage(player) => string_of_player(player) ++ " a l'avantage  "
| Game(player) =>string_of_player(player) ++ "a remporte le jeu" ;
};
