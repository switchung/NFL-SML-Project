(*Tristan and Nicks NFL "Offense" versus "Defense" data modeling project*)

datatype teamname = Name of string; (*team name*)
datatype perFor = perScore of int; (*percentage scored on a drive*)
datatype perAgainst = perAgainst of int; (*percentage scored AGAINST on a drive (so take 100- perAgainst to find percentage stops by defense)*)
datatype pointsFor = Score of int; (*total points made*)
datatype pointsAgainst = Stop of int; (*total points allowed*)
datatype record = Record of int * int; (*record of wins, losses*)

datatype team = Team of teamname * pointsFor * pointsAgainst * perFor * perAgainst * record;

val Bills =  Team(Name("Bills"), Score(137), Stop(251), perScore(24), perAgainst(38), Record(3, 7));
val Dolphins =  Team(Name("Dolphins"), Score(199), Stop(256), perScore(30), perAgainst(37), Record(5, 5));
val Jets =  Team(Name("Jets"), Score(208), Stop(254), perScore(30), perAgainst(34), Record(3, 7));
val Patriots =  Team(Name("Patriots"), Score(280), Stop(236), perScore(42), perAgainst(37), Record(7, 3));
val Ravens =  Team(Name("Ravens"), Score(213), Stop(160), perScore(39), perAgainst(29), Record(4, 5));
val Browns =  Team(Name("Browns"), Score(218), Stop(263), perScore(28), perAgainst(33), Record(3, 6));
val Bengals =  Team(Name("Bengals"), Score(235), Stop(288), perScore(38), perAgainst(50), Record(5,4));
val Steelers = Team(Name("Steelers"), Score(279), Stop(209), perScore(39), perAgainst(34), Record(6, 2));
val Texans = Team(Name("Texans"), Score(216), Stop(184), perScore(37), perAgainst(34), Record(6, 3));
val Colts = Team(Name("Colts"), Score(260), Stop(239), perScore(43), perAgainst(43), Record(4, 5));
val Jaguars = Team(Name("Jaguars"), Score(160), Stop(199), perScore(30), perAgainst(39), Record(3, 6));
val Titans = Team(Name("Titans"), Score(168), Stop(151), perScore(37), perAgainst(31), Record(5, 4));
val Broncos = Team(Name("Broncos"), Score(205), Stop(213), perScore(33), perAgainst(37), Record(3, 6));
val Chiefs = Team(Name("Chiefs"), Score(353), Stop(240), perScore(55), perAgainst(40), Record(9, 1));
val Chargers = Team(Name("Chargers"), Score(240), Stop(186), perScore(44), perAgainst(33), Record(7, 2));
val Raiders = Team(Name("Raiders"), Score(147), Stop(272), perScore(29), perAgainst(50), Record(1, 8));
val Cowboys = Team(Name("Cowboys"), Score(181), Stop(171), perScore(38), perAgainst(32), Record(4, 5));
val Giants = Team(Name("Giants"), Score(177), Stop(228), perScore(37), perAgainst(46), Record(2, 7));
val Eagles = Team(Name("Eagles"), Score(198), Stop(183), perScore(36), perAgainst(36), Record(4, 5));
val Redskins = Team(Name("Redskins"), Score(176), Stop(175), perScore(36), perAgainst(31), Record(6, 3));
val Bears = Team(Name("Bears"), Score(269), Stop(175), perScore(41), perAgainst(28), Record(6, 3));
val Lions = Team(Name("Lions"), Score(202), Stop(244), perScore(39), perAgainst(42), Record(3, 6));
val Packers = Team(Name("Packers"), Score(223), Stop(216), perScore(41), perAgainst(36), Record(4, 4));
val Vikings = Team(Name("Vikings"), Score(221), Stop(204), perScore(33), perAgainst(37), Record(5, 3));
val Saints = Team(Name("Saints"), Score(330), Stop(232), perScore(61), perAgainst(41), Record(8, 1));
val Cardinal = Team(Name("Cardinals"), Score(124), Stop(225), perScore(17), perAgainst(36), Record(2, 7));
val Rams = Team(Name("Rams"), Score(335), Stop(231), perScore(52), perAgainst(39), Record(9, 1));
val FourtyNiners = Team(Name("FourtyNiners"), Score(230), Stop(266), perScore(39), perAgainst(40), Record(2, 8));
val Seahawks = Team(Name("Seahawks"), Score(219), Stop(192), perScore(37), perAgainst(34), Record(4, 5));
val Panthers = Team(Name("Panthers"), Score(241), Stop(232), perScore(40), perAgainst(39), Record(6,3));
val Falcons = Team(Name("Falcons"), Score(244), Stop(254), perScore(46), perAgainst(44), Record(4,5));
val Buccaneers = Team(Name("Buccaneers"), Score(232), Stop(291), perScore(36), perAgainst(43), Record(3,6));

val NFL = [Bills, Dolphins, Jets, Patriots, Ravens, Browns, Bengals, Steelers, Texans, Colts, Jaguars, Titans, Broncos, Chiefs, Chargers, Raiders, Cowboys, Giants, Eagles, Redskins, Bears, Lions, Packers, Vikings, Saints, Cardinal, Rams, FourtyNiners, Seahawks, Panthers, Falcons, Buccaneers];





(*Calculations based on scoring/defensive percentages*)
fun foldr(func, [], seed) = seed
  | foldr(func, x::rest, seed) = 
    func(x, foldr(func,rest,seed));

(*Run through the NFL and find average percentage successful drives*)
fun meanOffense(NFLteams) = foldr(fn(Team(_,_,_, perScore(x), _, _), y)=> x + y, NFLteams, 0) div 32;

(*Run through the NFL and find average percentage opponent successful drives*)
fun meanDefense(NFLteams) = foldr(fn(Team(_,_,_,_, perAgainst(x), _), y)=> x + y, NFLteams, 0) div 32;

val offenseMean = meanOffense(NFL);
val defenseMean = meanDefense(NFL);

(*based on the average offense and defense values from above, see if your offense or defense is better*)
fun offenseBetterThanDefense(x, y, offenseMean, defenseMean) =
    (x - offenseMean) > (defenseMean - y);

(*If offense is better, than add your total number of wins to the tally, if your defense is better, add total wins to the tally*)
(*Uses a moded version of foldr that will return a tupil of offensive wins and defensive wins*)
fun modFoldr(func, [], (offense,defense)) = (offense,defense)
  | modFoldr(func, x::rest, (offense,defense)) = 
    func(x, modFoldr(func,rest, (offense,defense)));

fun winCounter(NFLteams, meanO, meanD) = modFoldr(fn(Team(_,_,_, perScore(x), perAgainst(y), Record(wins, _)), (offense,defense))=> if offenseBetterThanDefense(x, y, meanO, meanD) then (offense + wins, defense) else (offense,defense + wins), NFLteams, (0,0));


(*Compare the total wins for teams with better defense versus offense and return a string of which has more wins*)
fun offenseVersusDefense(NFLteams, meanO, meanD) = 
    let val winTuple = winCounter(NFLteams, meanO, meanD)
        val offensiveWins = #1winTuple
        val defensiveWins = #2winTuple
    in if offensiveWins > defensiveWins then "offense" else "defense"
    end;




(*Calculations based on total points for and against*)


(*Same only using the total points for and against in each function, thus necesitating new functions*)
fun meanOffensePoints(NFLteams) = foldr(fn(Team(_,Score(x),_,_, _, _), y)=> x + y, NFLteams, 0) div 32;

fun meanDefensePoints(NFLteams) = foldr(fn(Team(_,_,Stop(x),_, _, _), y)=> x + y, NFLteams, 0) div 32;

val offenseMeanPoints = meanOffensePoints(NFL);
val defenseMeanPoints = meanDefensePoints(NFL);

fun winCounterPoints(NFLteams, meanO, meanD) = modFoldr(fn(Team(_,Score(x),Stop(y),_,_, Record(wins, _)), (offense,defense))=> if offenseBetterThanDefense(x, y, meanO, meanD) then (offense + wins, defense) else (offense,defense + wins), NFLteams, (0,0));

fun offenseVersusDefensePoints(NFLteams, meanO, meanD) = 
    let val winTuple = winCounterPoints(NFLteams, meanO, meanD)
        val offensiveWins = #1winTuple
        val defensiveWins = #2winTuple
    in if offensiveWins > defensiveWins then "offense" else "defense"
    end;






(*Offense or defense based on the two calculations:*)
fun print(NFLteams, omean, dmean, omeanpoints, dmeanpoints) = 
    let val OorD = offenseVersusDefense(NFLteams, omean, dmean)
        val scores = winCounter(NFLteams, omean, dmean)
        val OorDPoints = offenseVersusDefensePoints(NFLteams, omeanpoints, dmeanpoints)
        val scoresPoints = winCounterPoints(NFLteams, omeanpoints, dmeanpoints)
    in "According to calculations based on percentage of successful drives or stops, " ^ OorD ^ "won more games. Teams with better offense won " ^ Int.toString(#1scores) ^ " and teams with a better defense won " ^ Int.toString(#2scores) ^ ". Calculations based on points scored or allowed, " ^ OorDPoints  ^ "won more games. Teams with better offense won " ^ Int.toString(#1scoresPoints) ^ " and teams with a better defense won " ^ Int.toString(#2scoresPoints) ^ "."
    end;


print(NFL, offenseMean, defenseMean, offenseMeanPoints, defenseMeanPoints);