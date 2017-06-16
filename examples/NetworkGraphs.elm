module NetworkGraphs exposing (miserablesGraph, pollbooksGraph)

import Graph


type alias PollbookNode =
    { label : String
    , kind : String
    }


pollbooksGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ PollbookNode "1000 Years for Revenge" "n"
        , PollbookNode "Bush vs. the Beltway" "c"
        , PollbookNode "Charlie Wilson's War" "c"
        , PollbookNode "Losing Bin Laden" "c"
        , PollbookNode "Sleeping With the Devil" "n"
        , PollbookNode "The Man Who Warned America" "c"
        , PollbookNode "Why America Slept" "n"
        , PollbookNode "Ghost Wars" "n"
        , PollbookNode "A National Party No More" "c"
        , PollbookNode "Bush Country" "c"
        , PollbookNode "Dereliction of Duty" "c"
        , PollbookNode "Legacy" "c"
        , PollbookNode "Off with Their Heads" "c"
        , PollbookNode "Persecution" "c"
        , PollbookNode "Rumsfeld's War" "c"
        , PollbookNode "Breakdown" "c"
        , PollbookNode "Betrayal" "c"
        , PollbookNode "Shut Up and Sing" "c"
        , PollbookNode "Meant To Be" "n"
        , PollbookNode "The Right Man" "c"
        , PollbookNode "Ten Minutes from Normal" "c"
        , PollbookNode "Hillary's Scheme" "c"
        , PollbookNode "The French Betrayal of America" "c"
        , PollbookNode "Tales from the Left Coast" "c"
        , PollbookNode "Hating America" "c"
        , PollbookNode "The Third Terrorist" "c"
        , PollbookNode "Endgame" "c"
        , PollbookNode "Spin Sisters" "c"
        , PollbookNode "All the Shah's Men" "n"
        , PollbookNode "Dangerous Dimplomacy" "c"
        , PollbookNode "The Price of Loyalty" "l"
        , PollbookNode "House of Bush, House of Saud" "l"
        , PollbookNode "The Death of Right and Wrong" "c"
        , PollbookNode "Useful Idiots" "c"
        , PollbookNode "The O'Reilly Factor" "c"
        , PollbookNode "Let Freedom Ring" "c"
        , PollbookNode "Those Who Trespass" "c"
        , PollbookNode "Bias" "c"
        , PollbookNode "Slander" "c"
        , PollbookNode "The Savage Nation" "c"
        , PollbookNode "Deliver Us from Evil" "c"
        , PollbookNode "Give Me a Break" "c"
        , PollbookNode "The Enemy Within" "c"
        , PollbookNode "The Real America" "c"
        , PollbookNode "Who's Looking Out for You?" "c"
        , PollbookNode "The Official Handbook Vast Right Wing Conspiracy" "c"
        , PollbookNode "Power Plays" "n"
        , PollbookNode "Arrogance" "c"
        , PollbookNode "The Perfect Wife" "n"
        , PollbookNode "The Bushes" "c"
        , PollbookNode "Things Worth Fighting For" "c"
        , PollbookNode "Surprise, Security, the American Experience" "n"
        , PollbookNode "Allies" "c"
        , PollbookNode "Why Courage Matters" "c"
        , PollbookNode "Hollywood Interrupted" "c"
        , PollbookNode "Fighting Back" "c"
        , PollbookNode "We Will Prevail" "c"
        , PollbookNode "The Faith of George W Bush" "c"
        , PollbookNode "Rise of the Vulcans" "c"
        , PollbookNode "Downsize This!" "l"
        , PollbookNode "Stupid White Men" "l"
        , PollbookNode "Rush Limbaugh Is a Big Fat Idiot" "l"
        , PollbookNode "The Best Democracy Money Can Buy" "l"
        , PollbookNode "The Culture of Fear" "l"
        , PollbookNode "America Unbound" "l"
        , PollbookNode "The Choice" "l"
        , PollbookNode "The Great Unraveling" "l"
        , PollbookNode "Rogue Nation" "l"
        , PollbookNode "Soft Power" "l"
        , PollbookNode "Colossus" "n"
        , PollbookNode "The Sorrows of Empire" "l"
        , PollbookNode "Against All Enemies" "l"
        , PollbookNode "American Dynasty" "l"
        , PollbookNode "Big Lies" "l"
        , PollbookNode "The Lies of George W. Bush" "l"
        , PollbookNode "Worse Than Watergate" "l"
        , PollbookNode "Plan of Attack" "n"
        , PollbookNode "Bush at War" "c"
        , PollbookNode "The New Pearl Harbor" "l"
        , PollbookNode "Bushwomen" "l"
        , PollbookNode "The Bubble of American Supremacy" "l"
        , PollbookNode "Living History" "l"
        , PollbookNode "The Politics of Truth" "l"
        , PollbookNode "Fanatics and Fools" "l"
        , PollbookNode "Bushwhacked" "l"
        , PollbookNode "Disarming Iraq" "l"
        , PollbookNode "Lies and the Lying Liars Who Tell Them" "l"
        , PollbookNode "MoveOn's 50 Ways to Love Your Country" "l"
        , PollbookNode "The Buying of the President 2004" "l"
        , PollbookNode "Perfectly Legal" "l"
        , PollbookNode "Hegemony or Survival" "l"
        , PollbookNode "The Exception to the Rulers" "l"
        , PollbookNode "Freethinkers" "l"
        , PollbookNode "Had Enough?" "l"
        , PollbookNode "It's Still the Economy, Stupid!" "l"
        , PollbookNode "We're Right They're Wrong" "l"
        , PollbookNode "What Liberal Media?" "l"
        , PollbookNode "The Clinton Wars" "l"
        , PollbookNode "Weapons of Mass Deception" "l"
        , PollbookNode "Dude, Where's My Country?" "l"
        , PollbookNode "Thieves in High Places" "l"
        , PollbookNode "Shrub" "l"
        , PollbookNode "Buck Up Suck Up" "l"
        , PollbookNode "The Future of Freedom" "n"
        , PollbookNode "Empire" "n"
        ]
        [ ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 3, 1 )
        , ( 4, 0 )
        , ( 4, 2 )
        , ( 5, 0 )
        , ( 5, 1 )
        , ( 5, 2 )
        , ( 5, 3 )
        , ( 5, 4 )
        , ( 6, 0 )
        , ( 6, 1 )
        , ( 6, 4 )
        , ( 6, 5 )
        , ( 7, 2 )
        , ( 7, 5 )
        , ( 7, 6 )
        , ( 8, 3 )
        , ( 9, 3 )
        , ( 9, 8 )
        , ( 10, 3 )
        , ( 10, 6 )
        , ( 10, 8 )
        , ( 11, 3 )
        , ( 11, 8 )
        , ( 11, 9 )
        , ( 11, 10 )
        , ( 12, 3 )
        , ( 12, 6 )
        , ( 12, 8 )
        , ( 12, 9 )
        , ( 12, 10 )
        , ( 12, 11 )
        , ( 13, 3 )
        , ( 13, 8 )
        , ( 13, 11 )
        , ( 13, 12 )
        , ( 14, 3 )
        , ( 14, 8 )
        , ( 14, 9 )
        , ( 14, 11 )
        , ( 14, 12 )
        , ( 14, 7 )
        , ( 15, 3 )
        , ( 15, 10 )
        , ( 15, 12 )
        , ( 16, 3 )
        , ( 16, 10 )
        , ( 16, 15 )
        , ( 17, 3 )
        , ( 17, 11 )
        , ( 17, 12 )
        , ( 17, 13 )
        , ( 18, 3 )
        , ( 18, 6 )
        , ( 18, 12 )
        , ( 19, 3 )
        , ( 19, 10 )
        , ( 20, 3 )
        , ( 20, 8 )
        , ( 20, 9 )
        , ( 20, 11 )
        , ( 21, 3 )
        , ( 21, 8 )
        , ( 21, 10 )
        , ( 21, 11 )
        , ( 22, 3 )
        , ( 22, 6 )
        , ( 22, 8 )
        , ( 22, 11 )
        , ( 23, 3 )
        , ( 23, 8 )
        , ( 23, 12 )
        , ( 23, 21 )
        , ( 24, 3 )
        , ( 24, 8 )
        , ( 24, 9 )
        , ( 24, 12 )
        , ( 24, 20 )
        , ( 25, 3 )
        , ( 25, 6 )
        , ( 25, 14 )
        , ( 25, 22 )
        , ( 26, 3 )
        , ( 26, 8 )
        , ( 26, 11 )
        , ( 26, 14 )
        , ( 26, 24 )
        , ( 27, 3 )
        , ( 27, 8 )
        , ( 27, 9 )
        , ( 27, 11 )
        , ( 27, 23 )
        , ( 28, 4 )
        , ( 29, 4 )
        , ( 29, 6 )
        , ( 29, 11 )
        , ( 29, 13 )
        , ( 30, 4 )
        , ( 30, 7 )
        , ( 31, 4 )
        , ( 31, 30 )
        , ( 32, 8 )
        , ( 32, 12 )
        , ( 32, 13 )
        , ( 32, 23 )
        , ( 33, 32 )
        , ( 33, 8 )
        , ( 33, 10 )
        , ( 33, 12 )
        , ( 33, 23 )
        , ( 35, 34 )
        , ( 35, 8 )
        , ( 35, 10 )
        , ( 36, 34 )
        , ( 36, 35 )
        , ( 36, 12 )
        , ( 37, 34 )
        , ( 37, 8 )
        , ( 37, 10 )
        , ( 37, 35 )
        , ( 37, 33 )
        , ( 38, 34 )
        , ( 38, 10 )
        , ( 38, 35 )
        , ( 38, 12 )
        , ( 38, 37 )
        , ( 38, 33 )
        , ( 39, 34 )
        , ( 39, 10 )
        , ( 39, 35 )
        , ( 39, 12 )
        , ( 39, 38 )
        , ( 39, 33 )
        , ( 40, 8 )
        , ( 40, 35 )
        , ( 40, 12 )
        , ( 40, 13 )
        , ( 40, 20 )
        , ( 40, 22 )
        , ( 40, 39 )
        , ( 40, 24 )
        , ( 40, 25 )
        , ( 40, 26 )
        , ( 40, 27 )
        , ( 41, 8 )
        , ( 41, 9 )
        , ( 41, 40 )
        , ( 41, 12 )
        , ( 41, 36 )
        , ( 41, 27 )
        , ( 42, 8 )
        , ( 42, 40 )
        , ( 42, 13 )
        , ( 42, 39 )
        , ( 43, 8 )
        , ( 43, 35 )
        , ( 43, 13 )
        , ( 43, 42 )
        , ( 44, 8 )
        , ( 44, 40 )
        , ( 44, 35 )
        , ( 44, 12 )
        , ( 44, 13 )
        , ( 45, 8 )
        , ( 45, 9 )
        , ( 45, 40 )
        , ( 45, 11 )
        , ( 45, 26 )
        , ( 46, 8 )
        , ( 46, 12 )
        , ( 47, 9 )
        , ( 47, 40 )
        , ( 47, 41 )
        , ( 47, 11 )
        , ( 47, 12 )
        , ( 47, 13 )
        , ( 47, 42 )
        , ( 47, 36 )
        , ( 47, 37 )
        , ( 47, 17 )
        , ( 47, 33 )
        , ( 47, 45 )
        , ( 47, 46 )
        , ( 47, 23 )
        , ( 47, 24 )
        , ( 47, 26 )
        , ( 47, 27 )
        , ( 48, 9 )
        , ( 48, 20 )
        , ( 49, 9 )
        , ( 49, 20 )
        , ( 49, 31 )
        , ( 49, 48 )
        , ( 50, 9 )
        , ( 50, 11 )
        , ( 51, 9 )
        , ( 52, 9 )
        , ( 52, 22 )
        , ( 52, 51 )
        , ( 53, 40 )
        , ( 53, 20 )
        , ( 53, 24 )
        , ( 53, 26 )
        , ( 54, 40 )
        , ( 54, 41 )
        , ( 54, 12 )
        , ( 54, 47 )
        , ( 54, 23 )
        , ( 54, 27 )
        , ( 55, 10 )
        , ( 55, 12 )
        , ( 55, 15 )
        , ( 55, 19 )
        , ( 56, 11 )
        , ( 56, 19 )
        , ( 56, 43 )
        , ( 57, 13 )
        , ( 57, 56 )
        , ( 57, 20 )
        , ( 57, 48 )
        , ( 57, 49 )
        , ( 58, 14 )
        , ( 58, 7 )
        , ( 58, 30 )
        , ( 58, 49 )
        , ( 58, 50 )
        , ( 58, 51 )
        , ( 58, 52 )
        , ( 60, 59 )
        , ( 61, 59 )
        , ( 62, 59 )
        , ( 62, 60 )
        , ( 63, 59 )
        , ( 63, 60 )
        , ( 63, 62 )
        , ( 64, 58 )
        , ( 64, 51 )
        , ( 64, 52 )
        , ( 65, 64 )
        , ( 65, 58 )
        , ( 65, 51 )
        , ( 66, 64 )
        , ( 66, 28 )
        , ( 66, 30 )
        , ( 67, 64 )
        , ( 67, 65 )
        , ( 67, 66 )
        , ( 67, 30 )
        , ( 68, 64 )
        , ( 68, 58 )
        , ( 68, 65 )
        , ( 69, 64 )
        , ( 69, 58 )
        , ( 69, 65 )
        , ( 69, 51 )
        , ( 70, 64 )
        , ( 70, 66 )
        , ( 70, 30 )
        , ( 71, 7 )
        , ( 71, 68 )
        , ( 71, 70 )
        , ( 72, 71 )
        , ( 72, 28 )
        , ( 72, 66 )
        , ( 72, 49 )
        , ( 72, 70 )
        , ( 73, 71 )
        , ( 73, 72 )
        , ( 73, 66 )
        , ( 73, 30 )
        , ( 73, 31 )
        , ( 74, 71 )
        , ( 74, 72 )
        , ( 74, 73 )
        , ( 74, 66 )
        , ( 74, 30 )
        , ( 74, 31 )
        , ( 75, 71 )
        , ( 75, 72 )
        , ( 75, 73 )
        , ( 75, 74 )
        , ( 75, 30 )
        , ( 75, 31 )
        , ( 75, 70 )
        , ( 76, 71 )
        , ( 76, 72 )
        , ( 76, 75 )
        , ( 76, 66 )
        , ( 76, 30 )
        , ( 76, 31 )
        , ( 76, 49 )
        , ( 76, 53 )
        , ( 77, 71 )
        , ( 77, 75 )
        , ( 77, 58 )
        , ( 77, 30 )
        , ( 77, 19 )
        , ( 77, 31 )
        , ( 77, 76 )
        , ( 78, 71 )
        , ( 78, 72 )
        , ( 78, 74 )
        , ( 78, 75 )
        , ( 78, 31 )
        , ( 79, 71 )
        , ( 79, 72 )
        , ( 79, 74 )
        , ( 79, 75 )
        , ( 79, 30 )
        , ( 80, 71 )
        , ( 80, 72 )
        , ( 80, 66 )
        , ( 80, 30 )
        , ( 81, 71 )
        , ( 82, 71 )
        , ( 82, 72 )
        , ( 82, 73 )
        , ( 82, 74 )
        , ( 82, 75 )
        , ( 82, 30 )
        , ( 82, 31 )
        , ( 82, 76 )
        , ( 83, 71 )
        , ( 83, 73 )
        , ( 83, 75 )
        , ( 83, 30 )
        , ( 83, 76 )
        , ( 84, 72 )
        , ( 84, 74 )
        , ( 84, 75 )
        , ( 84, 66 )
        , ( 84, 73 )
        , ( 84, 60 )
        , ( 84, 30 )
        , ( 84, 62 )
        , ( 84, 76 )
        , ( 84, 79 )
        , ( 84, 81 )
        , ( 84, 82 )
        , ( 84, 83 )
        , ( 85, 72 )
        , ( 85, 7 )
        , ( 85, 58 )
        , ( 85, 65 )
        , ( 85, 66 )
        , ( 86, 72 )
        , ( 86, 73 )
        , ( 86, 66 )
        , ( 86, 84 )
        , ( 86, 60 )
        , ( 86, 30 )
        , ( 86, 61 )
        , ( 86, 76 )
        , ( 86, 81 )
        , ( 87, 72 )
        , ( 87, 74 )
        , ( 87, 84 )
        , ( 87, 83 )
        , ( 88, 72 )
        , ( 88, 74 )
        , ( 88, 66 )
        , ( 88, 84 )
        , ( 89, 72 )
        , ( 89, 73 )
        , ( 89, 66 )
        , ( 89, 84 )
        , ( 89, 86 )
        , ( 89, 88 )
        , ( 90, 72 )
        , ( 90, 66 )
        , ( 90, 70 )
        , ( 91, 72 )
        , ( 91, 74 )
        , ( 91, 75 )
        , ( 91, 31 )
        , ( 91, 90 )
        , ( 91, 79 )
        , ( 92, 72 )
        , ( 92, 73 )
        , ( 92, 75 )
        , ( 93, 73 )
        , ( 93, 66 )
        , ( 93, 86 )
        , ( 93, 30 )
        , ( 94, 73 )
        , ( 94, 84 )
        , ( 94, 93 )
        , ( 95, 73 )
        , ( 95, 94 )
        , ( 95, 61 )
        , ( 96, 73 )
        , ( 96, 66 )
        , ( 96, 84 )
        , ( 96, 94 )
        , ( 97, 73 )
        , ( 97, 66 )
        , ( 97, 84 )
        , ( 97, 86 )
        , ( 97, 96 )
        , ( 97, 81 )
        , ( 98, 73 )
        , ( 98, 74 )
        , ( 98, 87 )
        , ( 98, 91 )
        , ( 99, 73 )
        , ( 99, 74 )
        , ( 99, 66 )
        , ( 99, 84 )
        , ( 99, 93 )
        , ( 99, 60 )
        , ( 99, 59 )
        , ( 99, 30 )
        , ( 99, 62 )
        , ( 99, 63 )
        , ( 99, 90 )
        , ( 100, 73 )
        , ( 100, 66 )
        , ( 100, 84 )
        , ( 100, 86 )
        , ( 100, 96 )
        , ( 100, 98 )
        , ( 100, 99 )
        , ( 100, 62 )
        , ( 100, 79 )
        , ( 100, 91 )
        , ( 100, 83 )
        , ( 101, 84 )
        , ( 101, 86 )
        , ( 101, 94 )
        , ( 101, 100 )
        , ( 101, 61 )
        , ( 102, 93 )
        , ( 102, 94 )
        , ( 102, 95 )
        , ( 102, 46 )
        , ( 103, 67 )
        , ( 104, 67 )
        , ( 104, 69 )
        , ( 104, 103 )
        ]


miserablesGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Myriel"
        , "Napoleon"
        , "Mlle.Baptistine"
        , "Mme.Magloire"
        , "CountessdeLo"
        , "Geborand"
        , "Champtercier"
        , "Cravatte"
        , "Count"
        , "OldMan"
        , "Labarre"
        , "Valjean"
        , "Marguerite"
        , "Mme.deR"
        , "Isabeau"
        , "Gervais"
        , "Tholomyes"
        , "Listolier"
        , "Fameuil"
        , "Blacheville"
        , "Favourite"
        , "Dahlia"
        , "Zephine"
        , "Fantine"
        , "Mme.Thenardier"
        , "Thenardier"
        , "Cosette"
        , "Javert"
        , "Fauchelevent"
        , "Bamatabois"
        , "Perpetue"
        , "Simplice"
        , "Scaufflaire"
        , "Woman1"
        , "Judge"
        , "Champmathieu"
        , "Brevet"
        , "Chenildieu"
        , "Cochepaille"
        , "Pontmercy"
        , "Boulatruelle"
        , "Eponine"
        , "Anzelma"
        , "Woman2"
        , "MotherInnocent"
        , "Gribier"
        , "Jondrette"
        , "Mme.Burgon"
        , "Gavroche"
        , "Gillenormand"
        , "Magnon"
        , "Mlle.Gillenormand"
        , "Mme.Pontmercy"
        , "Mlle.Vaubois"
        , "Lt.Gillenormand"
        , "Marius"
        , "BaronessT"
        , "Mabeuf"
        , "Enjolras"
        , "Combeferre"
        , "Prouvaire"
        , "Feuilly"
        , "Courfeyrac"
        , "Bahorel"
        , "Bossuet"
        , "Joly"
        , "Grantaire"
        , "MotherPlutarch"
        , "Gueulemer"
        , "Babet"
        , "Claquesous"
        , "Montparnasse"
        , "Toussaint"
        , "Child1"
        , "Child2"
        , "Brujon"
        , "Mme.Hucheloup"
        ]
        [ ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 3, 2 )
        , ( 4, 0 )
        , ( 5, 0 )
        , ( 6, 0 )
        , ( 7, 0 )
        , ( 8, 0 )
        , ( 9, 0 )
        , ( 11, 10 )
        , ( 11, 3 )
        , ( 11, 2 )
        , ( 11, 0 )
        , ( 12, 11 )
        , ( 13, 11 )
        , ( 14, 11 )
        , ( 15, 11 )
        , ( 17, 16 )
        , ( 18, 16 )
        , ( 18, 17 )
        , ( 19, 16 )
        , ( 19, 17 )
        , ( 19, 18 )
        , ( 20, 16 )
        , ( 20, 17 )
        , ( 20, 18 )
        , ( 20, 19 )
        , ( 21, 16 )
        , ( 21, 17 )
        , ( 21, 18 )
        , ( 21, 19 )
        , ( 21, 20 )
        , ( 22, 16 )
        , ( 22, 17 )
        , ( 22, 18 )
        , ( 22, 19 )
        , ( 22, 20 )
        , ( 22, 21 )
        , ( 23, 16 )
        , ( 23, 17 )
        , ( 23, 18 )
        , ( 23, 19 )
        , ( 23, 20 )
        , ( 23, 21 )
        , ( 23, 22 )
        , ( 23, 12 )
        , ( 23, 11 )
        , ( 24, 23 )
        , ( 24, 11 )
        , ( 25, 24 )
        , ( 25, 23 )
        , ( 25, 11 )
        , ( 26, 24 )
        , ( 26, 11 )
        , ( 26, 16 )
        , ( 26, 25 )
        , ( 27, 11 )
        , ( 27, 23 )
        , ( 27, 25 )
        , ( 27, 24 )
        , ( 27, 26 )
        , ( 28, 11 )
        , ( 28, 27 )
        , ( 29, 23 )
        , ( 29, 27 )
        , ( 29, 11 )
        , ( 30, 23 )
        , ( 31, 30 )
        , ( 31, 11 )
        , ( 31, 23 )
        , ( 31, 27 )
        , ( 32, 11 )
        , ( 33, 11 )
        , ( 33, 27 )
        , ( 34, 11 )
        , ( 34, 29 )
        , ( 35, 11 )
        , ( 35, 34 )
        , ( 35, 29 )
        , ( 36, 34 )
        , ( 36, 35 )
        , ( 36, 11 )
        , ( 36, 29 )
        , ( 37, 34 )
        , ( 37, 35 )
        , ( 37, 36 )
        , ( 37, 11 )
        , ( 37, 29 )
        , ( 38, 34 )
        , ( 38, 35 )
        , ( 38, 36 )
        , ( 38, 37 )
        , ( 38, 11 )
        , ( 38, 29 )
        , ( 39, 25 )
        , ( 40, 25 )
        , ( 41, 24 )
        , ( 41, 25 )
        , ( 42, 41 )
        , ( 42, 25 )
        , ( 42, 24 )
        , ( 43, 11 )
        , ( 43, 26 )
        , ( 43, 27 )
        , ( 44, 28 )
        , ( 44, 11 )
        , ( 45, 28 )
        , ( 47, 46 )
        , ( 48, 47 )
        , ( 48, 25 )
        , ( 48, 27 )
        , ( 48, 11 )
        , ( 49, 26 )
        , ( 49, 11 )
        , ( 50, 49 )
        , ( 50, 24 )
        , ( 51, 49 )
        , ( 51, 26 )
        , ( 51, 11 )
        , ( 52, 51 )
        , ( 52, 39 )
        , ( 53, 51 )
        , ( 54, 51 )
        , ( 54, 49 )
        , ( 54, 26 )
        , ( 55, 51 )
        , ( 55, 49 )
        , ( 55, 39 )
        , ( 55, 54 )
        , ( 55, 26 )
        , ( 55, 11 )
        , ( 55, 16 )
        , ( 55, 25 )
        , ( 55, 41 )
        , ( 55, 48 )
        , ( 56, 49 )
        , ( 56, 55 )
        , ( 57, 55 )
        , ( 57, 41 )
        , ( 57, 48 )
        , ( 58, 55 )
        , ( 58, 48 )
        , ( 58, 27 )
        , ( 58, 57 )
        , ( 58, 11 )
        , ( 59, 58 )
        , ( 59, 55 )
        , ( 59, 48 )
        , ( 59, 57 )
        , ( 60, 48 )
        , ( 60, 58 )
        , ( 60, 59 )
        , ( 61, 48 )
        , ( 61, 58 )
        , ( 61, 60 )
        , ( 61, 59 )
        , ( 61, 57 )
        , ( 61, 55 )
        , ( 62, 55 )
        , ( 62, 58 )
        , ( 62, 59 )
        , ( 62, 48 )
        , ( 62, 57 )
        , ( 62, 41 )
        , ( 62, 61 )
        , ( 62, 60 )
        , ( 63, 59 )
        , ( 63, 48 )
        , ( 63, 62 )
        , ( 63, 57 )
        , ( 63, 58 )
        , ( 63, 61 )
        , ( 63, 60 )
        , ( 63, 55 )
        , ( 64, 55 )
        , ( 64, 62 )
        , ( 64, 48 )
        , ( 64, 63 )
        , ( 64, 58 )
        , ( 64, 61 )
        , ( 64, 60 )
        , ( 64, 59 )
        , ( 64, 57 )
        , ( 64, 11 )
        , ( 65, 63 )
        , ( 65, 64 )
        , ( 65, 48 )
        , ( 65, 62 )
        , ( 65, 58 )
        , ( 65, 61 )
        , ( 65, 60 )
        , ( 65, 59 )
        , ( 65, 57 )
        , ( 65, 55 )
        , ( 66, 64 )
        , ( 66, 58 )
        , ( 66, 59 )
        , ( 66, 62 )
        , ( 66, 65 )
        , ( 66, 48 )
        , ( 66, 63 )
        , ( 66, 61 )
        , ( 66, 60 )
        , ( 67, 57 )
        , ( 68, 25 )
        , ( 68, 11 )
        , ( 68, 24 )
        , ( 68, 27 )
        , ( 68, 48 )
        , ( 68, 41 )
        , ( 69, 25 )
        , ( 69, 68 )
        , ( 69, 11 )
        , ( 69, 24 )
        , ( 69, 27 )
        , ( 69, 48 )
        , ( 69, 41 )
        , ( 70, 25 )
        , ( 70, 69 )
        , ( 70, 68 )
        , ( 70, 11 )
        , ( 70, 24 )
        , ( 70, 27 )
        , ( 70, 41 )
        , ( 70, 58 )
        , ( 71, 27 )
        , ( 71, 69 )
        , ( 71, 68 )
        , ( 71, 70 )
        , ( 71, 11 )
        , ( 71, 48 )
        , ( 71, 41 )
        , ( 71, 25 )
        , ( 72, 26 )
        , ( 72, 27 )
        , ( 72, 11 )
        , ( 73, 48 )
        , ( 74, 48 )
        , ( 74, 73 )
        , ( 75, 69 )
        , ( 75, 68 )
        , ( 75, 25 )
        , ( 75, 48 )
        , ( 75, 41 )
        , ( 75, 70 )
        , ( 75, 71 )
        , ( 76, 64 )
        , ( 76, 65 )
        , ( 76, 66 )
        , ( 76, 63 )
        , ( 76, 62 )
        , ( 76, 48 )
        , ( 76, 58 )
        ]
