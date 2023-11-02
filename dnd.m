    (* A lot of the formulas should be rewritten as copying them from Notebook did result in Expressions that are very inefficient (were those really used in Notebook, like Times[n,Power[m,-1]] for n/m ?) *)

(* Wolfram Language Tools *)

    (* Formatting numbers *)
    fN[x_] := If[x == Round[x], Round[x], N[x]];
    fN[x_, digits : _Integer] :=  If[x == Round[x], Round[x], N[x, digits]];
    fN[x_, precision_] :=  If[Abs[x - Round[x]] <= precision, Round[x], N[x]];
    fN[x_, precision_, digits : Integer] := 
        If[Abs[x - Round[x]] <= precision, Round[x], N[x, digits]];


(* Stochastic Functions *)

    (* Probability to roll exactly a sum of p with n s-sided dice. *)
    diceP[p_, n_, s_] := Times[Power[s,Times[-1,n]],Sum[Times[Power[-1,k],Binomial[n,k],Binomial[Plus[-1,p,Times[-1,k,s]],Plus[Times[-1,n],p,Times[-1,k,s]]]],List[k,0,Floor[Times[Plus[Times[-1,n],p],Power[s,-1]]]]]];

    (* Probability to roll at least a sum of minSum with n s-sided dice. *)
    diceSumP[minSum_, n_, s_] := Sum[Times[Power[s,Times[-1,n]],Sum[Times[Power[-1,k],Binomial[n,k],Binomial[Plus[-1,i,Times[-1,k,s]],Plus[i,Times[-1,n],Times[-1,k,s]]]],List[k,0,Floor[Times[Plus[i,Times[-1,n]],Power[s,-1]]]]]],List[i,minSum,Times[n,s]]];

    (* Probability to get exactly k successes in n experiments/trials with individual Probability of success per experiment/trial of p. *)
    pKSuccessesInNTrials[k_, n_, p_] := Times[Power[Plus[1,Times[-1,p]],Plus[Times[-1,k],n]],Power[p,k],Binomial[n,k]];

    (* Probability to get at least k successes in n experiments/trials for a probability to succeed in any individual experiment/trial of p. *)
    pAtLeastKSuccessesInNTrials[k_, n_, p_] := Plus[1,Times[-1,Power[Plus[1,Times[-1,p]],n],Plus[Power[Power[Plus[1,Times[-1,p]],-1],n],Times[-1,Power[Plus[1,Times[-1,p]],Times[-1,k]],Power[p,k],Binomial[n,k],Hypergeometric2F1[1,Plus[k,Times[-1,n]],Plus[1,k],Times[Power[Plus[-1,p],-1],p]]]]]];

    (* Prepare the table of rolling 2d6 for fast access.
        diceSums=Table[{s,diceSumP[s,2,6]},{s,1,13}];
    *)

    (* rolling n dN-sided dice *)
    roll[n_, dN_] := Table[RandomInteger[{1, dN}], n];

    (* ? *)
    avgHighest[1, s_] := d[s];
    avgHighest[2, s_] := Times[Rational[1,6],Power[s,-1],Plus[1,s],Plus[-1,Times[4,s]]];
    avgHighest[3, s_] := Times[Rational[1,4],Power[s,-1],Plus[1,s],Plus[-1,Times[3,s]]];
    avgHighest[4, s_] := Times[Rational[1,30],Power[s,-3],Plus[1,s],Plus[1,Times[-1,s],Times[-9,Power[s,2]],Times[24,Power[s,3]]]];
    avgLowest[n_, s_] := (s + 1) - avgHighest[n, s];

    (* Probability to win a contested Check *)
    (* syntax error to be corrected here
    contestedCheck[challenger_, challenged_] := Total[Flatten[Table[If[Greater[Plus[a,challenger],Plus[b,challenged]],1,0],List[a,1,20],List[b,1,20]]]],Power[400,-1];
    *)

    (*probability of getting at least one k in n tries*)
    pAtLeastOneK[k_, n_] := Plus[1,Times[-1,Power[Plus[1,Times[Rational[1,20],Plus[-21,k]]],n]]];


(* Maths *)

    distance[a_, b_, c_] := Power[Plus[Power[a,2],Power[b,2],Power[c,2]],Rational[1,2]];
    distance[a_, b_] := distance[a, b, 0]; (* Slower than to inline *)

    (* Radius of the Sphere cut, cutting a sphere of radius r at a height h from the surface, measuring along the straight through the center. *)
    rSphereCut[r_, h_] := Power[Plus[Times[-1,Power[h,2]],Power[r,2]],Rational[1,2]];


(* Simulation Functions *)

    defaultResistances = <|physical -> False, 
   magical -> 
    False|> (* the simplification is that all physical are considered \
the same and that magical means they have against the char's main \
type of magical *);
defaultImmunities = <|physical -> False, 
   magical -> 
    False|> (* the simplification is that all physical are considered \
the same and that magical means they have against the char's main \
type of magical *);
defaultSave[level_] := proficiencyBonus[level];
defaultAC[level_] := Switch[level,
   1, 14,
   2, 14,
   3, 14,
   4, 15,
   5, 15,
   6, 15,
   7, 16,
   8, 16,
   9, 16,
   10, 17,
   11, 17,
   12, 17,
   13, 18,
   14, 18,
   15, 18,
   16, 19,
   17, 19,
   18, 19,
   19, 20,
   20, 20
   ];
conditions[ac0_, save0_, targets0_, resistances0_, immunities0_] := <|
   ac -> ac0, save -> save0, targets -> targets0, 
   resistances -> resistances0, immunities -> immunities0, 
   attackModifier -> "normal"|>;
conditions[ac0_, save0_, targets0_] := 
  conditions[ac0, save0, targets0, defaultResistances, 
   defaultImmunities];
conditions[ac0_, save0_] := conditions[ac0, save0, 1];
defaultConditions := 
  Table[{level, 
    conditions[defaultAC[level], defaultSave[level]]}, {level, 1, 
    20}];

    templateSequence[level_, conditions_] := {};


(* Game Mechanics *)

    (* Probability to succeed in a check with dc, but with nat 20 success and nat 1 failure, so bad default really, based on assuming the default to cover attacks being the most used case, which it is, but makes the code somewhat hard to read. Also adv is really modifier. It would also be more modern Wolfram Language style to provide Options instead of having to know the string literals. *)
    pSuccess[dc_] := Max[Rational[1,20],Min[Rational[19,20],Plus[Rational[1,20],Times[Rational[1,20],Plus[20,Times[-1,dc]]]]]];
    pSuccess[dc_, "lucky"] := pSuccess[dc, "advantage"];
    pSuccess[dc_, adv_] := If[Equal[adv,"advantage"],Plus[pSuccess[dc],Times[Plus[1,Times[-1,pSuccess[dc]]],pSuccess[dc]]],If[Equal[adv,"disadvantage"],Power[pSuccess[dc],2],pSuccess[dc]]];
    pSuccess[dc_, "superdisadvantage"] := pSuccess[dc, "disadvantage"] pSuccess[dc];
    pSuccess[dc_, adv_, "lucky"] := pSuccess[dc, adv] + (1 - pSuccess[dc, adv]) pSuccess[dc];
    pSuccess[dc_, "superadvantage"] := pSuccess[dc, "advantage", "lucky"];

    (* Average roll of a n-sided dice. *)
    d[n_] := Times[Rational[1,2],Plus[1,n]];

    (* Probability to succeed in a Check with DC, this one working for the real Check rules, so no auto-success on 20 or auto-failure on 1. *)
    pSuccessNoNats[dc_] := Max[0,Min[1,Plus[Rational[1,20],Times[Rational[1,20],Plus[20,Times[-1,dc]]]]]];
    pSuccessNoNats[dc_, "lucky"] := pSuccessNoNats[dc, "advantage"];
    pSuccessNoNats[dc_, adv_] := If[Equal[adv,"advantage"],Plus[pSuccessNoNats[dc],Times[Plus[1,Times[-1,pSuccessNoNats[dc]]],pSuccessNoNats[dc]]],If[Equal[adv,"disadvantage"],Power[pSuccessNoNats[dc],2],pSuccessNoNats[dc]]];
    pSuccessNoNats[dc_, "superdisadvantage"] := pSuccessNoNats[dc, "disadvantage"] pSuccessNoNats[dc];
    pSuccessNoNats[dc_, adv_, "lucky"] := pSuccessNoNats[dc, adv] + (1 - pSuccessNoNats[dc, adv]) pSuccessNoNats[dc];
    pSuccessNoNats[dc_, "superadvantage"] := pSuccessNoNats[dc, "advantage", "lucky"];

    (* Probability to succeed in a Roll against DC with nat-1 and nat-20 with buffs. I am not sure this is correct. Looks not obvious at all anymore. *)
    pSuccessBuffed[dc_, modifier_, 0] := pSuccess[dc, modifier];
    pSuccessBuffed[dc_, modifier_, d_] := Sum[pSuccess[dc-i,modifier]/d,{i,1,d}];
    pSuccessBuffedNoNats[dc_, modifier_, d_] := Sum[Times[Power[d,-1],pSuccessNoNats[Plus[dc,Times[-1,i]],modifier]],List[i,1,d]];
    pSuccessBuffedBuffed[dc_, modifier_, d1_, d2_] := Sum[Times[Power[d2,-1],pSuccessBuffed[Plus[dc,Times[-1,i]],modifier,d1]],List[i,1,d2]];
    pSuccessBuffedBuffedNoNats[dc_, modifier_, d1_, d2_] := Sum[Times[Power[d2,-1],pSuccessBuffedNoNats[Plus[dc,Times[-1,i]],modifier,d1]],List[i,1,d2]];

    (* Taking half on Success Saves damage. *)
    hDOS[damage_, saveDC_, conditions_] := Plus[Times[damage,Plus[1,Times[-1,Max[0,Min[1,Plus[Rational[1,20],Times[Rational[1,20],Plus[20,Times[-1,saveDC],conditions[save]]]]]]]]],Times[Rational[1,2],damage,Max[0,Min[1,Plus[Rational[1,20],Times[Rational[1,20],Plus[20,Times[-1,saveDC],conditions[save]]]]]]]];
    hDOS[damage_, saveDC_] := hDOS[damage, saveDC, defaultConditions[[1, 2]]];

    (*how much change in height can one move with movement of m moving xy horizontally?*)
    dZQ[xy_, m_] := Power[Plus[Power[m,2],Times[-1,Power[xy,2]]],Rational[1,2]];

    (* Radius of the Circle hitting the ground when a Cone AoE is directed straight down from some range/height *)
    radiusConeProjectionCircle[range_] := Times[Power[5,Rational[-1,2]],range];

    (* Probability to not roll any 20 with Portent in n days. *)
    pNo20[n_, level_] := Power[Rational[19,20],Times[n,If[Less[level,14],2,3]]];

    proficiencyBonus[level_] := Plus[1,Ceiling[Times[Rational[1,4],level]]];

    timeTravel[feet_] := Quantity[Times[Rational[1,300],feet],"Minutes"];
    timeTravelMiles[miles_, speed_] := Quantity[MixedMagnitude[List[Times[miles,Power[Switch[speed,"slow",2,"normal",3,"fast",4],-1]],0]],MixedUnit[List["Hours","Minutes"]]];
    timeSpecialTravelPaceTravel[miles0_, movement0_, speed0_] := (*speed [slow,normal,fast]*)
        Quantity[MixedMagnitude[List[IntegerPart[Times[30,miles0,Power[movement0,-1],Power[If[Equal[speed0,"slow"],2,If[Equal[speed0,"fast"],4,3]],-1]]],Times[60,FractionalPart[Times[30,miles0,Power[movement0,-1],Power[If[Equal[speed0,"slow"],2,If[Equal[speed0,"fast"],4,3]],-1]]]]]],MixedUnit[List["Hours","Minutes"]]];

    (* aka ability bonus *)
    abilityModifier[score_] := Floor[Times[Rational[1,2],Plus[-10,score]]];


(* Specific Rules *)

    auraOfVitality[lifeCleric_] := 2 d[6] + If[lifeCleric, 5, 0]; (* lifeCleric [True | False] *)
	prayerOfHealing[level_, spellModifier_, lifeCleric_] := level d[8] + spellModifier + If[lifeCleric, level+2]; (* lifeCleric [True | False] *)
    
    (* Metamagic Extended Duration *)
    auraOfVitalitySum[lifeCleric_, metamagic_] := 10 auraOfVitality[lifeCleric] If[metamagic, 2, 1];

    avgGreatWeaponFighting[d_] := Mean[(If[#[[1]] < 3, #[[2]], #[[1]]]) & /@ Flatten[Table[{a, b}, {a, 1, d}, {b, 1, d}], 1]];

	bigbysHand[level_] := 4d[8]+If[level>5,(level-5) 2d[8],0];

    spiritShroudDamage[level_] := If[Less[level,3],0,Times[Plus[Floor[Times[Plus[level,-3],Power[2,-1]]],1],d[8]]];

    damageEldritchBlast[level_, charBonus_, hex_, spiritShroud_] := Times[Plus[1,Floor[Times[Rational[1,6],Plus[1,level]]]],Plus[Times[hex,d[6]],d[10],If[Greater[level,1],charBonus,0],Times[spiritShroud,spiritShroudDamage[If[Less[level,5],0,If[Less[level,9],3,5]]]]]]; (*charBonus=0 for not having AgonizingBlast*)

    eldritchBlastBeams[level_] := If[level < 5, 1, If[level < 11, 2, If[level < 17, 3, 4]]];

    cureWounds[level_, scaMod_, lifeCleric_, beaconOfHope_] := level If[beaconOfHope == 1, 8, d[8]] + scaMod + (lifeCleric (level + 2));

    cureWounds[level_, scaMod_] := cureWounds[level, scaMod, 0, 0];

    healingWord[level_, scaMod_, lifeCleric_, beaconOfHope_] := level If[beaconOfHope == 1, 4, d[4]] + scaMod + (lifeCleric (level + 2));

    preserveLife[classLevel_] := If[classLevel >= 2, 5 classLevel, 0];

    heal[lifeCleric_] := 70 + (lifeCleric (6 + 2));

    regenerate[lifeCleric_, turns_, beaconOfHope_] := If[beaconOfHope == 1, 4 8 + 15, 
    4 d[8] + 15] + (lifeCleric (7 + 2)) + turns (1 + lifeCleric (7 + 2));

    rageDamage[level_] := If[level <= 8, 2, If[level <= 15, 3, 4]];

    (* Barbarian baseline damage. Use [0|1] for berserker. *)
    barbarianDmg[level_, berserker_, gwm_] :=(*enter level gwm becomes available, 0 if never*)
        Times[If[And[Less[level,2],Equal[gwm,1]],With[List[Set[ac,14]],fN[Times[pSuccess[Plus[ac,-5,5]],Power[pSuccess[Plus[ac,-5]],-1]]]],1],Plus[If[GreaterEqual[level,3],berserker,0],If[Less[level,5],1,2]],Plus[d[12],If[Equal[gwm,0],0,If[GreaterEqual[level,gwm],10,0]],If[Less[level,4],3,If[Less[level,9],4,5]],rageDamage[level]]];

    sneakAttack[level_] := Times[Ceiling[Times[Rational[1,2],level]],d[6]];

    magicMissileDamage[level_, intBonus_, empowered_] := (level + 2) (d[4] + 1 + empowered intBonus);

    scorchingRay[level_] := If[level < 2, 0, (level + 1) (2 d[6])];

    jimsMagicMissile[level_, spiritShroud_] := (2 + level) (2 d[4] + spiritShroud d[8]);

    scorchingRayHex[level_] := If[level < 2, 0, (level + 1) (2 d[6] + d[6])];

    scorchingRay[level_, spiritShroud_] := If[level < 2, 0, (level + 1) (2 d[6] + spiritShroud d[8])];

    boomingBlade[characterLevel_] := If[characterLevel < 5, 0, If[characterLevel < 11, 1, If[characterLevel < 17, 2, 3]]] d[8];

    shadowBlade[level_] := If[level < 3, 2 d[8], If[level < 5, 3 d[8], If[level < 7, 4 d[8], 5 d[8]]]];

    summonFey[level_] := If[Less[level,3],0,Times[Floor[Times[level,Power[2,-1]]],Plus[d[6],3,d[6]]]];

    animateObjects = 65;

    spiritGuardians[level_] := If[level < 3, 0, level d[8]];

    spiritGuardians[level_, dc_, modifier_] := Plus[Times[Plus[1,Times[-1,pSuccessNoNats[dc,modifier]]],spiritGuardians[level]],Times[Rational[1,2],pSuccessNoNats[dc,modifier],spiritGuardians[level]]];

    thornWhip[characterLevel_] := If[characterLevel < 5, 1, If[characterLevel < 11, 2, If[characterLevel < 17, 3, 4]]] d[6];

    firebolt[characterLevel_] := If[characterLevel < 5, 1, If[characterLevel < 11, 2, If[characterLevel < 17, 3, 4]]] d[10];

    spiritualWeapon[level_, scaMod_] := If[Less[level,2],0,Plus[Times[Floor[Times[level,Power[2,-1]]],d[8]],scaMod]];

    dragonsBreath[level_] := If[level < 2, 0, (level + 1) d[6]];

    hitPoints[level_, dice_, conBonus_] := Plus[dice,Times[Plus[1,Times[Rational[1,2],dice]],Plus[-1,level]],Times[conBonus,level]]; (*d[6] is just 6 as the argument here*)

    divineSmite[level_, undeadOrFiend_] := (Min[(level + 1) , 5] + If[undeadOrFiend, 1, 0]) d[8];

    divineSmite[level_] := divineSmite[level, False];

    rage[level_] := If[level < 9, 2, If[level < 16, 3, 4]];

    carryCapacity[strength_, size_, variant_] := Times[strength,If[variant,5,15],Switch[size,"tiny",Power[2,-1],"small",Power[2,0],"medium",Power[2,0],"large",Power[2,1],"huge",Power[2,2],"gargantuan",Power[2,3]]];

    drawCapacity[strength_, size_, variant_] := 5 carryCapacity[strength, size, variant];

    jumpLong[str_, approachRun_] := Times[str,If[approachRun,1,Times[1,Power[2,-1]]]];
    jumpLong[str_] := jumpLong[str, True];
    jumpHigh[str_, approachRun_, height_] := Times[Plus[3,Times[Rational[3,2],height],abilityModifier[str]],If[approachRun,1,Times[1,Power[2,-1]]]];
    jumpHigh[str_, approachRun_] := Times[Plus[3,abilityModifier[str]],If[approachRun,1,Times[1,Power[2,-1]]]];
    jumpHigh[str_] := jumpHigh[str, True];

	summonDraconicSpirit[level_] := Floor[level/2](d[6]+4+level) + 2d[6];

    wage[level_] := Piecewise[{{1, level <= 4}, {5, level <= 10}, {20, level <= 16}}, 50] level;


(* Unit Testing *)

    runTests[doRun_] := With[{runTests = doRun},
    If[runTests, testReport = TestReport[{
       VerificationTest[pSuccess[1], 19/20, TestID -> 1],
       VerificationTest[pSuccess[20], 1/20, TestID -> 2],
       VerificationTest[pSuccess[21], 1/20, TestID -> 3],
       VerificationTest[pSuccessNoNats[1], 1, TestID -> 4],
       VerificationTest[pSuccessNoNats[21], 0, TestID -> 5],
       VerificationTest[pSuccess[20, "advantage"], 1 - (19/20)^2, 
        TestID -> 6],
       VerificationTest[pSuccess[20, "superadvantage"], 1 - (19/20)^3,
         TestID -> 7],
       VerificationTest[pSuccess[30, "disadvantage"], 1/20^2, 
        TestID -> 8],
       VerificationTest[pSuccess[30, "superdisadvantage"], 1/20^3, 
        TestID -> 9],
       VerificationTest[hDOS[1, 23, defaultConditions[[1, 2]]], 1, 
        TestID -> 10],
       VerificationTest[hDOS[1, 22, defaultConditions[[1, 2]]], 
        19/20 + 1/(20 2), TestID -> 11],
       VerificationTest[hDOS[1, 3, defaultConditions[[1, 2]]], 1/2, 
        TestID -> 12],
       VerificationTest[pSuccessBuffedNoNats[24, "normal", 4], 1/80, 
        TestID -> 13],
       VerificationTest[pSuccessBuffedNoNats[25, "normal", 4], 0, 
        TestID -> 14],
       VerificationTest[pSuccessBuffed[25, "normal", 4], 1/20, 
        TestID -> 15]
       }], Nothing]
   ];


(* Help *)

    help[]:=With[{version="version 0.0.1"},
        Print[version];
        Print["help[] dispalys this help."];
        Print["help[\"pSuccess\"] displays the help for the pSuccess function."];
        Print["runTests[True][\"AllTestsSucceeded\"] runs unit tests."];
        Print["Some functions are"];
        Print[" pSuccess, d, roll"];
    ];
    help["pSuccess"]:=With[{},
        Print["pSuccess[eDC,modifier]"];
        Print["  - The effective eDC is the DC modified by the sum of boni/mali."];
        Print["  - modifier is [\"normal\"|\"advantage\"|\"disadvantage\"]"];
        Print["  > pSuccess[15+2-8,\"advantage\"]"];
        Print[""];
        Print["  + You might be looking for pSuccessNoNats[eDC,modifier]."];
    ];
    help["d"]:=With[{},
        Print["d[n]"];
        Print["  - The average roll with a n-sided fair dice."];
        Print[""];
        Print["  + You might be looking for roll[n,dN]."];
    ];
    help["roll"]:=With[{},
        Print["roll[n,dN]"];
        Print["  - Roll n dN-sided fair dice."];
        Print[""];
        Print["  + You might be looking for d[n]."];
    ];
    help[]

